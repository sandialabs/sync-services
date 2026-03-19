using System.Net;
using System.Net.Http.Headers;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace FileSystem.Server;

public sealed class HttpJournalClient : IGeneralInterfaceClient, IDisposable
{
    private readonly HttpClient _httpClient;
    private readonly bool _ownsHttpClient;
    private readonly string? _authToken;

    public HttpJournalClient(ServerOptions options)
        : this(CreateHttpClient(options), options.GatewayAuthToken, ownsHttpClient: true)
    {
    }

    internal HttpJournalClient(HttpClient httpClient, string? authToken, bool ownsHttpClient = false)
    {
        _httpClient = httpClient;
        _authToken = string.IsNullOrWhiteSpace(authToken) ? null : authToken;
        _ownsHttpClient = ownsHttpClient;
    }

    public Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
            ["pinned?"] = request.Pinned,
            ["proof?"] = request.Proof,
        };

        return CallFunctionAsync("get", arguments, requiresAuth: true, cancellationToken);
    }

    public Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
            ["value"] = request.Content.DeepClone(),
        };

        return CallFunctionAsync("set!", arguments, requiresAuth: true, cancellationToken);
    }

    public Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken)
    {
        var requests = new JsonArray();
        foreach (var operation in request.Requests)
        {
            var item = new JsonObject
            {
                ["function"] = operation.Function,
            };

            if (operation.Arguments is not null)
            {
                item["arguments"] = operation.Arguments.DeepClone();
            }

            requests.Add(item);
        }

        var arguments = new JsonObject
        {
            ["requests"] = requests,
        };

        return CallFunctionAsync("general-batch!", arguments, requiresAuth: true, cancellationToken);
    }

    public async Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
        };

        var result = await CallFunctionAsync("pin!", arguments, requiresAuth: true, cancellationToken);
        return result is JsonValue value && value.TryGetValue<bool>(out var pinned) ? pinned : true;
    }

    public async Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
        };

        var result = await CallFunctionAsync("unpin!", arguments, requiresAuth: true, cancellationToken);
        return result is JsonValue value && value.TryGetValue<bool>(out var pinned) ? pinned : true;
    }

    public async Task<long> SizeAsync(CancellationToken cancellationToken)
    {
        var result = await CallFunctionAsync("size", null, requiresAuth: false, cancellationToken);
        return result switch
        {
            JsonValue value when value.TryGetValue<long>(out var size) => size,
            _ => throw new InvalidDataException("Journal size response must be a number.")
        };
    }

    public async Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken)
    {
        var result = await CallFunctionAsync("bridges", null, requiresAuth: true, cancellationToken);
        if (result is null)
        {
            return Array.Empty<string>();
        }

        if (result is not JsonArray array)
        {
            throw new InvalidDataException("Journal bridges response must be an array.");
        }

        return array
            .Select(node => node?.GetValue<string>() ?? string.Empty)
            .Where(name => !string.IsNullOrWhiteSpace(name))
            .ToArray();
    }

    public void Dispose()
    {
        if (_ownsHttpClient)
        {
            _httpClient.Dispose();
        }
    }

    private async Task<JsonNode?> CallFunctionAsync(string functionName, JsonObject? arguments, bool requiresAuth, CancellationToken cancellationToken)
    {
        var body = new JsonObject
        {
            ["function"] = functionName,
        };

        if (arguments is not null)
        {
            body["arguments"] = arguments;
        }

        if (requiresAuth)
        {
            if (_authToken is null)
            {
                throw new InvalidOperationException($"Journal function '{functionName}' requires an auth token.");
            }

            body["authentication"] = new JsonObject
            {
                ["*type/string*"] = _authToken,
            };
        }

        using var request = new HttpRequestMessage(HttpMethod.Post, string.Empty);
        request.Content = new StringContent(body.ToJsonString(), Encoding.UTF8, "application/json");
        request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

        using var response = await _httpClient.SendAsync(request, cancellationToken);
        var content = await response.Content.ReadAsStringAsync(cancellationToken);
        var parsed = TryParseJson(content);

        ThrowSemanticErrorIfPresent(parsed, response.StatusCode);

        if (!response.IsSuccessStatusCode)
        {
            throw new HttpRequestException(
                $"Journal request failed with {(int)response.StatusCode} {response.StatusCode}.",
                null,
                response.StatusCode);
        }

        return parsed;
    }

    private static void ThrowSemanticErrorIfPresent(JsonNode? body, HttpStatusCode statusCode)
    {
        if (body is not JsonArray array || array.Count < 3)
        {
            return;
        }

        if (!string.Equals(array[0]?.GetValue<string>(), "error", StringComparison.Ordinal))
        {
            return;
        }

        var code = array[1]?["*type/quoted*"]?.GetValue<string>() ?? "journal-error";
        var message = array[2]?["*type/string*"]?.GetValue<string>() ?? "Journal returned an error";
        throw new GatewaySemanticException(code, message, body.DeepClone(), statusCode);
    }

    private static JsonNode? TryParseJson(string content)
    {
        if (string.IsNullOrWhiteSpace(content))
        {
            return null;
        }

        try
        {
            return JsonNode.Parse(content);
        }
        catch
        {
            return JsonValue.Create(content);
        }
    }

    private static HttpClient CreateHttpClient(ServerOptions options)
    {
        return new HttpClient
        {
            BaseAddress = new Uri(options.JournalJsonUrl, UriKind.Absolute),
            Timeout = TimeSpan.FromMilliseconds(options.GatewayTimeoutMs),
        };
    }
}
