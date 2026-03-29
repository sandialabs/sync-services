using System.Net;
using System.Net.Http.Headers;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace FileSystem.Server;

public sealed class GatewaySemanticException : Exception
{
    public GatewaySemanticException(string code, string message, JsonNode? details, HttpStatusCode statusCode)
        : base(message)
    {
        Code = code;
        Details = details;
        StatusCode = statusCode;
    }

    public string Code { get; }

    public JsonNode? Details { get; }

    public HttpStatusCode StatusCode { get; }
}

public sealed class HttpGatewayClient : IGeneralInterfaceClient, IDisposable
{
    private readonly HttpClient _httpClient;
    private readonly bool _ownsHttpClient;
    private readonly string? _authToken;

    public HttpGatewayClient(ServerOptions options)
        : this(CreateHttpClient(options), options.GatewayAuthToken, ownsHttpClient: true)
    {
    }

    internal HttpGatewayClient(HttpClient httpClient, string? authToken, bool ownsHttpClient = false)
    {
        _httpClient = httpClient;
        _authToken = string.IsNullOrWhiteSpace(authToken) ? null : authToken;
        _ownsHttpClient = ownsHttpClient;
    }

    public async Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken)
    {
        var indexedPath = IsIndexedPath(request.Path);
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
        };

        if (indexedPath)
        {
            arguments["pinned?"] = request.Pinned;
            arguments["proof?"] = request.Proof;
        }

        return await PostGeneralAsync(indexedPath ? "resolve" : "get", arguments, cancellationToken);
    }

    public async Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
            ["value"] = request.Content.DeepClone(),
        };

        return await PostGeneralAsync("set", arguments, cancellationToken);
    }

    public async Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken)
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

        return await PostGeneralAsync("batch", arguments, cancellationToken);
    }

    public async Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
        };

        var result = await PostGeneralAsync("pin", arguments, cancellationToken);
        return result is JsonValue value && value.TryGetValue<bool>(out var pinned) ? pinned : true;
    }

    public async Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(request.Path),
        };

        var result = await PostGeneralAsync("unpin", arguments, cancellationToken);
        return result is JsonValue value && value.TryGetValue<bool>(out var pinned) ? pinned : true;
    }

    public async Task<long> SizeAsync(CancellationToken cancellationToken)
    {
        using var request = new HttpRequestMessage(HttpMethod.Get, "general/size");
        using var response = await _httpClient.SendAsync(request, cancellationToken);
        var content = await response.Content.ReadAsStringAsync(cancellationToken);
        var body = TryParseJson(content);

        if (!response.IsSuccessStatusCode)
        {
            ThrowSemanticErrorIfPresent(body, response.StatusCode);
            throw new HttpRequestException(
                $"Gateway request failed with {(int)response.StatusCode} {response.StatusCode}.",
                null,
                response.StatusCode);
        }

        return body switch
        {
            JsonValue value when value.TryGetValue<long>(out var size) => size,
            _ => throw new InvalidDataException("Gateway size response must be a number.")
        };
    }

    public async Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken)
    {
        var result = await PostGeneralAsync(
            "config",
            new JsonObject
            {
                ["path"] = JsonSerializer.SerializeToNode(new object[] { "private", "bridge" }),
            },
            cancellationToken);
        return ExtractBridgeNames(result);
    }

    public void Dispose()
    {
        if (_ownsHttpClient)
        {
            _httpClient.Dispose();
        }
    }

    private async Task<JsonNode?> PostGeneralAsync(string operation, JsonObject arguments, CancellationToken cancellationToken)
    {
        using var request = new HttpRequestMessage(HttpMethod.Post, $"general/{operation}");
        request.Content = new StringContent(
            arguments.ToJsonString(),
            Encoding.UTF8,
            "application/json");

        if (_authToken != null)
        {
            request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", _authToken);
        }

        using var response = await _httpClient.SendAsync(request, cancellationToken);
        var content = await response.Content.ReadAsStringAsync(cancellationToken);
        var body = TryParseJson(content);

        if (!response.IsSuccessStatusCode)
        {
            ThrowSemanticErrorIfPresent(body, response.StatusCode);
            throw new HttpRequestException(
                $"Gateway request failed with {(int)response.StatusCode} {response.StatusCode}.",
                null,
                response.StatusCode);
        }

        ThrowSemanticErrorIfPresent(body, response.StatusCode);
        return body;
    }

    private static void ThrowSemanticErrorIfPresent(JsonNode? body, HttpStatusCode statusCode)
    {
        if (body is not JsonObject obj)
        {
            return;
        }

        var code = obj["error"]?.GetValue<string>();
        var message = obj["message"]?.GetValue<string>();
        var source = obj["source"]?.GetValue<string>();
        if (string.IsNullOrWhiteSpace(code) || string.IsNullOrWhiteSpace(message) || source != "journal")
        {
            return;
        }

        throw new GatewaySemanticException(code, message, obj["details"]?.DeepClone(), statusCode);
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
        var baseUrl = options.GatewayBaseUrl.TrimEnd('/') + "/";
        return new HttpClient
        {
            BaseAddress = new Uri(baseUrl, UriKind.Absolute),
            Timeout = TimeSpan.FromMilliseconds(options.GatewayTimeoutMs),
        };
    }

    private static bool IsIndexedPath(IReadOnlyList<object> path) =>
        path.Count > 0 && path[0] is sbyte or byte or short or ushort or int or uint or long or ulong;

    internal static IReadOnlyList<string> ExtractBridgeNames(JsonNode? body)
    {
        var bridgeBlock = body?["private"]?["bridge"];
        if (bridgeBlock is not JsonObject obj)
        {
            return Array.Empty<string>();
        }

        return obj.Select(pair => pair.Key)
            .Where(name => !string.IsNullOrWhiteSpace(name))
            .OrderBy(name => name, StringComparer.Ordinal)
            .ToArray();
    }
}
