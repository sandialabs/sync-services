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

        return GetNormalizedAsync(indexedPath, arguments, cancellationToken);
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
        if (TryBuildSetBatchArguments(request, out var setBatchArguments))
        {
            return CallFunctionAsync("set-batch!", setBatchArguments, requiresAuth: true, cancellationToken);
        }

        var queries = new JsonArray();
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

            if (_authToken is not null && OperationRequiresAuth(operation.Function))
            {
                item["authentication"] = new JsonObject
                {
                    ["*type/string*"] = _authToken,
                };
            }

            queries.Add(item);
        }

        var arguments = new JsonObject
        {
            ["queries"] = queries,
        };

        return BatchNormalizedAsync(request, arguments, cancellationToken);
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
        var result = await CallFunctionAsync(
            "config",
            new JsonObject
            {
                ["path"] = JsonSerializer.SerializeToNode(new object[] { "private", "bridge" }),
            },
            requiresAuth: true,
            cancellationToken);
        return HttpGatewayClient.ExtractBridgeNames(result);
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

    private async Task<JsonNode?> GetNormalizedAsync(bool indexedPath, JsonObject arguments, CancellationToken cancellationToken)
    {
        var result = await CallFunctionAsync(indexedPath ? "resolve" : "get", arguments, requiresAuth: true, cancellationToken);
        if (indexedPath)
        {
            return result;
        }

        var content = result?.DeepClone();
        return new JsonObject
        {
            ["content"] = content,
            ["pinned?"] = false,
        };
    }

    private async Task<JsonNode?> BatchNormalizedAsync(GatewayBatchRequest request, JsonObject arguments, CancellationToken cancellationToken)
    {
        var result = await CallFunctionAsync("batch!", arguments, requiresAuth: false, cancellationToken);
        if (result is not JsonArray array)
        {
            return result;
        }

        var normalized = new JsonArray();
        for (var i = 0; i < request.Requests.Count; i++)
        {
            var operation = request.Requests[i];
            var item = i < array.Count ? array[i] : null;
            normalized.Add(NormalizeBatchResult(operation, item)?.DeepClone());
        }

        return normalized;
    }

    private static bool TryBuildSetBatchArguments(GatewayBatchRequest request, out JsonObject arguments)
    {
        var paths = new JsonArray();
        var values = new JsonArray();

        foreach (var operation in request.Requests)
        {
            if (!string.Equals(operation.Function, "set!", StringComparison.Ordinal) || operation.Arguments is null)
            {
                arguments = null!;
                return false;
            }

            var path = operation.Arguments["path"];
            var value = operation.Arguments["value"];
            if (path is null || value is null)
            {
                arguments = null!;
                return false;
            }

            paths.Add(path.DeepClone());
            values.Add(value.DeepClone());
        }

        arguments = new JsonObject
        {
            ["paths"] = paths,
            ["values"] = values,
        };
        return true;
    }

    private static JsonNode? NormalizeBatchResult(GatewayBatchOperation operation, JsonNode? item)
    {
        if (!string.Equals(operation.Function, "get", StringComparison.Ordinal))
        {
            return item?.DeepClone();
        }

        return new JsonObject
        {
            ["content"] = item?.DeepClone(),
            ["pinned?"] = false,
        };
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

    private static bool IsIndexedPath(IReadOnlyList<object> path) =>
        path.Count > 0 && path[0] is sbyte or byte or short or ushort or int or uint or long or ulong;

    private static bool OperationRequiresAuth(string functionName) =>
        string.Equals(functionName, "set!", StringComparison.Ordinal) ||
        string.Equals(functionName, "set-batch!", StringComparison.Ordinal) ||
        string.Equals(functionName, "pin!", StringComparison.Ordinal) ||
        string.Equals(functionName, "unpin!", StringComparison.Ordinal) ||
        string.Equals(functionName, "bridge!", StringComparison.Ordinal) ||
        string.Equals(functionName, "bridge-synchronize!", StringComparison.Ordinal) ||
        string.Equals(functionName, "*step*", StringComparison.Ordinal) ||
        string.Equals(functionName, "*secret*", StringComparison.Ordinal);
}
