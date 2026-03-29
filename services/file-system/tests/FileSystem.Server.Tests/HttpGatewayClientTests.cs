using System.Net;
using System.Text;
using System.Text.Json.Nodes;
using FileSystem.Server;
using Xunit;

namespace FileSystem.Server.Tests;

public sealed class HttpGatewayClientTests
{
    [Fact]
    public async Task GetAsync_SendsDirectArgumentObjectAndBearerToken()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://gateway/api/v1/", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var gateway = new HttpGatewayClient(httpClient, "secret-token");

        var result = await gateway.GetAsync(
            new GatewayGetRequest(
                new object[] { new object[] { "*state*", "docs" } },
                true,
                false),
            CancellationToken.None);

        Assert.Equal("http://gateway/api/v1/general/get", handler.LastRequestUri);
        Assert.Equal("Bearer secret-token", handler.LastAuthorization);
        Assert.Equal("""[["*state*","docs"]]""", handler.LastBody?["path"]?.ToJsonString());
        Assert.Null(handler.LastBody?["pinned?"]);
        Assert.Null(handler.LastBody?["proof?"]);
        Assert.Equal("get", result?["ok"]?.GetValue<string>());
    }

    [Fact]
    public async Task PinAndUnpinAsync_SendJournalPathToDedicatedRoutes()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://gateway/api/v1/", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var gateway = new HttpGatewayClient(httpClient, "secret-token");

        var path = new object[] { -1, new object[] { "*state*", "notes", "todo.txt" } };

        var pinResult = await gateway.PinAsync(new GatewayPinRequest(path), CancellationToken.None);
        Assert.True(pinResult);
        Assert.Equal("http://gateway/api/v1/general/pin", handler.LastRequestUri);
        Assert.Equal("""[-1,["*state*","notes","todo.txt"]]""", handler.LastBody?["path"]?.ToJsonString());

        var unpinResult = await gateway.UnpinAsync(new GatewayPinRequest(path), CancellationToken.None);
        Assert.True(unpinResult);
        Assert.Equal("http://gateway/api/v1/general/unpin", handler.LastRequestUri);
        Assert.Equal("""[-1,["*state*","notes","todo.txt"]]""", handler.LastBody?["path"]?.ToJsonString());
    }

    [Fact]
    public async Task BatchAsync_SendsGeneralBatchRequestShape()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://gateway/api/v1/", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var gateway = new HttpGatewayClient(httpClient, "secret-token");

        var request = new GatewayBatchRequest(
            new[]
            {
                new GatewayBatchOperation(
                    "set!",
                    new JsonObject
                    {
                        ["path"] = JsonNode.Parse("""[["*state*","docs","a.txt"]]"""),
                        ["value"] = JsonNode.Parse("""{"*type/string*":"a"}"""),
                    }),
                new GatewayBatchOperation(
                    "config",
                    null),
            });

        var result = await gateway.BatchAsync(request, CancellationToken.None);

        Assert.Equal("http://gateway/api/v1/general/batch", handler.LastRequestUri);
        Assert.Equal("Bearer secret-token", handler.LastAuthorization);
        Assert.Equal("set!", handler.LastBody?["queries"]?[0]?["function"]?.GetValue<string>());
        Assert.Equal("""[["*state*","docs","a.txt"]]""", handler.LastBody?["queries"]?[0]?["arguments"]?["path"]?.ToJsonString());
        Assert.Equal("""{"*type/string*":"a"}""", handler.LastBody?["queries"]?[0]?["arguments"]?["value"]?.ToJsonString());
        Assert.Equal("config", handler.LastBody?["queries"]?[1]?["function"]?.GetValue<string>());
        Assert.Null(handler.LastBody?["queries"]?[1]?["arguments"]);
        Assert.Equal("batch", result?[0]?["ok"]?.GetValue<string>());
    }

    [Fact]
    public async Task BridgesAsync_TreatsEmptyConfigurationAsEmptyBridgeList()
    {
        var handler = new RecordingHttpMessageHandler
        {
            NextResponseBody = JsonValue.Create(string.Empty),
            NextRespondWithEmptyBody = true,
        };
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://gateway/api/v1/", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var gateway = new HttpGatewayClient(httpClient, null);

        var bridges = await gateway.BridgesAsync(CancellationToken.None);

        Assert.Empty(bridges);
        Assert.Equal("http://gateway/api/v1/general/config", handler.LastRequestUri);
        Assert.Equal("""["private","bridge"]""", handler.LastBody?["path"]?.ToJsonString());
    }

    [Fact]
    public async Task JournalClient_GetAsync_SendsDirectFunctionEnvelope()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://journal/interface/json", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var journal = new HttpJournalClient(httpClient, "secret-token");

        var result = await journal.GetAsync(
            new GatewayGetRequest(
                new object[] { new object[] { "*state*", "docs" } },
                true,
                false),
            CancellationToken.None);

        Assert.Equal("http://journal/interface/json", handler.LastRequestUri);
        Assert.Null(handler.LastAuthorization);
        Assert.Equal("get", handler.LastBody?["function"]?.GetValue<string>());
        Assert.Equal("secret-token", handler.LastBody?["authentication"]?["*type/string*"]?.GetValue<string>());
        Assert.Equal("""[["*state*","docs"]]""", handler.LastBody?["arguments"]?["path"]?.ToJsonString());
        Assert.Null(handler.LastBody?["arguments"]?["pinned?"]);
        Assert.Null(handler.LastBody?["arguments"]?["proof?"]);
        Assert.Equal("get", result?["content"]?["ok"]?.GetValue<string>());
        Assert.Equal(false, result?["pinned?"]?.GetValue<bool>());
    }

    [Fact]
    public async Task JournalClient_BatchAsync_SendsDirectGeneralBatchEnvelope()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://journal/interface/json", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var journal = new HttpJournalClient(httpClient, "secret-token");

        var result = await journal.BatchAsync(
            new GatewayBatchRequest(
                new[]
                {
                    new GatewayBatchOperation(
                        "set!",
                        new JsonObject
                        {
                            ["path"] = JsonNode.Parse("""[["*state*","docs","a.txt"]]"""),
                            ["value"] = JsonNode.Parse("""{"*type/string*":"a"}"""),
                        }),
                    new GatewayBatchOperation("config", null),
                }),
            CancellationToken.None);

        Assert.Equal("http://journal/interface/json", handler.LastRequestUri);
        Assert.Equal("batch!", handler.LastBody?["function"]?.GetValue<string>());
        Assert.Null(handler.LastBody?["authentication"]);
        Assert.Equal("set!", handler.LastBody?["arguments"]?["queries"]?[0]?["function"]?.GetValue<string>());
        Assert.Equal("secret-token", handler.LastBody?["arguments"]?["queries"]?[0]?["authentication"]?["*type/string*"]?.GetValue<string>());
        Assert.Equal("config", handler.LastBody?["arguments"]?["queries"]?[1]?["function"]?.GetValue<string>());
        Assert.Null(handler.LastBody?["arguments"]?["queries"]?[1]?["authentication"]);
        Assert.Equal("batch", result?[0]?["ok"]?.GetValue<string>());
    }

    [Fact]
    public async Task JournalClient_BatchAsync_CollapsesPureSetWritesIntoSetBatch()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://journal/interface/json", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var journal = new HttpJournalClient(httpClient, "secret-token");

        await journal.BatchAsync(
            new GatewayBatchRequest(
                new[]
                {
                    new GatewayBatchOperation(
                        "set!",
                        new JsonObject
                        {
                            ["path"] = JsonNode.Parse("""[["*state*","docs","a.txt"]]"""),
                            ["value"] = JsonNode.Parse("""{"*type/string*":"a"}"""),
                        }),
                    new GatewayBatchOperation(
                        "set!",
                        new JsonObject
                        {
                            ["path"] = JsonNode.Parse("""[["*state*","docs","b.txt"]]"""),
                            ["value"] = JsonNode.Parse("""{"*type/string*":"b"}"""),
                        }),
                }),
            CancellationToken.None);

        Assert.Equal("http://journal/interface/json", handler.LastRequestUri);
        Assert.Equal("set-batch!", handler.LastBody?["function"]?.GetValue<string>());
        Assert.Equal("secret-token", handler.LastBody?["authentication"]?["*type/string*"]?.GetValue<string>());
        Assert.Equal("""[[["*state*","docs","a.txt"]],[["*state*","docs","b.txt"]]]""", handler.LastBody?["arguments"]?["paths"]?.ToJsonString());
        Assert.Equal("""[{"*type/string*":"a"},{"*type/string*":"b"}]""", handler.LastBody?["arguments"]?["values"]?.ToJsonString());
    }

    private sealed class RecordingHttpMessageHandler : HttpMessageHandler
    {
        public string? LastRequestUri { get; private set; }

        public string? LastAuthorization { get; private set; }

        public JsonNode? LastBody { get; private set; }

        public HttpStatusCode NextStatusCode { get; set; } = HttpStatusCode.OK;

        public JsonNode? NextResponseBody { get; set; } = JsonNode.Parse("""{"ok":"get"}""");

        public bool NextRespondWithEmptyBody { get; set; }

        protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
        {
            LastRequestUri = request.RequestUri?.ToString();
            LastAuthorization = request.Headers.Authorization?.ToString();

            if (request.Content != null)
            {
                var bodyText = await request.Content.ReadAsStringAsync(cancellationToken);
                LastBody = string.IsNullOrWhiteSpace(bodyText) ? null : JsonNode.Parse(bodyText);
            }
            else
            {
                LastBody = null;
            }

            var responseBody = NextRespondWithEmptyBody
                ? string.Empty
                : (NextResponseBody ?? JsonNode.Parse("""{"ok":"get"}""")!).ToJsonString();

            NextRespondWithEmptyBody = false;
            NextResponseBody = JsonNode.Parse("""{"ok":"get"}""");

            if (request.RequestUri?.AbsolutePath.EndsWith("/general/set", StringComparison.Ordinal) == true)
            {
                responseBody = """{"ok":"set"}""";
            }
            else if (request.RequestUri?.AbsolutePath.EndsWith("/general/batch", StringComparison.Ordinal) == true)
            {
                responseBody = """[{"ok":"batch"}]""";
            }
            else if (request.RequestUri?.AbsolutePath.EndsWith("/interface/json", StringComparison.Ordinal) == true)
            {
                var functionName = LastBody?["function"]?.GetValue<string>();
                responseBody = functionName switch
                {
                    "batch!" => """[{"ok":"batch"}]""",
                    "size" => "10",
                    "config" => """{"private":{"bridge":{"alice":{},"bob":{}}}}""",
                    "pin!" or "unpin!" => "true",
                    "set!" => """{"ok":"set"}""",
                    _ => """{"ok":"get"}""",
                };
            }
            else if (request.RequestUri?.AbsolutePath.EndsWith("/general/pin", StringComparison.Ordinal) == true ||
                     request.RequestUri?.AbsolutePath.EndsWith("/general/unpin", StringComparison.Ordinal) == true)
            {
                responseBody = "true";
            }
            else if (request.RequestUri?.AbsolutePath.EndsWith("/general/size", StringComparison.Ordinal) == true)
            {
                responseBody = "10";
            }
            return new HttpResponseMessage(NextStatusCode)
            {
                Content = new StringContent(responseBody, Encoding.UTF8, "application/json"),
            };
        }
    }
}
