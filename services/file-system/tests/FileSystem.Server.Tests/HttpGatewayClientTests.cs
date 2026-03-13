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
                true),
            CancellationToken.None);

        Assert.Equal("http://gateway/api/v1/general/get", handler.LastRequestUri);
        Assert.Equal("Bearer secret-token", handler.LastAuthorization);
        Assert.Equal("""[["*state*","docs"]]""", handler.LastBody?["path"]?.ToJsonString());
        Assert.True(handler.LastBody?["details?"]?.GetValue<bool>());
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
    public async Task PeersAsync_TreatsEmptyBodyAsEmptyPeerList()
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

        var peers = await gateway.PeersAsync(CancellationToken.None);

        Assert.Empty(peers);
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
            else if (request.RequestUri?.AbsolutePath.EndsWith("/general/pin", StringComparison.Ordinal) == true ||
                     request.RequestUri?.AbsolutePath.EndsWith("/general/unpin", StringComparison.Ordinal) == true)
            {
                responseBody = "true";
            }
            else if (request.RequestUri?.AbsolutePath.EndsWith("/general/size", StringComparison.Ordinal) == true)
            {
                responseBody = "10";
            }
            else if (request.RequestUri?.AbsolutePath.EndsWith("/general/peers", StringComparison.Ordinal) == true && !string.IsNullOrEmpty(responseBody))
            {
                responseBody = """["alice","bob"]""";
            }

            return new HttpResponseMessage(NextStatusCode)
            {
                Content = new StringContent(responseBody, Encoding.UTF8, "application/json"),
            };
        }
    }
}
