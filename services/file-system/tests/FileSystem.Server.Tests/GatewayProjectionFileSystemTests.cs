using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using FileSystem.Server;
using Xunit;

namespace FileSystem.Server.Tests;

public sealed class GatewayProjectionFileSystemTests
{
    [Fact]
    public void ControlPinRead_IsDiscoveryBased()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-pin-read", gateway);

        using (var beforeDiscover = fileSystem.OpenFile(@"\control\pin", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None))
        using (var reader = new StreamReader(beforeDiscover, Encoding.UTF8, leaveOpen: false))
        {
            Assert.Equal(string.Empty, reader.ReadToEnd());
        }

        using (var discoveredRead = fileSystem.OpenFile(@"\ledger\state\written.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None))
        using (var reader = new StreamReader(discoveredRead, Encoding.UTF8, leaveOpen: false))
        {
            _ = reader.ReadToEnd();
        }

        using var afterDiscover = fileSystem.OpenFile(@"\control\pin", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var afterReader = new StreamReader(afterDiscover, Encoding.UTF8, leaveOpen: false);
        var afterText = afterReader.ReadToEnd();

        Assert.Contains("pinned /ledger/state/written.txt", afterText, StringComparison.Ordinal);
    }

    [Fact]
    public void ControlPinWrite_AppliesPinnedAndUnpinnedDirectives()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-pin-write", gateway);

        using (var writeStream = fileSystem.OpenFile(@"\control\pin", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(writeStream, new UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.WriteLine("pinned /ledger/state/hello.txt");
            writer.WriteLine("unpinned /ledger/state/written.txt");
            writer.WriteLine("pinned /ledger/state/docs");
        }

        Assert.Equal("""[-1,["*state*","docs"]]""", gateway.LastPinPathJson);
        Assert.Equal("""[-1,["*state*","written.txt"]]""", gateway.LastUnpinPathJson);

        using var readBack = fileSystem.OpenFile(@"\control\pin", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var reader = new StreamReader(readBack, Encoding.UTF8, leaveOpen: false);
        var rendered = reader.ReadToEnd();

        Assert.Contains("pinned /ledger/state/hello.txt", rendered, StringComparison.Ordinal);
        Assert.Contains("unpinned /ledger/state/written.txt", rendered, StringComparison.Ordinal);
        Assert.Contains("pinned /ledger/state/docs", rendered, StringComparison.Ordinal);
    }

    [Fact]
    public void ControlPinWrite_RejectsInvalidDirective()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-pin-invalid", gateway);

        var exception = Assert.Throws<InvalidDataException>(() =>
        {
            using var invalidStream = fileSystem.OpenFile(@"\control\pin", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None);
            using var writer = new StreamWriter(invalidStream, new UTF8Encoding(false), 1024, leaveOpen: false);
            writer.WriteLine("pinndded /ledger/state/hello.txt");
        });

        Assert.Contains("Invalid pin control directive", exception.Message, StringComparison.Ordinal);
    }

    [Fact]
    public void Move_UsesGatewayBatchForStageWrites()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-batch-move", gateway, enableStageWrites: true);

        _ = fileSystem.ListEntriesInDirectory(@"\stage");
        fileSystem.Move(@"\stage\source.txt", @"\stage\target.txt");

        Assert.Equal(1, gateway.BatchRequestCount);
        Assert.NotNull(gateway.LastBatchRequest);
        Assert.Equal(2, gateway.LastBatchRequest!.Requests.Count);
        Assert.Equal("set!", gateway.LastBatchRequest.Requests[0].Function);
        Assert.Equal("set!", gateway.LastBatchRequest.Requests[1].Function);
        Assert.Equal(
            """[["*state*","target.txt"]]""",
            gateway.LastBatchRequest.Requests[0].Arguments?["path"]?.ToJsonString());
        Assert.Equal(
            """[["*state*","source.txt"]]""",
            gateway.LastBatchRequest.Requests[1].Arguments?["path"]?.ToJsonString());
    }

    [Fact]
    public void SetAttributes_OnOpenWritableStageFile_DefersGatewayRewriteUntilCommit()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-write-coalesce", gateway, enableStageWrites: true);

        using (var stream = fileSystem.OpenFile(@"\stage\coalesced.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None))
        {
            stream.Write(Encoding.UTF8.GetBytes("coalesced\n"));
            fileSystem.SetAttributes(@"\stage\coalesced.txt", isHidden: null, isReadonly: true, isArchived: null);
            Assert.Equal(0, gateway.SetRequestCount);
        }

        Assert.Equal(1, gateway.SetRequestCount);
        Assert.Equal("""[["*state*","coalesced.txt"]]""", gateway.LastSetPathJson);
        Assert.Equal("""{"mode":292}""", gateway.LastSetContent?["*file-system/file*"]?["meta"]?.ToJsonString());
    }

    [Fact]
    public void OpenWritableStageFile_BatchesParentAndTargetHydrationReads()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-read-batch", gateway, enableStageWrites: true);

        using var stream = fileSystem.OpenFile(@"\stage\docs\source.txt", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None);

        Assert.Equal(1, gateway.BatchRequestCount);
        Assert.NotNull(gateway.LastBatchRequest);
        Assert.Equal(2, gateway.LastBatchRequest!.Requests.Count);
        Assert.Equal("get", gateway.LastBatchRequest.Requests[0].Function);
        Assert.Equal("get", gateway.LastBatchRequest.Requests[1].Function);
        Assert.Equal(
            """[["*state*","docs"]]""",
            gateway.LastBatchRequest.Requests[0].Arguments?["path"]?.ToJsonString());
        Assert.Equal(
            """[["*state*","docs","source.txt"]]""",
            gateway.LastBatchRequest.Requests[1].Arguments?["path"]?.ToJsonString());
    }

    private sealed class RecordingGatewayClient : IGeneralInterfaceClient
    {
        public int SetRequestCount { get; private set; }

        public string? LastSetPathJson { get; private set; }

        public JsonNode? LastSetContent { get; private set; }

        public string? LastPinPathJson { get; private set; }

        public string? LastUnpinPathJson { get; private set; }

        public GatewayBatchRequest? LastBatchRequest { get; private set; }

        public int BatchRequestCount { get; private set; }

        public Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var pathJson = JsonSerializer.Serialize(request.Path);

            return Task.FromResult(pathJson switch
            {
                """[["*state*"]]""" => JsonNode.Parse("""{"content":["directory",{"source.txt":"value"},false],"pinned?":false}"""),
                """[["*state*","docs"]]""" => JsonNode.Parse("""{"content":["directory",{"source.txt":"value"},false],"pinned?":false}"""),
                """[["*state*","source.txt"]]""" => JsonNode.Parse("""{"content":{"*type/byte-vector*":"736f757263650a"},"pinned?":false}"""),
                """[["*state*","docs","source.txt"]]""" => JsonNode.Parse("""{"content":{"*type/byte-vector*":"736f757263650a"},"pinned?":false}"""),
                """[-1,["*state*","written.txt"]]""" => JsonNode.Parse("""{"content":{"*type/byte-vector*":"70696e6e65642066696c650a"},"pinned?":true}"""),
                _ => JsonNode.Parse("""{"content":{"*type/byte-vector*":"756e70696e6e65640a"},"pinned?":false}""")
            });
        }

        public Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            SetRequestCount++;
            LastSetPathJson = JsonSerializer.Serialize(request.Path);
            LastSetContent = request.Content.DeepClone();
            return Task.FromResult<JsonNode?>(JsonValue.Create(true));
        }

        public Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            BatchRequestCount++;
            LastBatchRequest = request;
            var output = new JsonArray();
            foreach (var operation in request.Requests)
            {
                if (operation.Function == "get" && operation.Arguments is not null)
                {
                    var path = JsonSerializer.Deserialize<object[]>(operation.Arguments["path"]!.ToJsonString())
                        ?? throw new InvalidDataException("Batch request path is invalid.");
                    var pinned = operation.Arguments["pinned?"]?.GetValue<bool>() ?? false;
                    var proof = operation.Arguments["proof?"]?.GetValue<bool>() ?? false;
                    output.Add(GetAsync(new GatewayGetRequest(path, pinned, proof), cancellationToken).GetAwaiter().GetResult());
                    continue;
                }

                output.Add(JsonValue.Create(true));
            }

            return Task.FromResult<JsonNode?>(output);
        }

        public Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            LastPinPathJson = JsonSerializer.Serialize(request.Path);
            return Task.FromResult(true);
        }

        public Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            LastUnpinPathJson = JsonSerializer.Serialize(request.Path);
            return Task.FromResult(true);
        }

        public Task<long> SizeAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.FromResult(10L);
        }

        public Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.FromResult<IReadOnlyList<string>>(new[] { "alice", "bob" });
        }
    }
}
