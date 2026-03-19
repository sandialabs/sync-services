using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text;
using System.Net;
using System.Net.Http;
using Microsoft.Extensions.Options;
using SMBLibrary;
using SmbFileAttributes = SMBLibrary.FileAttributes;

namespace FileSystem.Server;

public sealed class GrammarValidationWorker : BackgroundService
{
    private readonly ILogger<GrammarValidationWorker> _logger;
    private readonly IHostApplicationLifetime _lifetime;
    private readonly ServerOptions _options;

    public GrammarValidationWorker(
        ILogger<GrammarValidationWorker> logger,
        IHostApplicationLifetime lifetime,
        IOptions<ServerOptions> options)
    {
        _logger = logger;
        _lifetime = lifetime;
        _options = options.Value;
    }

    protected override Task ExecuteAsync(CancellationToken stoppingToken)
    {
        try
        {
            ValidatePathCompilation();
            ValidatePathDecompilation();
            ValidateInvalidPaths();
            ValidateFixtureLoading();
            ValidateReadOnlyMutations();
            ValidateMockGatewayClient();
            ValidateHttpGatewayClient();
            ValidateHttpJournalClient();
            ValidateGatewayProjectionFileSystem();
            ValidateContextualNodeListing();
            ValidatePinControlNamespace();
            ValidateSymlinkAwareFileStore();
            ValidateGatewayProjectionErrorMapping();
            ValidateGatewayStageMutationFileSystem();
            _logger.LogInformation("grammar validation completed successfully");
            _lifetime.StopApplication();
            return Task.CompletedTask;
        }
        catch (Exception exception)
        {
            _logger.LogError(exception, "grammar validation failed");
            throw;
        }
    }

    private void ValidatePathCompilation()
    {
        AssertCompiled("""[["*state*"]]""", @"\stage");
        AssertCompiled("""[["*state*","docs","guide.txt"]]""", @"\stage\docs\guide.txt");
        AssertCompiled("""[3,["*state*","archive.txt"]]""", @"\ledger\previous\3\state\archive.txt");
        AssertCompiled("""[-1,["*state*","latest.txt"]]""", @"\ledger\previous\-1\state\latest.txt");
        AssertCompiled(
            """[9,["*peer*","alice","chain"],["*state*","current-remote.txt"]]""",
            @"\ledger\peer\alice\state\current-remote.txt");
        AssertCompiled(
            """[9,["*peer*","alice","chain"],2,["*state*","remote-note.txt"]]""",
            @"\ledger\peer\alice\previous\2\state\remote-note.txt");
        AssertCompiled(
            """[9,["*peer*","alice","chain"],-1,["*peer*","bob","chain"],4,["*state*","deep.txt"]]""",
            @"\ledger\peer\alice\previous\-1\peer\bob\previous\4\state\deep.txt");
    }

    private void ValidatePathDecompilation()
    {
        AssertDecompiled(
            @"\stage\docs\guide.txt",
            """[["*state*","docs","guide.txt"]]""");
        AssertDecompiled(
            @"\ledger\previous\3\state\archive.txt",
            """[3,["*state*","archive.txt"]]""");
        AssertDecompiled(
            @"\ledger\previous\-1\state\latest.txt",
            """[-1,["*state*","latest.txt"]]""");
        AssertDecompiled(
            @"\ledger\peer\alice\state\current-remote.txt",
            """[-1,["*peer*","alice","chain"],["*state*","current-remote.txt"]]""");
        AssertDecompiled(
            @"\ledger\peer\alice\previous\2\state\remote-note.txt",
            """[-1,["*peer*","alice","chain"],2,["*state*","remote-note.txt"]]""");
        AssertDecompiled(
            @"\ledger\peer\alice\previous\-1\peer\bob\previous\4\state\deep.txt",
            """[-1,["*peer*","alice","chain"],-1,["*peer*","bob","chain"],4,["*state*","deep.txt"]]""");

        Assert(
            !JournalPathMapper.TryDecompileProjectedPath(@"\ledger\state\hello.txt", out _),
            @"\ledger\state\... should remain derived-only and not persist to the fixture");
        Assert(JournalPathMapper.IsWritableProjectedPath(@"\stage"), @"\stage should be writable");
        Assert(JournalPathMapper.IsWritableProjectedPath(@"\stage\docs"), @"\stage descendants should be writable");
        Assert(!JournalPathMapper.IsWritableProjectedPath(@"\ledger"), @"\ledger should be read-only");
        Assert(!JournalPathMapper.IsWritableProjectedPath(@"\ledger\state\docs"), @"\ledger/state should be read-only");
        Assert(!JournalPathMapper.IsWritableProjectedPath(@"\ledger\previous\3\state"), @"\ledger/previous should be read-only");
    }

    private void ValidateInvalidPaths()
    {
        AssertCompileFails("""{"bad":"root"}""", "Fixture path must be an array.");
        AssertCompileFails("""[1]""", "Fixture path block must be an array of strings.");
        AssertCompileFails("""[["*peer*","alice","chain"]]""", "Fixture stage path must begin with [\"*state*\", ...].");
        AssertCompileFails("""[1,["*peer*","alice","chain"]]""", "Ledger fixture path must terminate in a state block.");
        AssertCompileFails("""[1,["*state*","docs"],2]""", "State block must terminate the fixture path.");

        Assert(
            !JournalPathMapper.TryDecompileProjectedPath(@"\ledger\previous\abc\state\file.txt", out _),
            "non-integer previous segment should fail decompilation");
        Assert(
            !JournalPathMapper.TryDecompileProjectedPath(@"\ledger\peer", out _),
            "missing peer name should fail decompilation");
        Assert(
            !JournalPathMapper.TryDecompileProjectedPath(@"\ledger\previous\2", out _),
            "missing terminal state should fail decompilation");
    }

    private void ValidateFixtureLoading()
    {
        var fileSystem = JsonFileSystemLoader.LoadFromFile(_options.JsonFixturePath, "syncfs-validation");
        var snapshot = fileSystem.ExportSnapshot();

        Assert(snapshot.Directories.Any(directory => directory.Path == "stage"), "fixture should expose stage root");
        Assert(snapshot.Directories.Any(directory => directory.Path == "ledger"), "fixture should expose ledger root");
        Assert(snapshot.Directories.Any(directory => directory.Path == "ledger/state"), "fixture should mirror stage into ledger/state");
        Assert(snapshot.Files.Any(file => file.Path == "stage/hello.txt"), "fixture should expose stage/hello.txt");
        Assert(snapshot.Files.Any(file => file.Path == "stage/guide-link"), "fixture should expose stage symlink entry");
        Assert(snapshot.Files.Any(file => file.Path == "ledger/state/hello.txt"), "fixture should expose mirrored ledger/state/hello.txt");
        Assert(snapshot.Files.Any(file => file.Path == "ledger/previous/3/state/archive.txt"), "fixture should expose previous snapshot file");
        Assert(snapshot.Files.Any(file => file.Path == "ledger/peer/alice/state/current-remote.txt"), "fixture should expose peer state file");
        Assert(snapshot.Files.Any(file => file.Path == "ledger/peer/alice/previous/2/state/remote-note.txt"), "fixture should expose recursive peer previous file");
        Assert(fileSystem.TryGetSymlink(@"\stage\guide-link", out var fixtureSymlink), "fixture should materialize stage symlink");
        Assert(string.Equals(fixtureSymlink.ProjectedTargetPath, @"\stage\docs\guide.txt", StringComparison.Ordinal), "fixture symlink should resolve to the projected stage target");

        const string duplicateFixture = """
            [
              [[["*state*","hello.txt"]],{"content":{"*file-system/file*":{"content":"one"}},"pinned?":false}],
              [[["*state*","hello.txt"]],{"content":{"*file-system/file*":{"content":"two"}},"pinned?":false}]
            ]
            """;
        AssertThrows<InvalidDataException>(
            () => JsonFileSystemLoader.LoadFromJson(duplicateFixture, "duplicate"),
            "Duplicate fixture path");
    }

    private void ValidateReadOnlyMutations()
    {
        var fileSystem = JsonFileSystemLoader.LoadFromFile(_options.JsonFixturePath, "syncfs-readonly");

        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.CreateFile(@"\ledger\state\blocked.txt"),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.CreateDirectory(@"\ledger\peer\alice\state\blocked"),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.Delete(@"\ledger\previous\3\state\archive.txt"),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.Move(@"\ledger\previous\3\state\archive.txt", @"\ledger\previous\3\state\archive-2.txt"),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.OpenFile(@"\ledger\state\hello.txt", FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.SetAttributes(@"\ledger\state\hello.txt", isHidden: true, isReadonly: null, isArchived: null),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.SetDates(@"\ledger\state\hello.txt", DateTime.UtcNow, null, null),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.SetMetadata(
                @"\ledger\state\hello.txt",
                new InMemoryFileSystem.SnapshotMetadata(null, null, null, null, null, null)),
            "Read-only projected path");
        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.SetControl(
                @"\ledger\state\hello.txt",
                new InMemoryFileSystem.SnapshotControl(false, "bytes", null, null, null)),
            "Read-only projected path");
    }

    private void ValidateMockGatewayClient()
    {
        var tempFixturePath = Path.Combine(Path.GetTempPath(), $"syncfs-gateway-{Guid.NewGuid():N}.json");
        File.Copy(_options.JsonFixturePath, tempFixturePath, overwrite: true);

        try
        {
            var gateway = new MockGatewayClient(tempFixturePath, _logger);
            var stagePath = new object[]
            {
                new object[] { "*state*", "hello.txt" }
            };

            var readResult = gateway.GetAsync(new GatewayGetRequest(stagePath, true, false), CancellationToken.None)
                .GetAwaiter()
                .GetResult();
            Assert(readResult is JsonObject, "mock gateway get should return a JSON object when metadata is requested");
            Assert(
                string.Equals(readResult?["pinned?"]?.GetValue<bool>().ToString(), bool.FalseString, StringComparison.OrdinalIgnoreCase),
                "mock gateway get should include pinned?");

            var setPath = new object[]
            {
                new object[] { "*state*", "gateway-smoke.txt" }
            };
            var setContent = JsonNode.Parse("""{"*file-system/file*":{"content":"Gateway-backed write\n"}}""")!;
            gateway.SetAsync(new GatewaySetRequest(setPath, setContent), CancellationToken.None)
                .GetAwaiter()
                .GetResult();

            var written = gateway.GetAsync(new GatewayGetRequest(setPath, true, false), CancellationToken.None)
                .GetAwaiter()
                .GetResult();
            Assert(
                string.Equals(written?["content"]?.GetValue<string>(), "Gateway-backed write\n", StringComparison.Ordinal),
                "mock gateway set! should persist content");
        }
        finally
        {
            File.Delete(tempFixturePath);
        }
    }

    private void ValidateHttpGatewayClient()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://gateway/api/v1/", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var gateway = new HttpGatewayClient(httpClient, "secret-token");

        var getPath = new object[]
        {
            new object[] { "*state*", "docs" }
        };
        var getResult = gateway.GetAsync(new GatewayGetRequest(getPath, true, false), CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        Assert(string.Equals(handler.LastRequestUri, "http://gateway/api/v1/general/get", StringComparison.Ordinal), "http gateway get URI should match gateway route");
        Assert(string.Equals(handler.LastAuthorization, "Bearer secret-token", StringComparison.Ordinal), "http gateway should send bearer auth");
        Assert(string.Equals(handler.LastBody?["path"]?.ToJsonString(), """[["*state*","docs"]]""", StringComparison.Ordinal), "http gateway get should serialize journal path");
        Assert(handler.LastBody?["pinned?"]?.GetValue<bool>() == true, "http gateway get should serialize pinned?");
        Assert(handler.LastBody?["proof?"]?.GetValue<bool>() == false, "http gateway get should serialize proof? false for file-system reads");
        Assert(string.Equals(getResult?["ok"]?.GetValue<string>(), "get", StringComparison.Ordinal), "http gateway get should parse JSON response");

        var setPath = new object[]
        {
            new object[] { "*state*", "notes", "todo.txt" }
        };
        var setContent = JsonNode.Parse("""{"*file-system/file*":{"content":"updated\n"}}""")!;
        var setResult = gateway.SetAsync(new GatewaySetRequest(setPath, setContent), CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        Assert(string.Equals(handler.LastRequestUri, "http://gateway/api/v1/general/set", StringComparison.Ordinal), "http gateway set URI should match gateway route");
        Assert(string.Equals(handler.LastBody?["path"]?.ToJsonString(), """[["*state*","notes","todo.txt"]]""", StringComparison.Ordinal), "http gateway set should serialize journal path");
        Assert(string.Equals(handler.LastBody?["value"]?["*file-system/file*"]?["content"]?.GetValue<string>(), "updated\n", StringComparison.Ordinal), "http gateway set should serialize value");
        Assert(string.Equals(setResult?["ok"]?.GetValue<string>(), "set", StringComparison.Ordinal), "http gateway set should parse JSON response");

        var pinPath = new object[]
        {
            -1,
            new object[] { "*state*", "notes", "todo.txt" }
        };
        var pinResult = gateway.PinAsync(new GatewayPinRequest(pinPath), CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        Assert(pinResult, "http gateway pin should parse boolean response");
        Assert(string.Equals(handler.LastRequestUri, "http://gateway/api/v1/general/pin", StringComparison.Ordinal), "http gateway pin URI should match gateway route");
        Assert(string.Equals(handler.LastBody?["path"]?.ToJsonString(), """[-1,["*state*","notes","todo.txt"]]""", StringComparison.Ordinal), "http gateway pin should serialize journal path");

        var unpinResult = gateway.UnpinAsync(new GatewayPinRequest(pinPath), CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        Assert(unpinResult, "http gateway unpin should parse boolean response");
        Assert(string.Equals(handler.LastRequestUri, "http://gateway/api/v1/general/unpin", StringComparison.Ordinal), "http gateway unpin URI should match gateway route");
        Assert(string.Equals(handler.LastBody?["path"]?.ToJsonString(), """[-1,["*state*","notes","todo.txt"]]""", StringComparison.Ordinal), "http gateway unpin should serialize journal path");

        handler.NextStatusCode = HttpStatusCode.BadRequest;
        handler.NextResponseBody = JsonNode.Parse("""{"error":"authentication-error","message":"bad token","details":["error"],"source":"journal"}""");
        AssertThrows<GatewaySemanticException>(
            () => gateway.GetAsync(new GatewayGetRequest(getPath, true, false), CancellationToken.None).GetAwaiter().GetResult(),
            "bad token");

        var size = gateway.SizeAsync(CancellationToken.None).GetAwaiter().GetResult();
        Assert(size == 10L, "http gateway size should parse numeric responses");

        var bridges = gateway.BridgesAsync(CancellationToken.None).GetAwaiter().GetResult();
        Assert(bridges.SequenceEqual(new[] { "alice", "bob" }), "http gateway bridges should parse string arrays");

        handler.NextRespondWithEmptyBody = true;
        var emptyBridges = gateway.BridgesAsync(CancellationToken.None).GetAwaiter().GetResult();
        Assert(emptyBridges.Count == 0, "http gateway bridges should treat null response as empty");
    }

    private void ValidateHttpJournalClient()
    {
        var handler = new RecordingHttpMessageHandler();
        using var httpClient = new HttpClient(handler)
        {
            BaseAddress = new Uri("http://journal/interface/json", UriKind.Absolute),
            Timeout = TimeSpan.FromSeconds(5),
        };
        using var journal = new HttpJournalClient(httpClient, "secret-token");

        var getPath = new object[]
        {
            new object[] { "*state*", "docs" }
        };
        var getResult = journal.GetAsync(new GatewayGetRequest(getPath, true, false), CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        Assert(string.Equals(handler.LastRequestUri, "http://journal/interface/json", StringComparison.Ordinal), "http journal get URI should match direct journal endpoint");
        Assert(handler.LastAuthorization == null, "http journal should not send bearer auth");
        Assert(string.Equals(handler.LastBody?["function"]?.GetValue<string>(), "get", StringComparison.Ordinal), "http journal get should use function envelope");
        Assert(string.Equals(handler.LastBody?["authentication"]?["*type/string*"]?.GetValue<string>(), "secret-token", StringComparison.Ordinal), "http journal get should serialize authentication in the body");
        Assert(string.Equals(handler.LastBody?["arguments"]?["path"]?.ToJsonString(), """[["*state*","docs"]]""", StringComparison.Ordinal), "http journal get should serialize journal path");
        Assert(handler.LastBody?["arguments"]?["pinned?"]?.GetValue<bool>() == true, "http journal get should serialize pinned?");
        Assert(handler.LastBody?["arguments"]?["proof?"]?.GetValue<bool>() == false, "http journal get should serialize proof? false for file-system reads");
        Assert(string.Equals(getResult?["ok"]?.GetValue<string>(), "get", StringComparison.Ordinal), "http journal get should parse JSON response");

        var batchResult = journal.BatchAsync(
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
                        new GatewayBatchOperation("configuration", null),
                    }),
                CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        Assert(string.Equals(handler.LastBody?["function"]?.GetValue<string>(), "general-batch!", StringComparison.Ordinal), "http journal batch should use general-batch!");
        Assert(string.Equals(handler.LastBody?["arguments"]?["requests"]?[0]?["function"]?.GetValue<string>(), "set!", StringComparison.Ordinal), "http journal batch should preserve function names");
        Assert(string.Equals(handler.LastBody?["arguments"]?["requests"]?[1]?["function"]?.GetValue<string>(), "configuration", StringComparison.Ordinal), "http journal batch should preserve zero-argument requests");
        Assert(string.Equals(batchResult?[0]?["ok"]?.GetValue<string>(), "batch", StringComparison.Ordinal), "http journal batch should parse array response");
    }

    private void ValidateGatewayProjectionFileSystem()
    {
        var tempFixturePath = Path.Combine(Path.GetTempPath(), $"syncfs-projection-{Guid.NewGuid():N}.json");
        File.Copy(_options.JsonFixturePath, tempFixturePath, overwrite: true);

        try
        {
            var gateway = new MockGatewayClient(tempFixturePath, _logger);
            var fileSystem = new GatewayProjectionFileSystem("projection", gateway);

            var rootEntries = fileSystem.ListEntriesInDirectory(@"\");
            Assert(rootEntries.Any(entry => entry.Name == "stage"), "gateway projection root should list stage");
            Assert(rootEntries.Any(entry => entry.Name == "ledger"), "gateway projection root should list ledger");
            Assert(rootEntries.Any(entry => entry.Name == "control"), "gateway projection root should list control");

            var stageEntries = fileSystem.ListEntriesInDirectory(@"\stage");
            Assert(stageEntries.Any(entry => entry.Name == "hello.txt"), "gateway projection stage should list hello.txt");

            using var helloStream = fileSystem.OpenFile(@"\stage\hello.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
            using var helloReader = new StreamReader(helloStream, System.Text.Encoding.UTF8, leaveOpen: false);
            var helloText = helloReader.ReadToEnd();
            Assert(helloText.Contains("Synchronic file-system JSON fixture.", StringComparison.Ordinal), "gateway projection should read stage file content");

            var ledgerEntries = fileSystem.ListEntriesInDirectory(@"\ledger");
            Assert(ledgerEntries.Any(entry => entry.Name == "state"), "gateway projection ledger should list state");
            Assert(ledgerEntries.Any(entry => entry.Name == "peer"), "gateway projection ledger should list peer");
            Assert(ledgerEntries.Any(entry => entry.Name == "previous"), "gateway projection ledger should list previous");

            var peerEntries = fileSystem.ListEntriesInDirectory(@"\ledger\peer");
            Assert(peerEntries.Any(entry => entry.Name == "alice"), "gateway projection ledger/peer should list peer names");

            using var remoteStream = fileSystem.OpenFile(@"\ledger\peer\alice\state\current-remote.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
            using var remoteReader = new StreamReader(remoteStream, System.Text.Encoding.UTF8, leaveOpen: false);
            var remoteText = remoteReader.ReadToEnd();
            Assert(remoteText.Contains("mock current related-peer entry", StringComparison.Ordinal), "gateway projection should read peer state content");
            Assert(fileSystem.TryGetSymlink(@"\stage\guide-link", out var gatewaySymlink), "gateway projection should materialize stage symlink");
            Assert(string.Equals(gatewaySymlink.ProjectedTargetPath, @"\stage\docs\guide.txt", StringComparison.Ordinal), "gateway projection symlink should resolve to projected target");

            AssertThrows<UnauthorizedAccessException>(
                () => fileSystem.CreateFile(@"\stage\blocked.txt"),
                "Read-only projected path");
        }
        finally
        {
            File.Delete(tempFixturePath);
        }
    }

    private void ValidatePinControlNamespace()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-pin", gateway);

        var rootEntries = fileSystem.ListEntriesInDirectory(@"\");
        Assert(rootEntries.Any(entry => entry.Name == "control"), "gateway projection root should list control");

        var controlEntries = fileSystem.ListEntriesInDirectory(@"\control");
        Assert(controlEntries.Any(entry => entry.Name == "pin"), @"\control should list pin");

        using var beforeDiscover = fileSystem.OpenFile(@"\control\pin", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var beforeReader = new StreamReader(beforeDiscover, Encoding.UTF8, leaveOpen: false);
        var beforeText = beforeReader.ReadToEnd();
        Assert(string.IsNullOrEmpty(beforeText), @"\control\pin should start empty before ledger reads");

        using var discoveredRead = fileSystem.OpenFile(@"\ledger\state\written.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var discoveredReader = new StreamReader(discoveredRead, Encoding.UTF8, leaveOpen: false);
        _ = discoveredReader.ReadToEnd();

        using var afterDiscover = fileSystem.OpenFile(@"\control\pin", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var afterReader = new StreamReader(afterDiscover, Encoding.UTF8, leaveOpen: false);
        var afterText = afterReader.ReadToEnd();
        Assert(afterText.Contains("pinned /ledger/state/written.txt", StringComparison.Ordinal), @"\control\pin should render discovered ledger pin state");

        using (var writeStream = fileSystem.OpenFile(@"\control\pin", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(writeStream, new UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.WriteLine("pinned /ledger/state/hello.txt");
            writer.WriteLine("unpinned /ledger/state/written.txt");
            writer.WriteLine("pinned /ledger/state/docs");
        }

        Assert(string.Equals(gateway.LastPinPathJson, """[-1,["*state*","docs"]]""", StringComparison.Ordinal), "pin control file should pin the listed ledger directory path");
        Assert(string.Equals(gateway.LastUnpinPathJson, """[-1,["*state*","written.txt"]]""", StringComparison.Ordinal), "pin control file should unpin the listed ledger file path");

        using var afterWrite = fileSystem.OpenFile(@"\control\pin", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var afterWriteReader = new StreamReader(afterWrite, Encoding.UTF8, leaveOpen: false);
        var afterWriteText = afterWriteReader.ReadToEnd();
        Assert(afterWriteText.Contains("pinned /ledger/state/hello.txt", StringComparison.Ordinal), "pin control file should retain pinned file directive");
        Assert(afterWriteText.Contains("unpinned /ledger/state/written.txt", StringComparison.Ordinal), "pin control file should retain unpinned file directive");
        Assert(afterWriteText.Contains("pinned /ledger/state/docs", StringComparison.Ordinal), "pin control file should retain pinned directory directive");

        AssertThrows<InvalidDataException>(
            () =>
            {
                using var invalidStream = fileSystem.OpenFile(@"\control\pin", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None);
                using var invalidWriter = new StreamWriter(invalidStream, new UTF8Encoding(false), 1024, leaveOpen: false);
                invalidWriter.WriteLine("pinned /stage/hello.txt");
            },
            "/ledger/...");
        AssertThrows<NotSupportedException>(
            () => fileSystem.Delete(@"\control\pin"),
            "cannot be deleted");
        AssertThrows<NotSupportedException>(
            () => fileSystem.Move(@"\control\pin", @"\control\pin-new"),
            "Pin control paths do not support rename.");
        AssertThrows<NotSupportedException>(
            () => fileSystem.CreateDirectory(@"\control\pin"),
            "a file");
    }

    private void ValidateContextualNodeListing()
    {
        var fileSystem = new GatewayProjectionFileSystem("projection-contextual-peers", new ContextualPeerGatewayClient());

        var rootPeerEntries = fileSystem.ListEntriesInDirectory(@"\ledger\peer");
        Assert(rootPeerEntries.Any(entry => entry.Name == "journal-4"), "root ledger/peer should include journal-4");
        Assert(rootPeerEntries.Any(entry => entry.Name == "journal-7"), "root ledger/peer should include journal-7");

        var nestedPeerEntries = fileSystem.ListEntriesInDirectory(@"\ledger\peer\journal-7\peer");
        Assert(nestedPeerEntries.Any(entry => entry.Name == "journal-2"), "nested ledger/peer should include journal-2");
        Assert(nestedPeerEntries.Any(entry => entry.Name == "journal-3"), "nested ledger/peer should include journal-3");
        Assert(!nestedPeerEntries.Any(entry => entry.Name == "journal-4"), "nested ledger/peer should not reuse root journal-4 peer");
        Assert(!nestedPeerEntries.Any(entry => entry.Name == "journal-7"), "nested ledger/peer should not reuse root journal-7 peer");
    }

    private void ValidateSymlinkAwareFileStore()
    {
        var fileSystem = JsonFileSystemLoader.LoadFromFile(_options.JsonFixturePath, "syncfs-symlink-validation");
        var store = new SymlinkAwareFileStore(fileSystem);
        var securityContext = new SecurityContext("user", "machine", new IPEndPoint(IPAddress.Loopback, 0), null!, new object());

        var createStatus = store.CreateFile(
            out var handle,
            out var fileStatus,
            @"\stage\guide-link",
            (AccessMask)FileAccessMask.FILE_READ_ATTRIBUTES,
            0,
            ShareAccess.Read,
            CreateDisposition.FILE_OPEN,
            CreateOptions.FILE_OPEN_REPARSE_POINT,
            securityContext);
        Assert(createStatus == NTStatus.STATUS_SUCCESS, $"symlink open should succeed, got {createStatus}");
        Assert(fileStatus == FileStatus.FILE_OPENED, "symlink open should report FILE_OPENED");

        var queryStatus = store.GetFileInformation(out var info, handle, FileInformationClass.FileAttributeTagInformation);
        Assert(queryStatus == NTStatus.STATUS_SUCCESS, $"symlink tag query should succeed, got {queryStatus}");
        Assert(info is FileAttributeTagInformation, "symlink tag query should return FileAttributeTagInformation");
        var tagInfo = (FileAttributeTagInformation)info;
        Assert((tagInfo.FileAttributes & SmbFileAttributes.ReparsePoint) != 0, "symlink tag query should set reparse-point attribute");
        Assert(tagInfo.ReparsePointTag == 0xA000000C, "symlink tag query should use symlink reparse tag");

        var ioctlStatus = store.DeviceIOControl(handle, (uint)IoControlCode.FSCTL_GET_REPARSE_POINT, Array.Empty<byte>(), out var reparseBuffer, 4096);
        Assert(ioctlStatus == NTStatus.STATUS_SUCCESS, $"symlink get-reparse-point should succeed, got {ioctlStatus}");
        Assert(reparseBuffer.Length >= 20, "symlink reparse buffer should contain header and payload");

        var followedOpenStatus = store.CreateFile(
            out var followedHandle,
            out _,
            @"\stage\guide-link",
            (AccessMask)FileAccessMask.FILE_READ_DATA,
            0,
            ShareAccess.Read,
            CreateDisposition.FILE_OPEN,
            0,
            securityContext);
        Assert(followedOpenStatus == NTStatus.STATUS_SUCCESS, $"followed symlink open should succeed, got {followedOpenStatus}");
        var readStatus = store.ReadFile(out var data, followedHandle, 0, 4096);
        Assert(readStatus == NTStatus.STATUS_SUCCESS, $"followed symlink read should succeed, got {readStatus}");
        Assert(Encoding.UTF8.GetString(data).Contains("This file lives under docs/.", StringComparison.Ordinal), "followed symlink read should return target content");
        store.CloseFile(followedHandle);
        store.CloseFile(handle);

        var createLinkStatus = store.CreateFile(
            out var createHandle,
            out _,
            @"\stage\guide-link-created",
            (AccessMask)(FileAccessMask.FILE_WRITE_DATA | FileAccessMask.FILE_WRITE_ATTRIBUTES | FileAccessMask.FILE_READ_ATTRIBUTES),
            0,
            ShareAccess.Read,
            CreateDisposition.FILE_CREATE,
            CreateOptions.FILE_OPEN_REPARSE_POINT,
            securityContext);
        Assert(createLinkStatus == NTStatus.STATUS_SUCCESS, $"symlink create handle should succeed, got {createLinkStatus}");

        var createBuffer = BuildSymlinkReparseBufferForValidation(@"\stage\docs\guide.txt");
        var setStatus = store.DeviceIOControl(createHandle, (uint)IoControlCode.FSCTL_SET_REPARSE_POINT, createBuffer, out _, 0);
        Assert(setStatus == NTStatus.STATUS_SUCCESS, $"symlink set-reparse-point should succeed, got {setStatus}");
        Assert(fileSystem.TryGetSymlink(@"\stage\guide-link-created", out var createdSymlink), "symlink create should materialize in backing filesystem");
        Assert(string.Equals(createdSymlink.ProjectedTargetPath, @"\stage\docs\guide.txt", StringComparison.Ordinal), "created symlink should target projected guide path");
        store.CloseFile(createHandle);

        var createdOpenStatus = store.CreateFile(
            out var createdFollowHandle,
            out _,
            @"\stage\guide-link-created",
            (AccessMask)FileAccessMask.FILE_READ_DATA,
            0,
            ShareAccess.Read,
            CreateDisposition.FILE_OPEN,
            0,
            securityContext);
        Assert(createdOpenStatus == NTStatus.STATUS_SUCCESS, $"created symlink followed open should succeed, got {createdOpenStatus}");
        var createdReadStatus = store.ReadFile(out var createdData, createdFollowHandle, 0, 4096);
        Assert(createdReadStatus == NTStatus.STATUS_SUCCESS, $"created symlink followed read should succeed, got {createdReadStatus}");
        Assert(Encoding.UTF8.GetString(createdData).Contains("This file lives under docs/.", StringComparison.Ordinal), "created symlink followed read should return target content");
        store.CloseFile(createdFollowHandle);
    }

    private void ValidateGatewayProjectionErrorMapping()
    {
        var missingFileSystem = new GatewayProjectionFileSystem(
            "projection-missing",
            new ThrowingGatewayClient(new GatewaySemanticException("not-found", "path missing", null, HttpStatusCode.BadRequest)));
        AssertThrows<FileNotFoundException>(
            () => missingFileSystem.GetEntry(@"\stage\missing.txt"),
            "Gateway path not found");

        var deniedFileSystem = new GatewayProjectionFileSystem(
            "projection-denied",
            new ThrowingGatewayClient(new GatewaySemanticException("authentication-error", "bad token", null, HttpStatusCode.BadRequest)));
        AssertThrows<UnauthorizedAccessException>(
            () => deniedFileSystem.GetEntry(@"\stage\secret.txt"),
            "Gateway access denied");

        var failedFileSystem = new GatewayProjectionFileSystem(
            "projection-io",
            new ThrowingGatewayClient(new HttpRequestException("Connection refused")));
        AssertThrows<IOException>(
            () => failedFileSystem.GetEntry(@"\stage\unreachable.txt"),
            "Gateway request failed");

        var deniedWriteFileSystem = new GatewayProjectionFileSystem(
            "projection-write-denied",
            new ThrowingGatewayClient(new GatewaySemanticException("authentication-error", "bad token", null, HttpStatusCode.BadRequest)),
            enableStageWrites: true);
        AssertThrows<UnauthorizedAccessException>(
            () =>
            {
                using var stream = deniedWriteFileSystem.OpenFile(@"\stage\write-denied.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None);
                using var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false);
                writer.Write("denied\n");
            },
            "Gateway write denied");

        var missingWriteFileSystem = new GatewayProjectionFileSystem(
            "projection-write-missing",
            new ThrowingGatewayClient(new GatewaySemanticException("not-found", "parent missing", null, HttpStatusCode.BadRequest)),
            enableStageWrites: true);
        AssertThrows<FileNotFoundException>(
            () =>
            {
                using var stream = missingWriteFileSystem.OpenFile(@"\stage\write-missing.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None);
                using var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false);
                writer.Write("missing\n");
            },
            "Gateway write path not found");

        var failedWriteFileSystem = new GatewayProjectionFileSystem(
            "projection-write-io",
            new ThrowingGatewayClient(new HttpRequestException("Connection refused")),
            enableStageWrites: true);
        AssertThrows<IOException>(
            () =>
            {
                using var stream = failedWriteFileSystem.OpenFile(@"\stage\write-io.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None);
                using var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false);
                writer.Write("io\n");
            },
            "Gateway write request failed");
    }

    private void ValidateGatewayStageMutationFileSystem()
    {
        var gateway = new RecordingGatewayClient();
        var fileSystem = new GatewayProjectionFileSystem("projection-stage", gateway, enableStageWrites: true);
        const string initialText = "written through gateway\n";
        const string overwrittenText = "overwritten through gateway\n";

        using (var stream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.Write(initialText);
        }

        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","written.txt"]]""", StringComparison.Ordinal), "gateway stage mutation should call set with stage journal path");
        Assert(string.Equals(gateway.LastSetContent?["*file-system/file*"]?["content"]?["*type/byte-vector*"]?.GetValue<string>(), Convert.ToHexString(System.Text.Encoding.UTF8.GetBytes(initialText)).ToLowerInvariant(), StringComparison.Ordinal), "gateway stage mutation should send byte-vector payload");
        Assert(gateway.LastSetContent?["*file-system/file*"]?["meta"]?.ToJsonString() == """{"mode":420,"uid":1000,"gid":1001}""", "gateway stage mutation should preserve file metadata");

        using var readStream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var reader = new StreamReader(readStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var text = reader.ReadToEnd();
        Assert(text.Contains("written through gateway", StringComparison.Ordinal), "gateway stage mutation should update stage cache");

        using (var stream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.Write(overwrittenText);
        }

        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","written.txt"]]""", StringComparison.Ordinal), "gateway stage overwrite should keep the same stage journal path");
        Assert(string.Equals(gateway.LastSetContent?["*file-system/file*"]?["content"]?["*type/byte-vector*"]?.GetValue<string>(), Convert.ToHexString(System.Text.Encoding.UTF8.GetBytes(overwrittenText)).ToLowerInvariant(), StringComparison.Ordinal), "gateway stage overwrite should send updated byte-vector payload");
        Assert(gateway.LastSetContent?["*file-system/file*"]?["meta"]?.ToJsonString() == """{"mode":420,"uid":1000,"gid":1001}""", "gateway stage overwrite should preserve file metadata");

        using var overwrittenReadStream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var overwrittenReader = new StreamReader(overwrittenReadStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var overwrittenTextValue = overwrittenReader.ReadToEnd();
        Assert(string.Equals(overwrittenTextValue, overwrittenText, StringComparison.Ordinal), "gateway stage overwrite should update cached file contents");

        using (var stream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Append, FileAccess.Write, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.Write("and append\n");
        }

        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","written.txt"]]""", StringComparison.Ordinal), "gateway stage append should keep the same stage journal path");
        Assert(string.Equals(gateway.LastSetContent?["*file-system/file*"]?["content"]?["*type/byte-vector*"]?.GetValue<string>(), Convert.ToHexString(System.Text.Encoding.UTF8.GetBytes("overwritten through gateway\nand append\n")).ToLowerInvariant(), StringComparison.Ordinal), "gateway stage append should send appended byte-vector payload");
        Assert(gateway.LastSetContent?["*file-system/file*"]?["meta"]?.ToJsonString() == """{"mode":420,"uid":1000,"gid":1001}""", "gateway stage append should preserve file metadata");

        using var appendedReadStream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var appendedReader = new StreamReader(appendedReadStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var appendedTextValue = appendedReader.ReadToEnd();
        Assert(string.Equals(appendedTextValue, "overwritten through gateway\nand append\n", StringComparison.Ordinal), "gateway stage append should update cached file contents");

        using (var stream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Truncate, FileAccess.Write, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.Write("truncated\n");
        }

        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","written.txt"]]""", StringComparison.Ordinal), "gateway stage truncate should keep the same stage journal path");
        Assert(string.Equals(gateway.LastSetContent?["*file-system/file*"]?["content"]?["*type/byte-vector*"]?.GetValue<string>(), Convert.ToHexString(System.Text.Encoding.UTF8.GetBytes("truncated\n")).ToLowerInvariant(), StringComparison.Ordinal), "gateway stage truncate should send truncated byte-vector payload");
        Assert(gateway.LastSetContent?["*file-system/file*"]?["meta"]?.ToJsonString() == """{"mode":420,"uid":1000,"gid":1001}""", "gateway stage truncate should preserve file metadata");

        using var truncatedReadStream = fileSystem.OpenFile(@"\stage\written.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var truncatedReader = new StreamReader(truncatedReadStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var truncatedTextValue = truncatedReader.ReadToEnd();
        Assert(string.Equals(truncatedTextValue, "truncated\n", StringComparison.Ordinal), "gateway stage truncate should update cached file contents");

        using (var stream = fileSystem.OpenFile(@"\stage\created-by-open-or-create.txt", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.Write("open or create\n");
        }

        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","created-by-open-or-create.txt"]]""", StringComparison.Ordinal), "gateway stage open-or-create should create a missing stage file");
        Assert(string.Equals(gateway.LastSetContent?["*type/byte-vector*"]?.GetValue<string>(), Convert.ToHexString(System.Text.Encoding.UTF8.GetBytes("open or create\n")).ToLowerInvariant(), StringComparison.Ordinal), "gateway stage open-or-create should send created byte-vector payload without a file envelope");

        using var createdReadStream = fileSystem.OpenFile(@"\stage\created-by-open-or-create.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var createdReader = new StreamReader(createdReadStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var createdTextValue = createdReader.ReadToEnd();
        Assert(string.Equals(createdTextValue, "open or create\n", StringComparison.Ordinal), "gateway stage open-or-create should update cached file contents");

        AssertThrows<IOException>(
            () => fileSystem.OpenFile(@"\stage\created-by-open-or-create.txt", FileMode.CreateNew, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None),
            "Path already exists");
        AssertThrows<FileNotFoundException>(
            () => fileSystem.OpenFile(@"\stage\missing-open.txt", FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None),
            "missing-open.txt");

        using (var stream = fileSystem.OpenFile(@"\stage\docs\nested.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None))
        using (var writer = new StreamWriter(stream, new System.Text.UTF8Encoding(false), 1024, leaveOpen: false))
        {
            writer.Write("nested through gateway\n");
        }

        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","docs","nested.txt"]]""", StringComparison.Ordinal), "gateway stage nested write should call set with nested stage journal path");

        using var nestedReadStream = fileSystem.OpenFile(@"\stage\docs\nested.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var nestedReader = new StreamReader(nestedReadStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var nestedTextValue = nestedReader.ReadToEnd();
        Assert(string.Equals(nestedTextValue, "nested through gateway\n", StringComparison.Ordinal), "gateway stage nested write should update nested cached file contents");

        fileSystem.SetAttributes(@"\stage\written.txt", isHidden: null, isReadonly: true, isArchived: null);
        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","written.txt"]]""", StringComparison.Ordinal), "gateway stage file attribute mutation should keep the same journal path");
        Assert(gateway.LastSetContent?["*file-system/file*"]?["meta"]?.ToJsonString() == """{"mode":292,"uid":1000,"gid":1001}""", "gateway stage readonly attribute should clear file write bits while preserving uid/gid");

        var readonlyEntry = fileSystem.GetEntry(@"\stage\written.txt");
        Assert(readonlyEntry.IsReadonly, "gateway stage readonly attribute should update cached readonly flag");

        fileSystem.SetAttributes(@"\stage\written.txt", isHidden: null, isReadonly: false, isArchived: null);
        Assert(gateway.LastSetContent?["*file-system/file*"]?["meta"]?.ToJsonString() == """{"mode":420,"uid":1000,"gid":1001}""", "gateway stage readonly clear should restore writable file mode");

        fileSystem.SetDates(@"\stage\written.txt", DateTime.UtcNow, DateTime.UtcNow, DateTime.UtcNow);
        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","written.txt"]]""", StringComparison.Ordinal), "gateway stage SetDates should not emit a new gateway write");

        fileSystem.CreateDirectory(@"\stage\made-dir");
        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","made-dir","*directory*"]]""", StringComparison.Ordinal), "gateway stage mkdir should call set with stage directory-marker journal path");
        Assert(gateway.LastSetContent?.ToJsonString() == """{"*file-system/directory*":{}}""", "gateway stage mkdir should send directory-marker payload");
        var stageEntriesAfterMkdir = fileSystem.ListEntriesInDirectory(@"\stage");
        Assert(stageEntriesAfterMkdir.Any(entry => string.Equals(entry.Name, "made-dir", StringComparison.OrdinalIgnoreCase)), "gateway stage mkdir should add the directory to stage listings");
        Assert(!stageEntriesAfterMkdir.Any(entry => string.Equals(entry.Name, ".directory", StringComparison.OrdinalIgnoreCase)), "gateway stage directory listings should not expose .directory");
        AssertThrows<FileNotFoundException>(
            () => fileSystem.OpenFile(@"\stage\made-dir\.directory", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None),
            @".directory");

        AssertThrows<IOException>(
            () => fileSystem.Move(@"\stage\created-by-open-or-create.txt", @"\stage\written.txt"),
            "Destination already exists");
        fileSystem.Move(@"\stage\created-by-open-or-create.txt", @"\stage\created-by-open-or-create.txt");
        using var samePathReadStream = fileSystem.OpenFile(@"\stage\created-by-open-or-create.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var samePathReader = new StreamReader(samePathReadStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var samePathText = samePathReader.ReadToEnd();
        Assert(string.Equals(samePathText, "open or create\n", StringComparison.Ordinal), "gateway stage same-path move should be a no-op");
        AssertThrows<FileNotFoundException>(
            () => fileSystem.Move(@"\stage\missing-source.txt", @"\stage\missing-target.txt"),
            "missing-source.txt");

        AssertThrows<UnauthorizedAccessException>(
            () => fileSystem.OpenFile(@"\ledger\state\blocked.txt", FileMode.Create, FileAccess.ReadWrite, FileShare.ReadWrite, FileOptions.None),
            "Read-only projected path");

        fileSystem.Delete(@"\stage\written.txt");
        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","written.txt"]]""", StringComparison.Ordinal), "gateway stage delete should call set with stage journal path");
        Assert(gateway.LastSetContent?.ToJsonString() == """["nothing"]""", "gateway stage delete should send nothing payload");
        var stageEntries = fileSystem.ListEntriesInDirectory(@"\stage");
        Assert(!stageEntries.Any(entry => string.Equals(entry.Name, "written.txt", StringComparison.OrdinalIgnoreCase)), "gateway stage delete should remove the file from stage listings");
        AssertThrows<FileNotFoundException>(
            () => fileSystem.OpenFile(@"\stage\written.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None),
            @"\stage\written.txt");
        AssertThrows<FileNotFoundException>(
            () => fileSystem.Delete(@"\stage\written.txt"),
            @"\stage\written.txt");

        fileSystem.Delete(@"\stage\made-dir");
        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","made-dir","*directory*"]]""", StringComparison.Ordinal), "gateway stage rmdir should call set with stage directory-marker journal path");
        Assert(gateway.LastSetContent?.ToJsonString() == """["nothing"]""", "gateway stage rmdir should send nothing payload");
        var stageEntriesAfterRmdir = fileSystem.ListEntriesInDirectory(@"\stage");
        Assert(!stageEntriesAfterRmdir.Any(entry => string.Equals(entry.Name, "made-dir", StringComparison.OrdinalIgnoreCase)), "gateway stage rmdir should remove the directory from stage listings");

        fileSystem.CreateDirectory(@"\stage\made-dir");
        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","made-dir","*directory*"]]""", StringComparison.Ordinal), "gateway stage recreate-dir should call set with stage directory-marker journal path");
        Assert(gateway.LastSetContent?.ToJsonString() == """{"*file-system/directory*":{}}""", "gateway stage recreate-dir should send directory-marker payload");
        var stageEntriesAfterRecreate = fileSystem.ListEntriesInDirectory(@"\stage");
        Assert(stageEntriesAfterRecreate.Any(entry => string.Equals(entry.Name, "made-dir", StringComparison.OrdinalIgnoreCase)), "gateway stage recreate-dir should restore the directory to stage listings");

        fileSystem.Move(@"\stage\renamed-source.txt", @"\stage\renamed-target.txt");
        Assert(string.Equals(gateway.LastSetPathJson, """[["*state*","renamed-source.txt"]]""", StringComparison.Ordinal), "gateway stage move should delete the source path after writing destination");
        AssertThrows<FileNotFoundException>(
            () => fileSystem.OpenFile(@"\stage\renamed-source.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None),
            "renamed-source.txt");

        using var renamedTargetReadStream = fileSystem.OpenFile(@"\stage\renamed-target.txt", FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var renamedTargetReader = new StreamReader(renamedTargetReadStream, System.Text.Encoding.UTF8, leaveOpen: false);
        var renamedTargetText = renamedTargetReader.ReadToEnd();
        Assert(string.Equals(renamedTargetText, "rename source\n", StringComparison.Ordinal), "gateway stage move should materialize the destination contents");
    }

    private static void AssertCompiled(string json, string expectedPath)
    {
        using var document = JsonDocument.Parse(json);
        var actual = JournalPathMapper.CompileProjectedPath(document.RootElement);
        Assert(string.Equals(actual, expectedPath, StringComparison.Ordinal), $"expected compiled path {expectedPath}, got {actual}");
    }

    private static void AssertDecompiled(string projectedPath, string expectedJson)
    {
        Assert(JournalPathMapper.TryDecompileProjectedPath(projectedPath, out var actual), $"expected {projectedPath} to decompile");
        using var expectedDocument = JsonDocument.Parse(expectedJson);
        var expected = JsonSerializer.Serialize(expectedDocument.RootElement);
        var actualJson = JsonSerializer.Serialize(actual);
        Assert(string.Equals(actualJson, expected, StringComparison.Ordinal), $"expected decompiled path {expected}, got {actualJson}");
    }

    private static void AssertCompileFails(string json, string expectedMessageFragment)
    {
        using var document = JsonDocument.Parse(json);
        AssertThrows<InvalidDataException>(
            () => JournalPathMapper.CompileProjectedPath(document.RootElement),
            expectedMessageFragment);
    }

    private static void AssertThrows<TException>(Action action, string expectedMessageFragment)
        where TException : Exception
    {
        try
        {
            action();
        }
        catch (TException exception) when (exception.Message.Contains(expectedMessageFragment, StringComparison.Ordinal))
        {
            return;
        }
        catch (Exception exception)
        {
            throw new InvalidOperationException(
                $"Expected {typeof(TException).Name} containing '{expectedMessageFragment}', got {exception.GetType().Name}: {exception.Message}",
                exception);
        }

        throw new InvalidOperationException($"Expected {typeof(TException).Name} containing '{expectedMessageFragment}'.");
    }

    private static void Assert(bool condition, string message)
    {
        if (!condition)
        {
            throw new InvalidOperationException(message);
        }
    }

    private static byte[] BuildSymlinkReparseBufferForValidation(string targetPath)
    {
        var targetBytes = Encoding.Unicode.GetBytes(targetPath);
        var dataLength = 12 + targetBytes.Length * 2;
        var buffer = new byte[20 + targetBytes.Length * 2];
        WriteUInt32(buffer, 0, 0xA000000C);
        WriteUInt16(buffer, 4, (ushort)dataLength);
        WriteUInt16(buffer, 6, 0);
        WriteUInt16(buffer, 8, 0);
        WriteUInt16(buffer, 10, (ushort)targetBytes.Length);
        WriteUInt16(buffer, 12, (ushort)targetBytes.Length);
        WriteUInt16(buffer, 14, (ushort)targetBytes.Length);
        WriteUInt32(buffer, 16, 0);
        Buffer.BlockCopy(targetBytes, 0, buffer, 20, targetBytes.Length);
        Buffer.BlockCopy(targetBytes, 0, buffer, 20 + targetBytes.Length, targetBytes.Length);
        return buffer;
    }

    private static void WriteUInt16(byte[] buffer, int offset, ushort value)
    {
        buffer[offset] = (byte)(value & 0xFF);
        buffer[offset + 1] = (byte)(value >> 8);
    }

    private static void WriteUInt32(byte[] buffer, int offset, uint value)
    {
        buffer[offset] = (byte)(value & 0xFF);
        buffer[offset + 1] = (byte)((value >> 8) & 0xFF);
        buffer[offset + 2] = (byte)((value >> 16) & 0xFF);
        buffer[offset + 3] = (byte)(value >> 24);
    }

    private sealed class RecordingHttpMessageHandler : HttpMessageHandler
    {
        public string? LastRequestUri { get; private set; }

        public string? LastAuthorization { get; private set; }

        public JsonNode? LastBody { get; private set; }

        public HttpStatusCode NextStatusCode { get; set; } = HttpStatusCode.OK;

        public JsonNode? NextResponseBody { get; set; }

        public bool NextRespondWithEmptyBody { get; set; }

        protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
        {
            LastRequestUri = request.RequestUri?.ToString();
            LastAuthorization = request.Headers.Authorization?.ToString();

            var body = request.Content == null
                ? null
                : await request.Content.ReadAsStringAsync(cancellationToken);
            LastBody = string.IsNullOrWhiteSpace(body) ? null : JsonNode.Parse(body);

            var operation = request.RequestUri?.AbsolutePath.EndsWith("/set", StringComparison.OrdinalIgnoreCase) == true
                ? "set"
                : request.RequestUri?.AbsolutePath.EndsWith("/pin", StringComparison.OrdinalIgnoreCase) == true
                    ? "pin"
                    : request.RequestUri?.AbsolutePath.EndsWith("/unpin", StringComparison.OrdinalIgnoreCase) == true
                        ? "unpin"
                : request.RequestUri?.AbsolutePath.EndsWith("/size", StringComparison.OrdinalIgnoreCase) == true
                    ? "size"
                    : request.RequestUri?.AbsolutePath.EndsWith("/bridges", StringComparison.OrdinalIgnoreCase) == true
                        ? "bridges"
                        : request.RequestUri?.AbsolutePath.EndsWith("/interface/json", StringComparison.OrdinalIgnoreCase) == true
                            ? (LastBody?["function"]?.GetValue<string>() switch
                            {
                                "set!" => "set",
                                "pin!" => "pin",
                                "unpin!" => "unpin",
                                "size" => "size",
                                "bridges" => "bridges",
                                "general-batch!" => "batch",
                                _ => "get"
                            })
                        : "get";

            HttpContent content;
            if (NextRespondWithEmptyBody)
            {
                content = new StringContent(string.Empty, System.Text.Encoding.UTF8, "application/json");
            }
            else
            {
                JsonNode responseBody = NextResponseBody?.DeepClone() ?? operation switch
                {
                    "size" => JsonValue.Create(10L)!,
                    "bridges" => new JsonArray("alice", "bob"),
                    "pin" => JsonValue.Create(true)!,
                    "unpin" => JsonValue.Create(true)!,
                    _ => new JsonObject
                    {
                        ["ok"] = operation
                    }
                };

                if (NextStatusCode == HttpStatusCode.OK && responseBody is JsonObject obj)
                {
                    obj["ok"] ??= operation;
                }

                content = new StringContent(responseBody.ToJsonString(), System.Text.Encoding.UTF8, "application/json");
            }

            var response = new HttpResponseMessage(NextStatusCode)
            {
                Content = content
            };

            NextStatusCode = HttpStatusCode.OK;
            NextResponseBody = null;
            NextRespondWithEmptyBody = false;
            return response;
        }
    }

    private sealed class ThrowingGatewayClient : IGeneralInterfaceClient
    {
        private readonly Exception _exception;

        public ThrowingGatewayClient(Exception exception)
        {
            _exception = exception;
        }

        public Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken)
            => Task.FromException<JsonNode?>(_exception);

        public Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken)
            => Task.FromException<JsonNode?>(_exception);

        public Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken)
            => Task.FromException<JsonNode?>(_exception);

        public Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
            => Task.FromException<bool>(_exception);

        public Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
            => Task.FromException<bool>(_exception);

        public Task<long> SizeAsync(CancellationToken cancellationToken)
            => Task.FromResult(1L);

        public Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken)
            => Task.FromResult<IReadOnlyList<string>>(Array.Empty<string>());
    }

    private sealed class RecordingGatewayClient : IGeneralInterfaceClient
    {
        private readonly HashSet<string> _pinnedPaths = new(StringComparer.Ordinal);

        public RecordingGatewayClient()
        {
            _pinnedPaths.Add("""[-1,["*state*","written.txt"]]""");
            _pinnedPaths.Add("""[-1,["*state*","notes","todo.txt"]]""");
        }

        public string? LastSetPathJson { get; private set; }

        public JsonNode? LastSetContent { get; private set; }

        public string? LastPinPathJson { get; private set; }

        public string? LastUnpinPathJson { get; private set; }

        public Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken)
        {
            var pathJson = JsonSerializer.Serialize(request.Path);
            var pinned = _pinnedPaths.Contains(pathJson);
            return pathJson switch
            {
                """[["*state*"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = new JsonArray("directory", new JsonObject
                    {
                        ["docs"] = "directory",
                        ["written.txt"] = "value",
                    }, false),
                    ["pinned?"] = false,
                }),
                """[-1,["*state*"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = new JsonArray("directory", new JsonObject
                    {
                        ["docs"] = "directory",
                        ["written.txt"] = "value",
                    }, false),
                    ["pinned?"] = false,
                }),
                """[["*state*","docs"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = new JsonArray("directory", new JsonObject(), false),
                    ["pinned?"] = false,
                }),
                """[-1,["*state*","docs"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = new JsonArray("directory", new JsonObject(), false),
                    ["pinned?"] = pinned ? JsonNode.Parse("""[-1,["*state*","docs"]]""") : false,
                }),
                """[["*state*","renamed-source.txt"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = JsonFileSystemLoader.CreateByteFileContentNode(System.Text.Encoding.UTF8.GetBytes("rename source\n")),
                    ["pinned?"] = false,
                }),
                """[["*state*","written.txt"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = JsonFileSystemLoader.CreateByteFileContentNode(
                        System.Text.Encoding.UTF8.GetBytes("written through gateway\n"),
                        mode: 420,
                        uid: 1000,
                        gid: 1001),
                    ["pinned?"] = false,
                }),
                """[-1,["*state*","written.txt"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = JsonFileSystemLoader.CreateByteFileContentNode(
                        System.Text.Encoding.UTF8.GetBytes("written through gateway\n"),
                        mode: 420,
                        uid: 1000,
                        gid: 1001),
                    ["pinned?"] = pinned ? JsonNode.Parse("""[-1,["*state*","written.txt"]]""") : false,
                }),
                _ => Task.FromException<JsonNode?>(new FileNotFoundException(pathJson))
            };
        }

        public Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken)
        {
            LastSetPathJson = JsonSerializer.Serialize(request.Path);
            LastSetContent = request.Content.DeepClone();
            return Task.FromResult<JsonNode?>(request.Content.DeepClone());
        }

        public Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken)
        {
            var output = new JsonArray();
            foreach (var operation in request.Requests)
            {
                if (operation.Function == "get" && operation.Arguments is not null)
                {
                    var requestPath = JsonSerializer.Deserialize<object[]>(operation.Arguments["path"]!.ToJsonString())
                        ?? throw new InvalidDataException("Batch request path is invalid.");
                    var pinned = operation.Arguments["pinned?"]?.GetValue<bool>() ?? false;
                    var proof = operation.Arguments["proof?"]?.GetValue<bool>() ?? false;
                    output.Add(GetAsync(new GatewayGetRequest(requestPath, pinned, proof), cancellationToken).GetAwaiter().GetResult());
                    continue;
                }

                if (operation.Function != "set!" || operation.Arguments is null)
                {
                    return Task.FromException<JsonNode?>(new NotSupportedException());
                }

                var path = JsonSerializer.Deserialize<object[]>(operation.Arguments["path"]!.ToJsonString())
                    ?? throw new InvalidDataException("Batch request path is invalid.");
                var content = operation.Arguments["value"]?.DeepClone()
                    ?? throw new InvalidDataException("Batch request value is missing.");
                LastSetPathJson = JsonSerializer.Serialize(path);
                LastSetContent = content;
                output.Add(content.DeepClone());
            }

            return Task.FromResult<JsonNode?>(output);
        }

        public Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
        {
            LastPinPathJson = JsonSerializer.Serialize(request.Path);
            _pinnedPaths.Add(LastPinPathJson);
            return Task.FromResult(true);
        }

        public Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
        {
            LastUnpinPathJson = JsonSerializer.Serialize(request.Path);
            _pinnedPaths.Remove(LastUnpinPathJson);
            return Task.FromResult(true);
        }

        public Task<long> SizeAsync(CancellationToken cancellationToken)
            => Task.FromResult(1L);

        public Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken)
            => Task.FromResult<IReadOnlyList<string>>(Array.Empty<string>());
    }

    private sealed class ContextualPeerGatewayClient : IGeneralInterfaceClient
    {
        public Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken)
        {
            var pathJson = JsonSerializer.Serialize(request.Path);
            return pathJson switch
            {
                """[-1,["*peer*","journal-7","chain"],-1,["*peer*"]]""" => Task.FromResult<JsonNode?>(new JsonObject
                {
                    ["content"] = new JsonArray("directory", new JsonObject
                    {
                        ["journal-2"] = "directory",
                        ["journal-3"] = "directory",
                    }, false),
                    ["pinned?"] = false,
                }),
                _ => Task.FromException<JsonNode?>(new FileNotFoundException(pathJson))
            };
        }

        public Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken)
            => Task.FromException<JsonNode?>(new NotSupportedException());

        public Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken)
        {
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

                return Task.FromException<JsonNode?>(new NotSupportedException());
            }

            return Task.FromResult<JsonNode?>(output);
        }

        public Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
            => Task.FromException<bool>(new NotSupportedException());

        public Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
            => Task.FromException<bool>(new NotSupportedException());

        public Task<long> SizeAsync(CancellationToken cancellationToken)
            => Task.FromResult(1L);

        public Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken)
            => Task.FromResult<IReadOnlyList<string>>(new[] { "journal-4", "journal-7" });
    }
}
