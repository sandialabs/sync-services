using System.Text.Json;
using System.Text.Json.Nodes;

namespace FileSystem.Server;

public sealed record GatewayGetRequest(IReadOnlyList<object> Path, bool Pinned, bool Proof);

public sealed record GatewaySetRequest(IReadOnlyList<object> Path, JsonNode Content);

public sealed record GatewayPinRequest(IReadOnlyList<object> Path);

public sealed record GatewayBatchRequest(IReadOnlyList<GatewayBatchOperation> Requests);

public sealed record GatewayBatchOperation(string Function, JsonObject? Arguments);

public interface IGeneralInterfaceClient
{
    Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken);

    Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken);

    Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken);

    Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken);

    Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken);

    Task<long> SizeAsync(CancellationToken cancellationToken);

    Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken);
}

public sealed class MockGatewayClient : IGeneralInterfaceClient
{
    private readonly string _fixturePath;
    private readonly ILogger _logger;
    private readonly object _gate = new();

    public MockGatewayClient(string fixturePath, ILogger logger)
    {
        _fixturePath = fixturePath;
        _logger = logger;
    }

    public InMemoryFileSystem CreateProjectionFileSystem(string name)
    {
        lock (_gate)
        {
            _logger.LogInformation("loading mock gateway fixture from {JsonFixturePath}", _fixturePath);
            var entries = JsonFileSystemLoader.LoadEntriesFromFile(_fixturePath);
            return JsonFileSystemLoader.CreateFileSystem(
                entries,
                name,
                fileSystem =>
                {
                    lock (_gate)
                    {
                        var updatedEntries = JsonFileSystemLoader.ExportEntries(fileSystem);
                        JsonFileSystemLoader.SaveEntriesToFile(updatedEntries, _fixturePath);
                    }
                });
        }
    }

    public Task<JsonNode?> GetAsync(GatewayGetRequest request, CancellationToken cancellationToken)
    {
        lock (_gate)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var sharePath = ResolveSharePathForGet(request.Path);
            var entries = JsonFileSystemLoader.LoadEntriesFromFile(_fixturePath);
            var entry = entries.SingleOrDefault(candidate =>
                string.Equals(candidate.SharePath, sharePath, StringComparison.OrdinalIgnoreCase));

            if (entry == null)
            {
                throw new FileNotFoundException($"Gateway path not found: {sharePath}");
            }

            var value = JsonFileSystemLoader.BuildGatewayValue(entry, entries, details: request.Pinned || request.Proof);
            _logger.LogDebug(
                "mock gateway get path={SharePath} pinned={Pinned} proof={Proof}",
                sharePath,
                request.Pinned,
                request.Proof);
            return Task.FromResult<JsonNode?>(value);
        }
    }

    public Task<JsonNode?> SetAsync(GatewaySetRequest request, CancellationToken cancellationToken)
    {
        lock (_gate)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var sharePath = CompileSharePath(request.Path);
            if (!JournalPathMapper.IsWritableProjectedPath(sharePath))
            {
                throw new UnauthorizedAccessException($"Read-only projected path: {sharePath}");
            }

            var entries = JsonFileSystemLoader.LoadEntriesFromFile(_fixturePath)
                .Where(entry => !string.Equals(entry.SharePath, sharePath, StringComparison.OrdinalIgnoreCase))
                .ToList();
            entries.Add(JsonFileSystemLoader.CreateEntryFromContent(sharePath, request.Content, pinned: false));
            JsonFileSystemLoader.SaveEntriesToFile(entries, _fixturePath);

            _logger.LogDebug("mock gateway set! path={SharePath}", sharePath);
            return Task.FromResult<JsonNode?>(request.Content.DeepClone());
        }
    }

    public Task<JsonNode?> BatchAsync(GatewayBatchRequest request, CancellationToken cancellationToken)
    {
        lock (_gate)
        {
            cancellationToken.ThrowIfCancellationRequested();

            JsonArray? output = null;
            foreach (var operation in request.Requests)
            {
                switch (operation.Function)
                {
                    case "get":
                    case "resolve":
                    {
                        var arguments = operation.Arguments
                            ?? throw new InvalidDataException("Batch get requires arguments.");
                        var pathNode = arguments["path"]
                            ?? throw new InvalidDataException("Batch get requires path.");
                        var pinned = arguments["pinned?"]?.GetValue<bool>() ?? false;
                        var proof = arguments["proof?"]?.GetValue<bool>() ?? false;

                        var path = JsonSerializer.Deserialize<object[]>(pathNode.ToJsonString())
                            ?? throw new InvalidDataException("Batch get path is invalid.");
                        var result = GetAsync(
                            new GatewayGetRequest(path, pinned, proof),
                            cancellationToken).GetAwaiter().GetResult();

                        output ??= new JsonArray();
                        output.Add(result?.DeepClone());
                        break;
                    }
                    case "set!":
                    {
                        var arguments = operation.Arguments
                            ?? throw new InvalidDataException("Batch set! requires arguments.");
                        var pathNode = arguments["path"]
                            ?? throw new InvalidDataException("Batch set! requires path.");
                        var contentNode = arguments["value"]
                            ?? throw new InvalidDataException("Batch set! requires value.");

                        var path = JsonSerializer.Deserialize<object[]>(pathNode.ToJsonString())
                            ?? throw new InvalidDataException("Batch set! path is invalid.");
                        var result = SetAsync(
                            new GatewaySetRequest(path, contentNode.DeepClone()),
                            cancellationToken).GetAwaiter().GetResult();

                        output ??= new JsonArray();
                        output.Add(result?.DeepClone());
                        break;
                    }
                    default:
                        throw new NotSupportedException($"Mock batch does not support function '{operation.Function}'.");
                }
            }

            _logger.LogDebug("mock gateway general-batch requests={Count}", request.Requests.Count);
            return Task.FromResult<JsonNode?>(output ?? new JsonArray());
        }
    }

    public Task<bool> PinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
    {
        return UpdatePinnedStateAsync(request.Path, pinned: true, cancellationToken);
    }

    public Task<bool> UnpinAsync(GatewayPinRequest request, CancellationToken cancellationToken)
    {
        return UpdatePinnedStateAsync(request.Path, pinned: false, cancellationToken);
    }

    public Task<long> SizeAsync(CancellationToken cancellationToken)
    {
        lock (_gate)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var maxIndex = 0;
            foreach (var entry in JsonFileSystemLoader.LoadEntriesFromFile(_fixturePath))
            {
                if (!JournalPathMapper.TryDecompileProjectedPath(entry.SharePath, out var parts) ||
                    parts.Count == 0 ||
                    parts[0] is not int index)
                {
                    continue;
                }

                maxIndex = Math.Max(maxIndex, index);
            }

            return Task.FromResult((long)(maxIndex + 1));
        }
    }

    public Task<IReadOnlyList<string>> BridgesAsync(CancellationToken cancellationToken)
    {
        lock (_gate)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var bridges = JsonFileSystemLoader.LoadEntriesFromFile(_fixturePath)
                .Select(entry => entry.SharePath.Trim('\\').Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries))
                .Where(segments => segments.Length >= 3 &&
                    string.Equals(segments[0], "ledger", StringComparison.OrdinalIgnoreCase) &&
                    string.Equals(segments[1], "bridge", StringComparison.OrdinalIgnoreCase))
                .Select(segments => segments[2])
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .OrderBy(name => name, StringComparer.OrdinalIgnoreCase)
                .ToArray();
            return Task.FromResult<IReadOnlyList<string>>(bridges);
        }
    }

    private static string CompileSharePath(IReadOnlyList<object> path)
    {
        var json = JsonSerializer.Serialize(path);
        using var document = JsonDocument.Parse(json);
        return JournalPathMapper.CompileProjectedPath(document.RootElement);
    }

    private string ResolveSharePathForGet(IReadOnlyList<object> path)
    {
        var compiled = CompileSharePath(path);
        var entries = JsonFileSystemLoader.LoadEntriesFromFile(_fixturePath);
        if (entries.Any(entry => string.Equals(entry.SharePath, compiled, StringComparison.OrdinalIgnoreCase)))
        {
            return compiled;
        }

        if (LooksLikeCurrentLedgerState(path))
        {
            var stateBlock = (object[])path.Last();
            var stagePath = new List<object>
            {
                new object[] { "*state*" }.Concat(stateBlock.Skip(1).Cast<object>()).ToArray()
            };

            var stageCompiled = CompileSharePath(stagePath);
            if (entries.Any(entry => string.Equals(entry.SharePath, stageCompiled, StringComparison.OrdinalIgnoreCase)))
            {
                return stageCompiled;
            }
        }

        var normalizedPath = NormalizeCurrentBridgePath(path);
        if (!ReferenceEquals(normalizedPath, path))
        {
            var normalizedCompiled = CompileSharePath(normalizedPath);
            if (entries.Any(entry => string.Equals(entry.SharePath, normalizedCompiled, StringComparison.OrdinalIgnoreCase)))
            {
                return normalizedCompiled;
            }
        }

        return compiled;
    }

    private Task<bool> UpdatePinnedStateAsync(IReadOnlyList<object> path, bool pinned, CancellationToken cancellationToken)
    {
        lock (_gate)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var sharePath = ResolveSharePathForGet(path);
            var entries = JsonFileSystemLoader.LoadEntriesFromFile(_fixturePath);
            var index = entries.FindIndex(entry => string.Equals(entry.SharePath, sharePath, StringComparison.OrdinalIgnoreCase));
            if (index < 0)
            {
                throw new FileNotFoundException($"Gateway path not found: {sharePath}");
            }

            var entry = entries[index];
            entries[index] = entry with { Pinned = pinned };
            JsonFileSystemLoader.SaveEntriesToFile(entries, _fixturePath);
            _logger.LogDebug("mock gateway {Operation} path={SharePath}", pinned ? "pin!" : "unpin!", sharePath);
            return Task.FromResult(true);
        }
    }

    private static bool LooksLikeCurrentLedgerState(IReadOnlyList<object> path)
    {
        if (path.Count < 2 || path[0] is not int)
        {
            return false;
        }

        for (var index = 1; index < path.Count - 1; index++)
        {
            if (path[index] is int)
            {
                return false;
            }
        }

        if (path[^1] is not object[] stateBlock || stateBlock.Length == 0)
        {
            return false;
        }

        return string.Equals((string)stateBlock[0], "*state*", StringComparison.Ordinal);
    }

    private static IReadOnlyList<object> NormalizeCurrentBridgePath(IReadOnlyList<object> path)
    {
        if (path.Count == 0)
        {
            return path;
        }

        var normalized = new List<object>();
        var changed = false;
        for (var index = 0; index < path.Count; index++)
        {
            var current = path[index];
            if (current is int intValue &&
                intValue == -1 &&
                index > 0 &&
                path[index - 1] is object[] previousBlock &&
                previousBlock.Length >= 3 &&
                string.Equals(previousBlock[0] as string, "*bridge*", StringComparison.Ordinal) &&
                string.Equals(previousBlock[2] as string, "chain", StringComparison.Ordinal))
            {
                changed = true;
                continue;
            }

            normalized.Add(current);
        }

        return changed ? normalized : path;
    }
}
