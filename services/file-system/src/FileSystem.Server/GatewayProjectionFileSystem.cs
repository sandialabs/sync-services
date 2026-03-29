using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using DiskAccessLibrary.FileSystems.Abstractions;

namespace FileSystem.Server;

public sealed class GatewayProjectionFileSystem : IFileSystem, ISymlinkAwareFileSystem
{
    private readonly string _name;
    private readonly IGeneralInterfaceClient _gateway;
    private readonly object _gate = new();
    private readonly InMemoryFileSystem _cache;
    private readonly HashSet<string> _hydratedPaths = new(StringComparer.OrdinalIgnoreCase);
    private readonly HashSet<string> _hydratedDirectoryControlPaths = new(StringComparer.OrdinalIgnoreCase);
    private readonly HashSet<string> _deletedPaths = new(StringComparer.OrdinalIgnoreCase);
    private readonly HashSet<string> _markerBackedDirectories = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, bool> _discoveredPinStates = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, int> _pendingWritableStageFiles = new(StringComparer.OrdinalIgnoreCase);
    private readonly bool _enableStageWrites;

    public GatewayProjectionFileSystem(string name, IGeneralInterfaceClient gateway, bool enableStageWrites = false)
    {
        _name = name;
        _gateway = gateway;
        _enableStageWrites = enableStageWrites;
        _cache = new InMemoryFileSystem($"{name}-cache", isWritablePath: _ => false);
        SeedSyntheticDirectory(@"\stage");
        SeedSyntheticDirectory(@"\ledger");
        SeedChainNode(@"\ledger");
        SeedSyntheticDirectory(@"\control");
        _cache.SeedFile(@"\control\pin", Array.Empty<byte>());
    }

    public string Name => _name;

    public long Size => 64L * 1024L * 1024L;

    public long FreeSpace => Size / 2;

    public bool SupportsNamedStreams => false;

    public FileSystemEntry GetEntry(string path)
    {
        var normalized = NormalizePath(path);
        TraceOperation($"GetEntry path={normalized}");
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            return ExecuteRead(path, () => BuildPinControlFileEntry());
        }

        if (info.Kind == ProjectedPathKind.ControlSyntheticRoot)
        {
            return ExecuteRead(path, () => _cache.GetEntry(normalized));
        }

        return ExecuteRead(path, () =>
        {
            lock (_gate)
            {
                EnsurePathMaterialized(path);
                return _cache.GetEntry(path);
            }
        });
    }

    public FileSystemEntry CreateFile(string path)
    {
        var normalized = NormalizePath(path);
        TraceOperation($"CreateFile path={normalized}");
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            return ExecuteWrite(path, () => BuildPinControlFileEntry());
        }

        RejectSyntheticDirectoryControlMutation(normalized);
        if (!CanWriteStagePath(path))
        {
            throw CreateReadOnlyPathException(path);
        }

        lock (_gate)
        {
            EnsureParentDirectoryMaterialized(path);
            _deletedPaths.Remove(normalized);
            _cache.SeedFile(path, Array.Empty<byte>());
            _hydratedPaths.Add(normalized);
            return _cache.GetEntry(path);
        }
    }

    public FileSystemEntry CreateDirectory(string path)
    {
        var normalized = NormalizePath(path);
        TraceOperation($"CreateDirectory path={normalized}");
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            throw new NotSupportedException("The pin control path is a file.");
        }

        RejectSyntheticDirectoryControlMutation(normalized);
        if (!CanWriteStagePath(path))
        {
            throw CreateReadOnlyPathException(path);
        }

        if (info.Kind != ProjectedPathKind.StageContent || info.JournalPath == null)
        {
            throw CreateReadOnlyPathException(path);
        }
        var markerJournalPath = GetDirectoryMarkerJournalPath(info.JournalPath);

        lock (_gate)
        {
            ExecuteWrite(normalized, () => EnsureParentDirectoryMaterialized(normalized));
            var wasDeleted = _deletedPaths.Contains(normalized);
            if (!wasDeleted && CacheEntryExists(normalized))
            {
                var existing = _cache.GetEntry(normalized);
                var isDirectory = existing.GetType().GetProperty("IsDirectory")?.GetValue(existing);
                if (isDirectory is bool trueValue && trueValue)
                {
                    _deletedPaths.Remove(normalized);
                    _hydratedPaths.Add(normalized);
                    return existing;
                }

                throw new IOException($"Path already exists: {normalized}");
            }

            ExecuteWrite(normalized, () =>
            {
                _gateway.SetAsync(new GatewaySetRequest(markerJournalPath, JsonFileSystemLoader.CreateDirectoryMarkerContentNode()), CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
            });
            _deletedPaths.Remove(normalized);
            _markerBackedDirectories.Add(normalized);
            _hydratedDirectoryControlPaths.Add(normalized);
            _cache.SeedDirectory(
                normalized,
                metadata: null,
                control: null);
            _hydratedPaths.Add(normalized);
            return _cache.GetEntry(normalized);
        }
    }

    public void Move(string source, string destination)
    {
        var normalizedSource = NormalizePath(source);
        var normalizedDestination = NormalizePath(destination);
        TraceOperation($"Move source={normalizedSource} destination={normalizedDestination}");
        var sourceInfo = ParsePath(normalizedSource);
        var destinationInfo = ParsePath(normalizedDestination);
        if (sourceInfo.Kind == ProjectedPathKind.ControlPinFile || destinationInfo.Kind == ProjectedPathKind.ControlPinFile)
        {
            throw new NotSupportedException("Pin control paths do not support rename.");
        }

        RejectSyntheticDirectoryControlMutation(normalizedSource);
        RejectSyntheticDirectoryControlMutation(normalizedDestination);
        if (!CanWriteStagePath(source) || !CanWriteStagePath(destination))
        {
            throw CreateReadOnlyPathException(source);
        }

        if (sourceInfo.Kind != ProjectedPathKind.StageContent ||
            destinationInfo.Kind != ProjectedPathKind.StageContent ||
            sourceInfo.JournalPath == null ||
            destinationInfo.JournalPath == null)
        {
            throw CreateReadOnlyPathException(source);
        }

        if (string.Equals(normalizedSource, normalizedDestination, StringComparison.OrdinalIgnoreCase))
        {
            return;
        }

        lock (_gate)
        {
            if (_deletedPaths.Contains(normalizedSource))
            {
                throw new FileNotFoundException(normalizedSource);
            }

            ExecuteWrite(normalizedSource, () => EnsurePathMaterialized(normalizedSource));
            ExecuteWrite(normalizedDestination, () => EnsureParentDirectoryMaterialized(normalizedDestination));
            if (_deletedPaths.Contains(normalizedDestination))
            {
                _deletedPaths.Remove(normalizedDestination);
            }
            else if (CacheEntryExists(normalizedDestination))
            {
                throw new IOException($"Destination already exists: {normalizedDestination}");
            }

            var contentNode = BuildStageContentNode(normalizedSource);
            ExecuteWrite(normalizedDestination, () =>
            {
                _gateway.BatchAsync(
                        new GatewayBatchRequest(
                            new[]
                            {
                                new GatewayBatchOperation(
                                    "set!",
                                    new JsonObject
                                    {
                                        ["path"] = JsonSerializer.SerializeToNode(destinationInfo.JournalPath),
                                        ["value"] = contentNode.DeepClone(),
                                    }),
                                new GatewayBatchOperation(
                                    "set!",
                                    new JsonObject
                                    {
                                        ["path"] = JsonSerializer.SerializeToNode(sourceInfo.JournalPath),
                                        ["value"] = JsonFileSystemLoader.CreateNothingContentNode(),
                                    }),
                            }),
                        CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
            });

            _deletedPaths.Add(normalizedSource);
            _deletedPaths.Remove(normalizedDestination);
            _hydratedPaths.Remove(normalizedSource);

            if (contentNode is JsonArray)
            {
                _cache.SeedDirectory(normalizedDestination);
            }
            else
            {
                _cache.SeedFile(normalizedDestination, ReadCacheFileBytes(normalizedSource));
            }

            _hydratedPaths.Add(normalizedDestination);
        }
    }

    public void Delete(string path)
    {
        var normalized = NormalizePath(path);
        TraceOperation($"Delete path={normalized}");
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            throw new NotSupportedException("The pin control file cannot be deleted.");
        }

        RejectSyntheticDirectoryControlMutation(normalized);
        if (!CanWriteStagePath(path))
        {
            throw CreateReadOnlyPathException(path);
        }

        if (info.Kind != ProjectedPathKind.StageContent || info.JournalPath == null)
        {
            throw CreateReadOnlyPathException(path);
        }

        lock (_gate)
        {
            if (_deletedPaths.Contains(normalized))
            {
                throw new FileNotFoundException(normalized);
            }

            ExecuteWrite(normalized, () => EnsurePathMaterialized(normalized));
            var deleteJournalPath = _markerBackedDirectories.Contains(normalized)
                ? GetDirectoryMarkerJournalPath(info.JournalPath)
                : info.JournalPath;
            ExecuteWrite(normalized, () =>
            {
                _gateway.SetAsync(new GatewaySetRequest(deleteJournalPath, JsonFileSystemLoader.CreateNothingContentNode()), CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
            });
            _deletedPaths.Add(normalized);
            _markerBackedDirectories.Remove(normalized);
            _hydratedDirectoryControlPaths.Remove(normalized);
            _hydratedPaths.Remove(normalized);
        }
    }

    public List<FileSystemEntry> ListEntriesInDirectory(string path)
    {
        TraceOperation($"ListEntriesInDirectory path={NormalizePath(path)}");
        var normalized = NormalizePath(path);
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            return ExecuteRead(path, () => new List<FileSystemEntry> { BuildPinControlFileEntry() });
        }

        if (info.Kind == ProjectedPathKind.ControlSyntheticRoot)
        {
            return ExecuteRead(path, () => _cache.ListEntriesInDirectory(normalized));
        }

        return ExecuteRead(path, () =>
        {
            lock (_gate)
            {
                EnsureDirectoryMaterialized(path);
                return _cache.ListEntriesInDirectory(path)
                    .Where(entry => !IsDeletedChild(normalized, entry.Name))
                    .ToList();
            }
        });
    }

    public List<KeyValuePair<string, ulong>> ListDataStreams(string path)
    {
        return new List<KeyValuePair<string, ulong>>();
    }

    public bool TryGetSymlink(string path, out SymlinkEntryInfo symlink)
    {
        lock (_gate)
        {
            try
            {
                EnsurePathMaterialized(path);
            }
            catch
            {
                symlink = null!;
                return false;
            }

            return _cache.TryGetSymlink(path, out symlink);
        }
    }

    public void CreateOrUpdateSymlink(string path, string journalTargetJson, string projectedTargetPath)
    {
        var normalized = NormalizePath(path);
        if (!CanWriteStagePath(normalized))
        {
            throw CreateReadOnlyPathException(normalized);
        }

        var info = ParsePath(normalized);
        if (info.Kind != ProjectedPathKind.StageContent || info.JournalPath == null)
        {
            throw CreateReadOnlyPathException(normalized);
        }

        lock (_gate)
        {
            ExecuteWrite(normalized, () => EnsureParentDirectoryMaterialized(normalized));
            var contentNode = JsonFileSystemLoader.CreateSymlinkContentNode(journalTargetJson);
            ExecuteWrite(normalized, () =>
            {
                _gateway.SetAsync(new GatewaySetRequest(info.JournalPath, contentNode), CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
            });
            _deletedPaths.Remove(normalized);
            _cache.CreateOrUpdateSymlink(normalized, journalTargetJson, projectedTargetPath);
            _hydratedPaths.Add(normalized);
        }
    }

    public Stream OpenFile(string path, FileMode mode, FileAccess access, FileShare share, FileOptions options)
    {
        var normalized = NormalizePath(path);
        TraceOperation($"OpenFile path={normalized} mode={mode} access={access} share={share} options={options}");
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            if (access == FileAccess.Read && mode == FileMode.Open)
            {
                return ExecuteRead(path, () => new MemoryStream(RenderPinControlFileBytes(), writable: false));
            }

            if (access != FileAccess.Read &&
                mode is FileMode.Open or FileMode.Create or FileMode.CreateNew or FileMode.OpenOrCreate or FileMode.Truncate or FileMode.Append)
            {
                return ExecuteWrite(path, () =>
                    new GatewayStageWriteStream(
                        mode == FileMode.Append ? RenderPinControlFileBytes() : Array.Empty<byte>(),
                        append: mode == FileMode.Append,
                        onCommit: ReplacePinnedSet,
                        onClosed: static () => { }));
            }

            throw new NotSupportedException("Unsupported control pin file operation.");
        }

        if (CanWriteStagePath(path) && (access != FileAccess.Read || mode != FileMode.Open))
        {
            return OpenWritableStageFile(path, mode);
        }

        if (mode != FileMode.Open || access != FileAccess.Read)
        {
            throw CreateReadOnlyPathException(path);
        }

        return ExecuteRead(path, () =>
        {
            lock (_gate)
            {
                EnsurePathMaterialized(path);
                return _cache.OpenFile(path, FileMode.Open, FileAccess.Read, share, options);
            }
        });
    }

    private Stream OpenWritableStageFile(string path, FileMode mode)
    {
        var normalized = NormalizePath(path);
        TraceOperation($"OpenWritableStageFile path={normalized} mode={mode}");
        var info = ParsePath(normalized);
        if (info.Kind != ProjectedPathKind.StageContent || info.JournalPath == null)
        {
            throw CreateReadOnlyPathException(path);
        }

        var journalPath = info.JournalPath;

        lock (_gate)
        {
            if (_deletedPaths.Contains(normalized))
            {
                if (mode == FileMode.Open)
                {
                    throw new FileNotFoundException(normalized);
                }

                _deletedPaths.Remove(normalized);
            }

            ExecuteWrite(normalized, () => EnsureExistingPathMaterializedForWriteBatched(normalized, info, mode));
            ExecuteWrite(normalized, () => EnsureParentDirectoryMaterialized(normalized));
            if (mode == FileMode.CreateNew && CacheEntryExists(normalized))
            {
                throw new IOException($"Path already exists: {normalized}");
            }
            ExecuteWrite(normalized, () => EnsureExistingPathMaterializedForWrite(normalized, mode));
            var existingControl = TryGetCachedControl(normalized);
            byte[] initialContent = Array.Empty<byte>();
            ExecuteWrite(normalized, () => initialContent = GetInitialStageContent(normalized, mode));
            _deletedPaths.Remove(normalized);
            _cache.SeedFile(normalized, initialContent, metadata: null, control: ToWritableFileControl(existingControl));
            _hydratedPaths.Add(normalized);
            TrackPendingWritableStageFile(normalized);
            return new GatewayStageWriteStream(
                initialContent,
                append: mode == FileMode.Append,
                onCommit: bytes => CommitStageFile(normalized, journalPath, bytes),
                onClosed: () => UntrackPendingWritableStageFile(normalized));
        }
    }

    public void SetAttributes(string path, bool? isHidden, bool? isReadonly, bool? isArchived)
    {
        TraceOperation($"SetAttributes path={NormalizePath(path)} hidden={isHidden?.ToString() ?? "null"} readonly={isReadonly?.ToString() ?? "null"} archived={isArchived?.ToString() ?? "null"}");
        var normalized = NormalizePath(path);
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            return;
        }

        RejectSyntheticDirectoryControlMutation(normalized);
        if (!CanWriteStagePath(normalized))
        {
            throw CreateReadOnlyPathException(path);
        }

        if (info.Kind != ProjectedPathKind.StageContent || info.JournalPath == null)
        {
            throw CreateReadOnlyPathException(path);
        }

        lock (_gate)
        {
            ExecuteWrite(normalized, () => EnsurePathMaterialized(normalized));
            var entry = _cache.GetEntry(normalized);
            var existingControl = TryGetCachedControl(normalized);
            var updatedMode = existingControl?.Mode;
            if (isReadonly.HasValue)
            {
                updatedMode = ApplyReadonlyToMode(updatedMode, isReadonly.Value, entry.IsDirectory);
            }

            if (entry.IsDirectory)
            {
                if (!TryGetDirectoryMarkerJournalPathForDirectory(normalized, out var markerJournalPath))
                {
                    throw CreateReadOnlyPathException(path);
                }

                var directoryContentNode = JsonFileSystemLoader.CreateDirectoryMarkerContentNode(updatedMode, existingControl?.Uid, existingControl?.Gid);
                ExecuteWrite(normalized, () =>
                {
                    _gateway.SetAsync(new GatewaySetRequest(markerJournalPath, directoryContentNode), CancellationToken.None)
                        .GetAwaiter()
                        .GetResult();
                });
                _cache.SeedDirectory(
                    normalized,
                    metadata: null,
                    control: new InMemoryFileSystem.SnapshotControl(
                        existingControl?.Pinned,
                        null,
                        updatedMode,
                        existingControl?.Uid,
                        existingControl?.Gid));
                _markerBackedDirectories.Add(normalized);
                _hydratedPaths.Add(normalized);
                return;
            }

            var existingMetadata = TryGetCachedMetadata(normalized);
            var updatedMetadata = new InMemoryFileSystem.SnapshotMetadata(
                isHidden ?? existingMetadata?.IsHidden,
                isReadonly ?? existingMetadata?.IsReadonly,
                isArchived ?? existingMetadata?.IsArchived,
                existingMetadata?.CreationTimeUtc,
                existingMetadata?.LastWriteTimeUtc,
                existingMetadata?.LastAccessTimeUtc);
            _cache.SeedFile(
                normalized,
                ReadCacheFileBytes(normalized),
                metadata: updatedMetadata,
                control: new InMemoryFileSystem.SnapshotControl(
                    existingControl?.Pinned,
                    existingControl?.ContentKind ?? "bytes",
                    updatedMode,
                    existingControl?.Uid,
                    existingControl?.Gid));

            if (IsPendingWritableStageFile(normalized))
            {
                _deletedPaths.Remove(normalized);
                _hydratedPaths.Add(normalized);
                return;
            }

            var contentNode = BuildStageContentNode(normalized);
            ExecuteWrite(normalized, () =>
            {
                _gateway.SetAsync(new GatewaySetRequest(info.JournalPath, contentNode), CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
            });
            _deletedPaths.Remove(normalized);
            _hydratedPaths.Add(normalized);
        }
    }

    public void SetDates(string path, DateTime? creationDT, DateTime? lastWriteDT, DateTime? lastAccessDT)
    {
        TraceOperation($"SetDates path={NormalizePath(path)} creation={creationDT?.ToString("O") ?? "null"} write={lastWriteDT?.ToString("O") ?? "null"} access={lastAccessDT?.ToString("O") ?? "null"}");
        var normalized = NormalizePath(path);
        var info = ParsePath(normalized);
        if (info.Kind == ProjectedPathKind.ControlPinFile)
        {
            return;
        }

        if (!CanWriteStagePath(normalized))
        {
            throw CreateReadOnlyPathException(path);
        }

        lock (_gate)
        {
            ExecuteWrite(normalized, () => EnsurePathMaterialized(normalized));
        }
    }

    private void EnsurePathMaterialized(string path)
    {
        var normalized = NormalizePath(path);
        if (_deletedPaths.Contains(normalized))
        {
            throw new FileNotFoundException(normalized);
        }

        var info = ParsePath(normalized);
        if (info.Kind is ProjectedPathKind.StageContent or ProjectedPathKind.LedgerStateContent)
        {
            if (_hydratedPaths.Contains(normalized))
            {
                return;
            }
        }
        else if (CacheEntryExists(normalized))
        {
            return;
        }

        switch (info.Kind)
        {
            case ProjectedPathKind.Root:
            case ProjectedPathKind.StageSyntheticRoot:
            case ProjectedPathKind.ControlPinFile:
            case ProjectedPathKind.ControlSyntheticRoot:
            case ProjectedPathKind.LedgerSyntheticRoot:
            case ProjectedPathKind.LedgerNodeContainer:
            case ProjectedPathKind.LedgerPreviousContainer:
            case ProjectedPathKind.LedgerNodeRoot:
                EnsureDirectoryMaterialized(normalized);
                return;
            case ProjectedPathKind.StageContent:
            case ProjectedPathKind.LedgerStateContent:
                MaterializeContentPath(info, normalized);
                return;
            default:
                throw new FileNotFoundException(normalized);
        }
    }

    private void EnsureDirectoryMaterialized(string path)
    {
        var normalized = NormalizePath(path);
        var info = ParsePath(normalized);

        switch (info.Kind)
        {
            case ProjectedPathKind.Root:
                SeedSyntheticDirectory(@"\stage");
                SeedSyntheticDirectory(@"\ledger");
                SeedSyntheticDirectory(@"\control");
                return;
            case ProjectedPathKind.ControlSyntheticRoot:
                SeedSyntheticDirectory(@"\control");
                _cache.SeedFile(@"\control\pin", Array.Empty<byte>());
                return;
            case ProjectedPathKind.ControlPinFile:
                SeedSyntheticDirectory(@"\control");
                _cache.SeedFile(@"\control\pin", Array.Empty<byte>());
                return;
            case ProjectedPathKind.StageSyntheticRoot:
            case ProjectedPathKind.StageContent:
            case ProjectedPathKind.LedgerStateContent:
                MaterializeContentPath(info, normalized);
                return;
            case ProjectedPathKind.LedgerSyntheticRoot:
            case ProjectedPathKind.LedgerNodeRoot:
                SeedChainNode(normalized);
                return;
            case ProjectedPathKind.LedgerNodeContainer:
                SeedSyntheticDirectory(normalized);
                foreach (var bridge in GetBridgesForNodeContainer(normalized))
                {
                    var bridgeRoot = CombinePath(normalized, bridge);
                    SeedSyntheticDirectory(bridgeRoot);
                    SeedChainNode(bridgeRoot);
                }
                return;
            case ProjectedPathKind.LedgerPreviousContainer:
                SeedSyntheticDirectory(normalized);
                return;
            default:
                throw new DirectoryNotFoundException(normalized);
        }
    }

    private void EnsureDirectoryControlMaterialized(string directoryPath)
    {
        var normalizedDirectory = NormalizePath(directoryPath);
        if (!CacheEntryExists(normalizedDirectory))
        {
            EnsureDirectoryMaterialized(normalizedDirectory);
        }
        if (_hydratedDirectoryControlPaths.Contains(normalizedDirectory))
        {
            return;
        }

        if (TryGetDirectoryMarkerJournalPathForDirectory(normalizedDirectory, out var markerJournalPath))
        {
            try
            {
                var gatewayValue = _gateway.GetAsync(new GatewayGetRequest(markerJournalPath, true, false), CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
                if (gatewayValue is JsonObject obj &&
                    obj["content"] != null &&
                    !JsonFileSystemLoader.IsNothingContent(obj["content"]!))
                {
                    JsonFileSystemLoader.TryParseDirectoryEnvelopeMeta(obj["content"]!, out var mode, out var uid, out var gid);
                    _cache.SeedDirectory(normalizedDirectory, metadata: null, control: new InMemoryFileSystem.SnapshotControl(null, null, mode, uid, gid));
                    _markerBackedDirectories.Add(normalizedDirectory);
                }
                else
                {
                    _cache.SeedDirectory(normalizedDirectory, metadata: null, control: new InMemoryFileSystem.SnapshotControl(null, null, null, null, null));
                }
            }
            catch (FileNotFoundException)
            {
                _cache.SeedDirectory(normalizedDirectory, metadata: null, control: new InMemoryFileSystem.SnapshotControl(null, null, null, null, null));
            }
        }

        _hydratedDirectoryControlPaths.Add(normalizedDirectory);
    }

    private void MaterializeContentPath(ProjectedPathInfo info, string normalizedPath)
    {
        if (info.JournalPath == null)
        {
            throw new FileNotFoundException(normalizedPath);
        }

        var gatewayValue = _gateway.GetAsync(
                new GatewayGetRequest(info.JournalPath, true, false),
                CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        MaterializeContentPath(info, normalizedPath, gatewayValue);
    }

    private void MaterializeContentPath(ProjectedPathInfo info, string normalizedPath, JsonNode? gatewayValue)
    {
        if (normalizedPath.StartsWith(@"\ledger\", StringComparison.OrdinalIgnoreCase))
        {
            _discoveredPinStates[normalizedPath] = IsPinnedGatewayValue(gatewayValue);
        }

        if (gatewayValue is not JsonObject obj || obj["content"] == null)
        {
            throw new InvalidDataException($"Gateway returned invalid details payload for {normalizedPath}");
        }

        if (JsonFileSystemLoader.IsNothingContent(obj["content"]!))
        {
            throw new FileNotFoundException(normalizedPath);
        }

        EnsureSyntheticAncestors(normalizedPath);
        var entry = JsonFileSystemLoader.CreateEntryFromContent(normalizedPath, obj["content"]!, pinned: IsPinnedGatewayValue(obj));
        if (entry.IsDirectory)
        {
            _cache.SeedDirectory(
                entry.SharePath,
                metadata: null,
                control: new InMemoryFileSystem.SnapshotControl(entry.Pinned, null, entry.Mode, entry.Uid, entry.Gid));
            MaterializeDirectoryChildren(normalizedPath, obj["content"]!);
            _hydratedPaths.Add(normalizedPath);
            return;
        }

        if (string.Equals(entry.ContentKind, "symlink", StringComparison.OrdinalIgnoreCase))
        {
            var journalTargetJson = Encoding.UTF8.GetString(entry.ContentBytes);
            var projectedTargetPath = JournalPathMapper.CompileProjectedPath(JsonSerializer.Deserialize<JsonElement>(journalTargetJson));
            _cache.SeedSymlink(
                entry.SharePath,
                journalTargetJson,
                projectedTargetPath,
                metadata: null,
                control: new InMemoryFileSystem.SnapshotControl(entry.Pinned, entry.ContentKind, entry.Mode, entry.Uid, entry.Gid));
            _hydratedPaths.Add(normalizedPath);
            return;
        }

        _cache.SeedFile(
            entry.SharePath,
            entry.ContentBytes,
            metadata: null,
            control: new InMemoryFileSystem.SnapshotControl(entry.Pinned, entry.ContentKind, entry.Mode, entry.Uid, entry.Gid));
        _hydratedPaths.Add(normalizedPath);
    }

    private T ExecuteRead<T>(string path, Func<T> action)
    {
        try
        {
            return action();
        }
        catch (FileNotFoundException)
        {
            throw;
        }
        catch (DirectoryNotFoundException)
        {
            throw;
        }
        catch (UnauthorizedAccessException)
        {
            throw;
        }
        catch (IOException)
        {
            throw;
        }
        catch (GatewaySemanticException exception) when (IsPermissionError(exception))
        {
            throw new UnauthorizedAccessException($"Gateway access denied for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (GatewaySemanticException exception) when (IsMissingPathError(exception))
        {
            throw new FileNotFoundException($"Gateway path not found: {NormalizePath(path)}", exception);
        }
        catch (GatewaySemanticException exception)
        {
            throw new IOException($"Gateway semantic error for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (HttpRequestException exception)
        {
            throw new IOException($"Gateway request failed for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (TaskCanceledException exception)
        {
            throw new IOException($"Gateway request timed out for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (InvalidDataException exception)
        {
            throw new IOException($"Gateway returned invalid data for {NormalizePath(path)}: {exception.Message}", exception);
        }
    }

    private void CommitStageFile(string normalizedPath, IReadOnlyList<object> journalPath, byte[] content)
    {
        lock (_gate)
        {
            TraceOperation($"CommitStageFile path={normalizedPath} bytes={content.Length}");
            var existingControl = TryGetCachedControl(normalizedPath);
            var contentNode = JsonFileSystemLoader.CreateByteFileContentNode(
                content,
                existingControl?.Mode,
                existingControl?.Uid,
                existingControl?.Gid);
            ExecuteWrite(normalizedPath, () =>
            {
                _gateway.SetAsync(new GatewaySetRequest(journalPath, contentNode), CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
            });
            _deletedPaths.Remove(normalizedPath);
            _cache.SeedFile(normalizedPath, content, metadata: null, control: ToWritableFileControl(existingControl));
            _hydratedPaths.Add(normalizedPath);
        }
    }

    private void CommitDirectoryControl(string directoryPath, string json)
    {
        lock (_gate)
        {
            var normalizedDirectory = NormalizePath(directoryPath);
            var control = ParseDirectoryControlJson(json);
            if (!TryGetDirectoryMarkerJournalPathForDirectory(normalizedDirectory, out var markerJournalPath))
            {
                throw CreateReadOnlyPathException(CombinePath(normalizedDirectory, ".directory"));
            }

            var contentNode = JsonFileSystemLoader.CreateDirectoryMarkerContentNode(control.Mode, control.Uid, control.Gid);
            ExecuteWrite(normalizedDirectory, () =>
            {
                _gateway.SetAsync(new GatewaySetRequest(markerJournalPath, contentNode), CancellationToken.None)
                    .GetAwaiter()
                    .GetResult();
            });

            _cache.SeedDirectory(normalizedDirectory, metadata: null, control: new InMemoryFileSystem.SnapshotControl(null, null, control.Mode, control.Uid, control.Gid));
            _markerBackedDirectories.Add(normalizedDirectory);
            _hydratedDirectoryControlPaths.Add(normalizedDirectory);
        }
    }

    private void ExecuteWrite(string path, Action action)
    {
        try
        {
            action();
        }
        catch (FileNotFoundException)
        {
            throw;
        }
        catch (DirectoryNotFoundException)
        {
            throw;
        }
        catch (UnauthorizedAccessException)
        {
            throw;
        }
        catch (IOException)
        {
            throw;
        }
        catch (GatewaySemanticException exception) when (IsPermissionError(exception))
        {
            throw new UnauthorizedAccessException($"Gateway write denied for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (GatewaySemanticException exception) when (IsMissingPathError(exception))
        {
            throw new FileNotFoundException($"Gateway write path not found: {NormalizePath(path)}", exception);
        }
        catch (GatewaySemanticException exception)
        {
            throw new IOException($"Gateway write semantic error for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (HttpRequestException exception)
        {
            throw new IOException($"Gateway write request failed for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (TaskCanceledException exception)
        {
            throw new IOException($"Gateway write request timed out for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (InvalidDataException exception)
        {
            throw new IOException($"Gateway write returned invalid data for {NormalizePath(path)}: {exception.Message}", exception);
        }
    }

    private T ExecuteWrite<T>(string path, Func<T> action)
    {
        try
        {
            return action();
        }
        catch (FileNotFoundException)
        {
            throw;
        }
        catch (DirectoryNotFoundException)
        {
            throw;
        }
        catch (UnauthorizedAccessException)
        {
            throw;
        }
        catch (IOException)
        {
            throw;
        }
        catch (GatewaySemanticException exception) when (IsPermissionError(exception))
        {
            throw new UnauthorizedAccessException($"Gateway write denied for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (GatewaySemanticException exception) when (IsMissingPathError(exception))
        {
            throw new FileNotFoundException($"Gateway write path not found: {NormalizePath(path)}", exception);
        }
        catch (GatewaySemanticException exception)
        {
            throw new IOException($"Gateway write semantic error for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (HttpRequestException exception)
        {
            throw new IOException($"Gateway write request failed for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (TaskCanceledException exception)
        {
            throw new IOException($"Gateway write request timed out for {NormalizePath(path)}: {exception.Message}", exception);
        }
        catch (InvalidDataException exception)
        {
            throw new IOException($"Gateway write returned invalid data for {NormalizePath(path)}: {exception.Message}", exception);
        }
    }

    private void MaterializeDirectoryChildren(string parentPath, JsonNode contentNode)
    {
        if (contentNode is not JsonArray array || array.Count < 2 || array[1] is not JsonObject childMap)
        {
            return;
        }

        foreach (var pair in childMap)
        {
            if (pair.Key == null || pair.Value == null)
            {
                continue;
            }

            if (string.Equals(pair.Key, "*directory*", StringComparison.Ordinal))
            {
                continue;
            }

            var childPath = CombinePath(parentPath, pair.Key);
            if (CacheEntryExists(childPath))
            {
                continue;
            }

            var markerChildPath = CombinePath(childPath, "*directory*");
            if (childMap.TryGetPropertyValue(pair.Key, out var _) &&
                childMap.TryGetPropertyValue("*directory*", out var markerValue) &&
                markerValue != null &&
                CacheEntryExists(markerChildPath))
            {
                _cache.SeedDirectory(childPath);
                continue;
            }

            var kind = pair.Value.GetValue<string>();
            if (string.Equals(kind, "directory", StringComparison.OrdinalIgnoreCase))
            {
                _cache.SeedDirectory(childPath);
            }
            else
            {
                _cache.SeedFile(childPath, Array.Empty<byte>());
            }
        }
    }

    private void SeedChainNode(string path)
    {
        SeedSyntheticDirectory(path);
        SeedSyntheticDirectory(CombinePath(path, "state"));
        SeedSyntheticDirectory(CombinePath(path, "bridge"));
        SeedSyntheticDirectory(CombinePath(path, "previous"));
    }

    private FileSystemEntry BuildPinControlFileEntry()
    {
        _cache.SeedFile(@"\control\pin", RenderPinControlFileBytes());
        return _cache.GetEntry(@"\control\pin");
    }

    private byte[] RenderPinControlFileBytes()
    {
        var lines = _discoveredPinStates
            .OrderBy(pair => pair.Key, StringComparer.OrdinalIgnoreCase)
            .Select(pair => $"{(pair.Value ? "pinned" : "unpinned")} {pair.Key.Replace('\\', '/')}");
        var content = string.Join('\n', lines);
        return Encoding.UTF8.GetBytes(string.IsNullOrEmpty(content) ? string.Empty : content + "\n");
    }

    private void ReplacePinnedSet(byte[] bytes)
    {
        var desired = ParsePinControlDirectives(bytes);
        foreach (var pair in desired)
        {
            var info = ParsePath(pair.Key);
            if (info.Kind == ProjectedPathKind.Invalid || info.JournalPath == null || !pair.Key.StartsWith(@"\ledger\", StringComparison.OrdinalIgnoreCase))
            {
                throw new InvalidDataException($"Invalid pin control path: {pair.Key.Replace('\\', '/')}");
            }

            var success = pair.Value
                ? _gateway.PinAsync(new GatewayPinRequest(info.JournalPath), CancellationToken.None).GetAwaiter().GetResult()
                : _gateway.UnpinAsync(new GatewayPinRequest(info.JournalPath), CancellationToken.None).GetAwaiter().GetResult();
            if (!success)
            {
                throw new IOException($"Gateway {(pair.Value ? "pin" : "unpin")} failed for {pair.Key.Replace('\\', '/')}");
            }

            _discoveredPinStates[pair.Key] = pair.Value;
        }
    }

    private static Dictionary<string, bool> ParsePinControlDirectives(byte[] bytes)
    {
        var text = Encoding.UTF8.GetString(bytes).Replace("\0", string.Empty, StringComparison.Ordinal);
        if (text.Length > 0 && text[0] == '\uFEFF')
        {
            text = text[1..];
        }
        var result = new Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase);
        using var reader = new StringReader(text);
        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            var trimmed = line.Trim();
            if (string.IsNullOrEmpty(trimmed) || trimmed.StartsWith("#", StringComparison.Ordinal))
            {
                continue;
            }

            var firstSpace = trimmed.IndexOf(' ');
            if (firstSpace <= 0 || firstSpace == trimmed.Length - 1)
            {
                throw new InvalidDataException($"Invalid pin control line: {trimmed}");
            }

            var directive = trimmed[..firstSpace];
            var pathText = trimmed[(firstSpace + 1)..].Trim();
            var normalizedPath = NormalizePath(pathText);
            if (!string.Equals(directive, "pinned", StringComparison.OrdinalIgnoreCase) &&
                !string.Equals(directive, "unpinned", StringComparison.OrdinalIgnoreCase))
            {
                throw new InvalidDataException($"Invalid pin control directive: {directive}");
            }

            if (!normalizedPath.StartsWith(@"\ledger\", StringComparison.OrdinalIgnoreCase))
            {
                throw new InvalidDataException($"Pin control entries must target /ledger/...: {pathText}");
            }

            result[normalizedPath] = string.Equals(directive, "pinned", StringComparison.OrdinalIgnoreCase);
        }

        return result;
    }

    private IReadOnlyList<string> GetBridgesForNodeContainer(string normalizedPath)
    {
        if (string.Equals(normalizedPath, @"\ledger\bridge", StringComparison.OrdinalIgnoreCase))
        {
            return _gateway.BridgesAsync(CancellationToken.None).GetAwaiter().GetResult();
        }

        var bridgeListingPath = BuildBridgeListingJournalPath(normalizedPath);
        var gatewayValue = _gateway.GetAsync(new GatewayGetRequest(bridgeListingPath, true, false), CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        return ParseBridgeListing(gatewayValue);
    }

    private static IReadOnlyList<object> BuildBridgeListingJournalPath(string normalizedPath)
    {
        var segments = NormalizePath(normalizedPath).Trim('\\')
            .Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        if (segments.Length < 2 ||
            !string.Equals(segments[0], "ledger", StringComparison.OrdinalIgnoreCase) ||
            !string.Equals(segments[^1], "bridge", StringComparison.OrdinalIgnoreCase))
        {
            throw new InvalidDataException($"Path is not a ledger bridge container: {normalizedPath}");
        }

        var ledgerParts = new List<object>();
        var cursor = 1;
        while (cursor < segments.Length)
        {
            var segment = segments[cursor];
            if (string.Equals(segment, "bridge", StringComparison.OrdinalIgnoreCase))
            {
                if (cursor == segments.Length - 1)
                {
                    EnsureCurrentLedgerContext(ledgerParts);
                    EnsureCurrentBridgeContext(ledgerParts);
                    ledgerParts.Add(new object[] { "*bridge*" });
                    return ledgerParts;
                }

                EnsureCurrentLedgerContext(ledgerParts);
                EnsureCurrentBridgeContext(ledgerParts);
                ledgerParts.Add(new object[] { "*bridge*", segments[cursor + 1], "chain" });
                cursor += 2;
                continue;
            }

            if (string.Equals(segment, "previous", StringComparison.OrdinalIgnoreCase))
            {
                if (cursor == segments.Length - 1 || !int.TryParse(segments[cursor + 1], out var index))
                {
                    throw new InvalidDataException($"Ledger bridge container path has invalid previous segment: {normalizedPath}");
                }

                ledgerParts.Add(index);
                cursor += 2;
                continue;
            }

            throw new InvalidDataException($"Ledger bridge container path has unsupported segment: {normalizedPath}");
        }

        throw new InvalidDataException($"Ledger bridge container path is incomplete: {normalizedPath}");
    }

    private static IReadOnlyList<string> ParseBridgeListing(JsonNode? gatewayValue)
    {
        var content = gatewayValue is JsonObject obj && obj["content"] != null
            ? obj["content"]
            : gatewayValue;
        if (content == null)
        {
            return Array.Empty<string>();
        }

        if (content is JsonArray array)
        {
            if (array.Count >= 2 &&
                array[0] is JsonValue headerValue &&
                headerValue.TryGetValue<string>(out var header) &&
                string.Equals(header, "directory", StringComparison.OrdinalIgnoreCase))
            {
                if (array[1] is JsonObject childMap)
                {
                    return childMap
                        .Select(pair => pair.Key)
                        .Where(name => !string.Equals(name, "*directory*", StringComparison.Ordinal))
                        .OrderBy(name => name, StringComparer.OrdinalIgnoreCase)
                        .ToArray();
                }

                if (array[1] is JsonArray childArray)
                {
                    return childArray
                        .Select(ExtractBridgeName)
                        .Where(name => !string.IsNullOrWhiteSpace(name))
                        .Distinct(StringComparer.OrdinalIgnoreCase)
                        .OrderBy(name => name, StringComparer.OrdinalIgnoreCase)
                        .ToArray();
                }
            }

            if (array.Count > 0 && array[0] is JsonArray childList)
            {
                return childList
                    .Select(ExtractBridgeName)
                    .Where(name => !string.IsNullOrWhiteSpace(name))
                    .Distinct(StringComparer.OrdinalIgnoreCase)
                    .OrderBy(name => name, StringComparer.OrdinalIgnoreCase)
                    .ToArray();
            }

            return array
                .Select(ExtractBridgeName)
                .Where(name => !string.IsNullOrWhiteSpace(name))
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .OrderBy(name => name, StringComparer.OrdinalIgnoreCase)
                .ToArray();
        }

        return Array.Empty<string>();
    }

    private static string ExtractBridgeName(JsonNode? node)
    {
        if (node == null)
        {
            return string.Empty;
        }

        if (node is JsonValue value && value.TryGetValue<string>(out var directString))
        {
            return directString;
        }

        if (node is JsonObject wrapper &&
            wrapper["*type/string*"] is JsonValue wrappedValue &&
            wrappedValue.TryGetValue<string>(out var wrappedString))
        {
            return wrappedString;
        }

        return node.ToJsonString().Trim('"');
    }

    private InMemoryFileSystem BuildPinControlView(string normalizedControlPath)
    {
        var view = new InMemoryFileSystem($"{_name}-pin-view");
        view.SeedDirectory(@"\control");
        view.SeedDirectory(@"\control\pin");
        view.SeedDirectory(@"\control\pin\ledger");

        if (string.Equals(normalizedControlPath, @"\control", StringComparison.OrdinalIgnoreCase))
        {
            return view;
        }

        if (string.Equals(normalizedControlPath, @"\control\pin", StringComparison.OrdinalIgnoreCase))
        {
            return view;
        }

        if (string.Equals(normalizedControlPath, @"\control\pin\ledger", StringComparison.OrdinalIgnoreCase))
        {
            PopulatePinControlDirectoryChildren(view, normalizedControlPath, @"\ledger");
            return view;
        }

        var info = ParsePath(normalizedControlPath);
        if (info.Kind != ProjectedPathKind.ControlPinMirror || info.MirroredProjectedPath == null || info.MirroredKind == null)
        {
            throw new FileNotFoundException(normalizedControlPath);
        }

        if (IsMirroredDirectory(info))
        {
            EnsureSyntheticControlAncestors(view, normalizedControlPath);
            view.SeedDirectory(normalizedControlPath);
            PopulatePinControlDirectoryChildren(view, normalizedControlPath, info.MirroredProjectedPath);
            var isPinnedDirectory = info.JournalPath != null && IsPinnedJournalPath(info.JournalPath);
            if (!isPinnedDirectory && view.ListEntriesInDirectory(normalizedControlPath).Count == 0)
            {
                throw new FileNotFoundException(normalizedControlPath);
            }
        }
        else
        {
            EnsureSyntheticControlAncestors(view, normalizedControlPath);
            if (!ShouldExposePinnedControlPath(info))
            {
                throw new FileNotFoundException(normalizedControlPath);
            }
            view.SeedFile(normalizedControlPath, Array.Empty<byte>());
        }

        return view;
    }

    private FileSystemEntry PinControlFile(string normalizedControlPath, ProjectedPathInfo info)
    {
        if (info.JournalPath == null || info.MirroredProjectedPath == null || info.MirroredKind == null)
        {
            throw new NotSupportedException("Pin control file path is not pinnable.");
        }

        EnsurePathMaterialized(info.MirroredProjectedPath);
        var entry = _cache.GetEntry(info.MirroredProjectedPath);
        if (entry.IsDirectory)
        {
            throw new IOException($"Pinned control file target is a directory: {normalizedControlPath}");
        }

        if (!_gateway.PinAsync(new GatewayPinRequest(info.JournalPath), CancellationToken.None).GetAwaiter().GetResult())
        {
            throw new IOException($"Gateway pin failed for {normalizedControlPath}");
        }

        return BuildPinControlView(normalizedControlPath).GetEntry(normalizedControlPath);
    }

    private FileSystemEntry PinControlDirectory(string normalizedControlPath, ProjectedPathInfo info)
    {
        if (info.JournalPath == null || info.MirroredProjectedPath == null || info.MirroredKind == null)
        {
            throw new NotSupportedException("Pin control directory path is not pinnable.");
        }

        EnsurePathMaterialized(info.MirroredProjectedPath);
        var entry = _cache.GetEntry(info.MirroredProjectedPath);
        if (!entry.IsDirectory)
        {
            throw new IOException($"Pinned control directory target is not a directory: {normalizedControlPath}");
        }

        if (!_gateway.PinAsync(new GatewayPinRequest(info.JournalPath), CancellationToken.None).GetAwaiter().GetResult())
        {
            throw new IOException($"Gateway pin failed for {normalizedControlPath}");
        }

        return BuildPinControlView(normalizedControlPath).GetEntry(normalizedControlPath);
    }

    private void UnpinControlPath(string normalizedControlPath, ProjectedPathInfo info)
    {
        if (info.JournalPath == null || info.MirroredProjectedPath == null || info.MirroredKind == null)
        {
            throw new NotSupportedException("Pin control path is not pinnable.");
        }

        EnsurePathMaterialized(info.MirroredProjectedPath);
        if (!_gateway.UnpinAsync(new GatewayPinRequest(info.JournalPath), CancellationToken.None).GetAwaiter().GetResult())
        {
            throw new IOException($"Gateway unpin failed for {normalizedControlPath}");
        }
    }

    private void PopulatePinControlDirectoryChildren(InMemoryFileSystem view, string controlDirectoryPath, string mirroredDirectoryPath)
    {
        foreach (var child in GetMirroredChildren(mirroredDirectoryPath))
        {
            var childMirroredPath = CombinePath(mirroredDirectoryPath, child.Name);
            var childControlPath = CombinePath(controlDirectoryPath, child.Name);
            var childInfo = ParsePath(childControlPath);
            if (childInfo.Kind != ProjectedPathKind.ControlPinMirror || !ShouldExposePinnedControlPath(childInfo))
            {
                continue;
            }

            if (IsMirroredDirectory(childInfo))
            {
                view.SeedDirectory(childControlPath);
            }
            else
            {
                view.SeedFile(childControlPath, Array.Empty<byte>());
            }
        }
    }

    private bool ShouldExposePinnedControlPath(ProjectedPathInfo info)
    {
        if (info.Kind != ProjectedPathKind.ControlPinMirror || info.MirroredProjectedPath == null || info.MirroredKind == null)
        {
            return false;
        }

        if (IsControlScaffoldingPath(info))
        {
            return true;
        }

        if (info.JournalPath != null && IsPinnedJournalPath(info.JournalPath))
        {
            return true;
        }

        return IsMirroredDirectory(info) && HasPinnedMirroredDescendants(info.MirroredProjectedPath);
    }

    private static bool IsControlScaffoldingPath(ProjectedPathInfo info)
    {
        if (info.MirroredProjectedPath == null || info.MirroredKind == null)
        {
            return false;
        }

        if (info.MirroredKind is ProjectedPathKind.LedgerSyntheticRoot or
            ProjectedPathKind.LedgerNodeContainer or
            ProjectedPathKind.LedgerPreviousContainer or
            ProjectedPathKind.LedgerNodeRoot)
        {
            return true;
        }

        return info.MirroredKind == ProjectedPathKind.LedgerStateContent &&
            string.Equals(GetNameOrNull(info.MirroredProjectedPath), "state", StringComparison.OrdinalIgnoreCase);
    }

    private bool HasPinnedMirroredDescendants(string mirroredDirectoryPath)
    {
        foreach (var child in GetMirroredChildren(mirroredDirectoryPath))
        {
            var childMirroredPath = CombinePath(mirroredDirectoryPath, child.Name);
            var controlChildPath = CombinePath(@"\control\pin", childMirroredPath.TrimStart('\\'));
            var childInfo = ParsePath(controlChildPath);
            if (childInfo.Kind != ProjectedPathKind.ControlPinMirror)
            {
                continue;
            }

            if (ShouldExposePinnedControlPath(childInfo))
            {
                return true;
            }
        }

        return false;
    }

    private List<FileSystemEntry> GetMirroredChildren(string mirroredDirectoryPath)
    {
        EnsureDirectoryMaterialized(mirroredDirectoryPath);
        return _cache.ListEntriesInDirectory(mirroredDirectoryPath)
            .Where(entry => !IsDeletedChild(mirroredDirectoryPath, entry.Name))
            .ToList();
    }

    private bool IsPinnedJournalPath(IReadOnlyList<object> journalPath)
    {
        var gatewayValue = _gateway.GetAsync(new GatewayGetRequest(journalPath, true, false), CancellationToken.None)
            .GetAwaiter()
            .GetResult();
        return IsPinnedGatewayValue(gatewayValue);
    }

    private static bool IsPinnedGatewayValue(JsonNode? gatewayValue)
    {
        if (gatewayValue is not JsonObject obj)
        {
            return false;
        }

        var pinnedValue = obj["pinned?"];
        return pinnedValue switch
        {
            JsonArray array => array.Count > 0,
            JsonValue value when value.TryGetValue<bool>(out var boolValue) => boolValue,
            _ => false
        };
    }

    private bool IsMirroredDirectory(ProjectedPathInfo info)
    {
        if (info.MirroredKind is ProjectedPathKind.LedgerSyntheticRoot or
            ProjectedPathKind.LedgerNodeContainer or
            ProjectedPathKind.LedgerPreviousContainer or
            ProjectedPathKind.LedgerNodeRoot)
        {
            return true;
        }

        if (info.MirroredKind != ProjectedPathKind.LedgerStateContent || info.MirroredProjectedPath == null)
        {
            return false;
        }

        EnsurePathMaterialized(info.MirroredProjectedPath);
        return _cache.GetEntry(info.MirroredProjectedPath).IsDirectory;
    }

    private static void EnsureSyntheticControlAncestors(InMemoryFileSystem view, string controlPath)
    {
        var normalized = NormalizePath(controlPath);
        var current = "\\";
        var segments = normalized.Trim('\\').Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        foreach (var segment in segments.Take(Math.Max(0, segments.Length - 1)))
        {
            current = CombinePath(current, segment);
            view.SeedDirectory(current);
        }
    }

    private void EnsureParentDirectoryMaterialized(string path)
    {
        var parent = GetParentPath(path);
        EnsureDirectoryMaterialized(parent);
    }

    private byte[] GetInitialStageContent(string normalizedPath, FileMode mode)
    {
        if (mode is FileMode.Create or FileMode.CreateNew or FileMode.Truncate)
        {
            return Array.Empty<byte>();
        }

        try
        {
            if (!_hydratedPaths.Contains(normalizedPath))
            {
                EnsurePathMaterialized(normalizedPath);
            }

            using var stream = _cache.OpenFile(normalizedPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
            using var memory = new MemoryStream();
            stream.CopyTo(memory);
            return memory.ToArray();
        }
        catch (FileNotFoundException)
        {
            if (mode == FileMode.Open)
            {
                throw;
            }

            return Array.Empty<byte>();
        }
    }

    private void EnsureExistingPathMaterializedForWrite(string normalizedPath, FileMode mode)
    {
        if (mode == FileMode.CreateNew)
        {
            return;
        }

        try
        {
            if (!_hydratedPaths.Contains(normalizedPath))
            {
                EnsurePathMaterialized(normalizedPath);
            }
        }
        catch (FileNotFoundException)
        {
            if (mode == FileMode.Open)
            {
                throw;
            }
        }
    }

    private void EnsureExistingPathMaterializedForWriteBatched(string normalizedPath, ProjectedPathInfo info, FileMode mode)
    {
        var parentNormalizedPath = GetParentPath(normalizedPath);
        if (!ShouldBatchStageWriteHydration(parentNormalizedPath, normalizedPath, info, mode))
        {
            EnsureExistingPathMaterializedForWrite(normalizedPath, mode);
            return;
        }

        var parentInfo = ParsePath(parentNormalizedPath);
        if (parentInfo.JournalPath == null || info.JournalPath == null)
        {
            EnsureExistingPathMaterializedForWrite(normalizedPath, mode);
            return;
        }

        var batchResult = _gateway.BatchAsync(
                new GatewayBatchRequest(
                    new[]
                    {
                        CreateBatchGetOperation(parentInfo.JournalPath),
                        CreateBatchGetOperation(info.JournalPath),
                    }),
                CancellationToken.None)
            .GetAwaiter()
            .GetResult();

        if (batchResult is not JsonArray results || results.Count < 2)
        {
            throw new InvalidDataException("Gateway batch get returned invalid result shape.");
        }

        MaterializeContentPath(parentInfo, parentNormalizedPath, results[0]);

        try
        {
            MaterializeContentPath(info, normalizedPath, results[1]);
        }
        catch (FileNotFoundException)
        {
            if (mode == FileMode.Open)
            {
                throw;
            }
        }
    }

    private static GatewayBatchOperation CreateBatchGetOperation(IReadOnlyList<object> journalPath)
    {
        var arguments = new JsonObject
        {
            ["path"] = JsonSerializer.SerializeToNode(journalPath),
        };

        if (journalPath.Count > 0 && journalPath[0] is int or long)
        {
            arguments["pinned?"] = true;
            arguments["proof?"] = false;
            return new GatewayBatchOperation("resolve", arguments);
        }

        return new GatewayBatchOperation("get", arguments);
    }

    private bool ShouldBatchStageWriteHydration(string parentNormalizedPath, string normalizedPath, ProjectedPathInfo info, FileMode mode)
    {
        if (mode == FileMode.CreateNew ||
            _hydratedPaths.Contains(normalizedPath) ||
            CacheEntryExists(normalizedPath))
        {
            return false;
        }

        var parentInfo = ParsePath(parentNormalizedPath);
        if (parentInfo.Kind != ProjectedPathKind.StageContent ||
            info.Kind != ProjectedPathKind.StageContent)
        {
            return false;
        }

        if (_hydratedPaths.Contains(parentNormalizedPath) || CacheEntryExists(parentNormalizedPath))
        {
            return false;
        }

        return true;
    }

    private bool IsDeletedChild(string parentPath, string childName)
    {
        var childPath = CombinePath(parentPath, childName);
        return _deletedPaths.Contains(childPath);
    }

    private JsonNode BuildStageContentNode(string normalizedSource)
    {
        var existingControl = TryGetCachedControl(normalizedSource);
        if (_cache.TryGetSymlink(normalizedSource, out var symlink))
        {
            return JsonFileSystemLoader.CreateSymlinkContentNode(
                symlink.JournalTargetJson,
                symlink.Mode,
                symlink.Uid,
                symlink.Gid);
        }

        try
        {
            var children = _cache.ListEntriesInDirectory(normalizedSource)
                .Where(entry => !string.Equals(entry.Name, ".directory", StringComparison.OrdinalIgnoreCase))
                .ToList();
            if (children.Count > 0)
            {
                throw new IOException($"Directory is not empty: {normalizedSource}");
            }

            return JsonFileSystemLoader.CreateDirectoryContentNode();
        }
        catch (DirectoryNotFoundException)
        {
            return JsonFileSystemLoader.CreateByteFileContentNode(
                ReadCacheFileBytes(normalizedSource),
                existingControl?.Mode,
                existingControl?.Uid,
                existingControl?.Gid);
        }
    }

    private InMemoryFileSystem.SnapshotControl? TryGetCachedControl(string normalizedPath)
    {
        var snapshotPath = normalizedPath.TrimStart('\\').Replace('\\', '/');
        var snapshot = _cache.ExportSnapshot();

        var file = snapshot.Files.FirstOrDefault(entry => string.Equals(entry.Path, snapshotPath, StringComparison.OrdinalIgnoreCase));
        if (file != null)
        {
            return file.Control;
        }

        var directory = snapshot.Directories.FirstOrDefault(entry => string.Equals(entry.Path, snapshotPath, StringComparison.OrdinalIgnoreCase));
        return directory?.Control;
    }

    private InMemoryFileSystem.SnapshotMetadata? TryGetCachedMetadata(string normalizedPath)
    {
        var snapshotPath = normalizedPath.TrimStart('\\').Replace('\\', '/');
        var snapshot = _cache.ExportSnapshot();

        var file = snapshot.Files.FirstOrDefault(entry => string.Equals(entry.Path, snapshotPath, StringComparison.OrdinalIgnoreCase));
        if (file != null)
        {
            return file.Metadata;
        }

        var directory = snapshot.Directories.FirstOrDefault(entry => string.Equals(entry.Path, snapshotPath, StringComparison.OrdinalIgnoreCase));
        return directory?.Metadata;
    }

    private static InMemoryFileSystem.SnapshotControl? ToWritableFileControl(InMemoryFileSystem.SnapshotControl? existingControl)
    {
        if (existingControl == null)
        {
            return null;
        }

        return new InMemoryFileSystem.SnapshotControl(
            existingControl.Pinned,
            "bytes",
            existingControl.Mode,
            existingControl.Uid,
            existingControl.Gid);
    }

    private static int ApplyReadonlyToMode(int? existingMode, bool isReadonly, bool isDirectory)
    {
        const int defaultFileMode = 420;
        const int defaultDirectoryMode = 493;
        const int allWriteBits = 146;
        const int ownerWriteBit = 128;

        var mode = existingMode ?? (isDirectory ? defaultDirectoryMode : defaultFileMode);
        if (isReadonly)
        {
            return mode & ~allWriteBits;
        }

        return mode | ownerWriteBit;
    }

    private byte[] ReadCacheFileBytes(string normalizedPath)
    {
        using var stream = _cache.OpenFile(normalizedPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite, FileOptions.None);
        using var memory = new MemoryStream();
        stream.CopyTo(memory);
        return memory.ToArray();
    }

    private void EnsureSyntheticAncestors(string path)
    {
        var segments = NormalizePath(path).Trim('\\')
            .Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        var current = "\\";
        foreach (var segment in segments.Take(Math.Max(0, segments.Length - 1)))
        {
            current = CombinePath(current, segment);
            var info = ParsePath(current);
            if (info.Kind is ProjectedPathKind.StageSyntheticRoot or
                ProjectedPathKind.LedgerSyntheticRoot or
                ProjectedPathKind.LedgerNodeContainer or
                ProjectedPathKind.LedgerPreviousContainer or
                ProjectedPathKind.LedgerNodeRoot)
            {
                EnsureDirectoryMaterialized(current);
            }
            else if (!CacheEntryExists(current))
            {
                _cache.SeedDirectory(current);
            }
        }
    }

    private void SeedSyntheticDirectory(string path)
    {
        if (!CacheEntryExists(path))
        {
            _cache.SeedDirectory(path);
        }
    }

    private bool CacheEntryExists(string path)
    {
        try
        {
            _cache.GetEntry(path);
            return true;
        }
        catch
        {
            return false;
        }
    }

    private void TrackPendingWritableStageFile(string normalizedPath)
    {
        if (_pendingWritableStageFiles.TryGetValue(normalizedPath, out var count))
        {
            _pendingWritableStageFiles[normalizedPath] = count + 1;
            return;
        }

        _pendingWritableStageFiles[normalizedPath] = 1;
    }

    private void UntrackPendingWritableStageFile(string normalizedPath)
    {
        lock (_gate)
        {
            if (!_pendingWritableStageFiles.TryGetValue(normalizedPath, out var count))
            {
                return;
            }

            if (count <= 1)
            {
                _pendingWritableStageFiles.Remove(normalizedPath);
                return;
            }

            _pendingWritableStageFiles[normalizedPath] = count - 1;
        }
    }

    private bool IsPendingWritableStageFile(string normalizedPath) =>
        _pendingWritableStageFiles.ContainsKey(normalizedPath);

    private static bool IsPermissionError(GatewaySemanticException exception)
    {
        return exception.Code.Contains("auth", StringComparison.OrdinalIgnoreCase) ||
               exception.Code.Contains("permission", StringComparison.OrdinalIgnoreCase) ||
               exception.Code.Contains("access", StringComparison.OrdinalIgnoreCase) ||
               exception.Message.Contains("auth", StringComparison.OrdinalIgnoreCase) ||
               exception.Message.Contains("permission", StringComparison.OrdinalIgnoreCase) ||
               exception.Message.Contains("access", StringComparison.OrdinalIgnoreCase);
    }

    private static bool IsMissingPathError(GatewaySemanticException exception)
    {
        return exception.Code.Contains("missing", StringComparison.OrdinalIgnoreCase) ||
               exception.Code.Contains("not-found", StringComparison.OrdinalIgnoreCase) ||
               exception.Code.Contains("unknown", StringComparison.OrdinalIgnoreCase) ||
               exception.Message.Contains("missing", StringComparison.OrdinalIgnoreCase) ||
               exception.Message.Contains("not found", StringComparison.OrdinalIgnoreCase);
    }

    private static void TraceOperation(string message)
    {
        Console.WriteLine($"[GatewayProjectionFileSystem] {message}");
    }

    private static IReadOnlyList<object> GetDirectoryMarkerJournalPath(IReadOnlyList<object> journalPath)
    {
        if (journalPath.Count == 0)
        {
            return journalPath;
        }

        var result = new List<object>(journalPath.Count);
        for (var index = 0; index < journalPath.Count; index++)
        {
            var segment = journalPath[index];
            if (index == journalPath.Count - 1 && segment is object[] stateBlock)
            {
                var markerBlock = stateBlock.Cast<object>().Concat(new object[] { "*directory*" }).ToArray();
                result.Add(markerBlock);
            }
            else
            {
                result.Add(segment);
            }
        }

        return result;
    }

    private bool TryGetDirectoryMarkerJournalPathForDirectory(string directoryPath, out IReadOnlyList<object> markerJournalPath)
    {
        var info = ParsePath(directoryPath);
        if (info.Kind == ProjectedPathKind.StageSyntheticRoot)
        {
            markerJournalPath = new List<object>
            {
                new object[] { "*state*", "*directory*" }
            };
            return true;
        }

        if (info.Kind == ProjectedPathKind.StageContent && info.JournalPath != null)
        {
            markerJournalPath = GetDirectoryMarkerJournalPath(info.JournalPath);
            return true;
        }

        markerJournalPath = Array.Empty<object>();
        return false;
    }

    private ProjectedPathInfo ParsePath(string path)
    {
        var normalized = NormalizePath(path);
        if (normalized == "\\")
        {
            return new ProjectedPathInfo(ProjectedPathKind.Root, null);
        }

        var segments = normalized.Trim('\\')
            .Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        if (segments.Length == 0)
        {
            return new ProjectedPathInfo(ProjectedPathKind.Root, null);
        }

        if (string.Equals(segments[0], "stage", StringComparison.OrdinalIgnoreCase))
        {
            var journal = new List<object>
            {
                new object[] { "*state*" }.Concat(segments.Skip(1).Cast<object>()).ToArray()
            };
            return new ProjectedPathInfo(
                segments.Length == 1 ? ProjectedPathKind.StageSyntheticRoot : ProjectedPathKind.StageContent,
                journal);
        }

        if (string.Equals(segments[0], "control", StringComparison.OrdinalIgnoreCase))
        {
            if (segments.Length == 1)
            {
                return new ProjectedPathInfo(ProjectedPathKind.ControlSyntheticRoot, null);
            }

            if (!string.Equals(segments[1], "pin", StringComparison.OrdinalIgnoreCase))
            {
                return new ProjectedPathInfo(ProjectedPathKind.Invalid, null);
            }

            return segments.Length == 2
                ? new ProjectedPathInfo(ProjectedPathKind.ControlPinFile, null)
                : new ProjectedPathInfo(ProjectedPathKind.Invalid, null);
        }

        if (!string.Equals(segments[0], "ledger", StringComparison.OrdinalIgnoreCase))
        {
            return new ProjectedPathInfo(ProjectedPathKind.Invalid, null);
        }

        var ledgerParts = new List<object>();
        var cursor = 1;
        while (cursor < segments.Length)
        {
            var segment = segments[cursor];
            if (string.Equals(segment, "state", StringComparison.OrdinalIgnoreCase))
            {
                EnsureCurrentLedgerContext(ledgerParts);
                EnsureCurrentBridgeContext(ledgerParts);

                var stateBlock = new object[] { "*state*" }.Concat(segments.Skip(cursor + 1).Cast<object>()).ToArray();
                ledgerParts.Add(stateBlock);
                return new ProjectedPathInfo(ProjectedPathKind.LedgerStateContent, ledgerParts);
            }

            if (string.Equals(segment, "bridge", StringComparison.OrdinalIgnoreCase))
            {
                if (cursor == segments.Length - 1)
                {
                    return new ProjectedPathInfo(ProjectedPathKind.LedgerNodeContainer, null);
                }

                EnsureCurrentLedgerContext(ledgerParts);
                EnsureCurrentBridgeContext(ledgerParts);
                ledgerParts.Add(new object[] { "*bridge*", segments[cursor + 1], "chain" });
                cursor += 2;
                if (cursor == segments.Length)
                {
                    return new ProjectedPathInfo(ProjectedPathKind.LedgerNodeRoot, null);
                }
                continue;
            }

            if (string.Equals(segment, "previous", StringComparison.OrdinalIgnoreCase))
            {
                if (cursor == segments.Length - 1)
                {
                    return new ProjectedPathInfo(ProjectedPathKind.LedgerPreviousContainer, null);
                }

                if (!int.TryParse(segments[cursor + 1], out var index))
                {
                    return new ProjectedPathInfo(ProjectedPathKind.Invalid, null);
                }

                ledgerParts.Add(index);
                cursor += 2;
                if (cursor == segments.Length)
                {
                    return new ProjectedPathInfo(ProjectedPathKind.LedgerNodeRoot, null);
                }
                continue;
            }

            return new ProjectedPathInfo(ProjectedPathKind.Invalid, null);
        }

        return new ProjectedPathInfo(ProjectedPathKind.LedgerSyntheticRoot, null);
    }

    private static void EnsureCurrentLedgerContext(List<object> ledgerParts)
    {
        if (ledgerParts.Count == 0)
        {
            ledgerParts.Add(-1);
        }
    }

    private static void EnsureCurrentBridgeContext(List<object> ledgerParts)
    {
        if (ledgerParts.Count == 0)
        {
            return;
        }

        if (ledgerParts[^1] is object[] bridgeBlock &&
            bridgeBlock.Length >= 3 &&
            string.Equals(bridgeBlock[0] as string, "*bridge*", StringComparison.Ordinal) &&
            string.Equals(bridgeBlock[2] as string, "chain", StringComparison.Ordinal))
        {
            ledgerParts.Add(-1);
        }
    }

    private static UnauthorizedAccessException CreateReadOnlyPathException(string path)
    {
        return new UnauthorizedAccessException($"Read-only projected path: {NormalizePath(path)}");
    }

    private static void RejectSyntheticDirectoryControlMutation(string path)
    {
        if (string.Equals(GetNameOrNull(path), ".directory", StringComparison.OrdinalIgnoreCase))
        {
            throw CreateReadOnlyPathException(path);
        }
    }

    private static string CombinePath(string basePath, string segment)
    {
        var normalizedBase = NormalizePath(basePath);
        return normalizedBase == "\\" ? "\\" + segment : normalizedBase + "\\" + segment;
    }

    private static string NormalizePath(string path)
    {
        if (string.IsNullOrWhiteSpace(path) || path == "\\")
        {
            return "\\";
        }

        var normalized = path.Replace('/', '\\').Trim();
        if (!normalized.StartsWith("\\", StringComparison.Ordinal))
        {
            normalized = "\\" + normalized;
        }

        while (normalized.Contains("\\\\", StringComparison.Ordinal))
        {
            normalized = normalized.Replace("\\\\", "\\", StringComparison.Ordinal);
        }

        return normalized.Length > 1 ? normalized.TrimEnd('\\') : normalized;
    }

    private bool CanWriteStagePath(string path)
    {
        if (!_enableStageWrites)
        {
            return false;
        }

        return ParsePath(path).Kind == ProjectedPathKind.StageContent;
    }

    private bool CanWriteStageDirectoryControlPath(string directoryPath)
    {
        if (!_enableStageWrites)
        {
            return false;
        }

        var info = ParsePath(directoryPath);
        return info.Kind is ProjectedPathKind.StageSyntheticRoot or ProjectedPathKind.StageContent;
    }

    private static bool TryGetSyntheticDirectoryControlTargetPath(string path, out string directoryPath)
    {
        var normalized = NormalizePath(path);
        if (!string.Equals(GetNameOrNull(normalized), ".directory", StringComparison.OrdinalIgnoreCase))
        {
            directoryPath = string.Empty;
            return false;
        }

        directoryPath = GetParentPath(normalized);
        return true;
    }

    private static string GetParentPath(string path)
    {
        var normalized = NormalizePath(path);
        var lastSlash = normalized.LastIndexOf('\\');
        return lastSlash <= 0 ? "\\" : normalized[..lastSlash];
    }

    private static string? GetNameOrNull(string path)
    {
        var normalized = NormalizePath(path);
        if (normalized == "\\")
        {
            return null;
        }

        var lastSlash = normalized.LastIndexOf('\\');
        return lastSlash < 0 ? normalized : normalized[(lastSlash + 1)..];
    }

    private static DirectoryControlValues ParseDirectoryControlJson(string json)
    {
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;
        if (root.ValueKind != JsonValueKind.Object)
        {
            throw new InvalidDataException(".directory must be a JSON object.");
        }

        ValidateOnlyAllowedProperties(root, new[] { "version", "directory" }, ".directory");
        if (!root.TryGetProperty("version", out var versionElement) ||
            versionElement.ValueKind != JsonValueKind.Number ||
            versionElement.GetInt32() != 1)
        {
            throw new InvalidDataException(".directory version must equal 1.");
        }

        int? mode = null;
        int? uid = null;
        int? gid = null;
        if (root.TryGetProperty("directory", out var directoryElement))
        {
            if (directoryElement.ValueKind != JsonValueKind.Object)
            {
                throw new InvalidDataException(".directory directory must be an object.");
            }

            ValidateOnlyAllowedProperties(directoryElement, new[] { "meta" }, ".directory.directory");
            if (directoryElement.TryGetProperty("meta", out var metaElement))
            {
                if (metaElement.ValueKind != JsonValueKind.Object)
                {
                    throw new InvalidDataException(".directory.directory.meta must be an object.");
                }

                ValidateOnlyAllowedProperties(metaElement, new[] { "mode", "uid", "gid" }, ".directory.directory.meta");
                mode = ReadOptionalInteger(metaElement, "mode", ".directory.directory.meta");
                uid = ReadOptionalInteger(metaElement, "uid", ".directory.directory.meta");
                gid = ReadOptionalInteger(metaElement, "gid", ".directory.directory.meta");
            }
        }

        return new DirectoryControlValues(mode, uid, gid);
    }

    private static void ValidateOnlyAllowedProperties(JsonElement element, IEnumerable<string> allowedProperties, string context)
    {
        var allowed = new HashSet<string>(allowedProperties, StringComparer.Ordinal);
        foreach (var property in element.EnumerateObject())
        {
            if (!allowed.Contains(property.Name))
            {
                throw new InvalidDataException($"{context} contains unsupported field: {property.Name}");
            }
        }
    }

    private static int? ReadOptionalInteger(JsonElement element, string propertyName, string context)
    {
        if (!element.TryGetProperty(propertyName, out var property))
        {
            return null;
        }

        if (property.ValueKind != JsonValueKind.Number || !property.TryGetInt32(out var value))
        {
            throw new InvalidDataException($"{context}.{propertyName} must be an integer.");
        }

        return value;
    }

    private sealed record ProjectedPathInfo(ProjectedPathKind Kind, IReadOnlyList<object>? JournalPath, string? MirroredProjectedPath = null, ProjectedPathKind? MirroredKind = null);
    private sealed record DirectoryControlValues(int? Mode, int? Uid, int? Gid);

    private enum ProjectedPathKind
    {
        Invalid,
        Root,
        StageSyntheticRoot,
        StageContent,
        ControlSyntheticRoot,
        ControlPinFile,
        ControlPinRoot,
        ControlPinLedgerRoot,
        ControlPinMirror,
        LedgerSyntheticRoot,
        LedgerNodeContainer,
        LedgerPreviousContainer,
        LedgerNodeRoot,
        LedgerStateContent,
    }

    private sealed class GatewayStageWriteStream : MemoryStream, ISuppressibleCommitStream
    {
        private readonly Action<byte[]> _onCommit;
        private readonly Action _onClosed;
        private bool _committed;
        private bool _suppressCommit;

        public GatewayStageWriteStream(byte[] initialContent, bool append, Action<byte[]> onCommit, Action onClosed)
            : base()
        {
            _onCommit = onCommit;
            _onClosed = onClosed;
            Write(initialContent, 0, initialContent.Length);
            Position = append ? Length : 0;
            if (!append)
            {
                Position = 0;
            }
        }

        protected override void Dispose(bool disposing)
        {
            try
            {
                if (disposing && !_committed)
                {
                    _committed = true;
                    if (!_suppressCommit)
                    {
                        _onCommit(ToArray());
                    }
                }
            }
            finally
            {
                if (disposing)
                {
                    _onClosed();
                }

                base.Dispose(disposing);
            }
        }

        public void SuppressCommit()
        {
            _suppressCommit = true;
        }
    }

    private sealed class GatewayDirectoryControlWriteStream : MemoryStream
    {
        private readonly Action<string> _onCommit;
        private bool _committed;

        public GatewayDirectoryControlWriteStream(byte[] initialContent, Action<string> onCommit)
            : base()
        {
            _onCommit = onCommit;
            Write(initialContent, 0, initialContent.Length);
            Position = 0;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && !_committed)
            {
                _committed = true;
                _onCommit(Encoding.UTF8.GetString(ToArray()));
            }

            base.Dispose(disposing);
        }
    }
}
