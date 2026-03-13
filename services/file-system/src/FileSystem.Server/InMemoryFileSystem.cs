using System.Runtime.Serialization;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using DiskAccessLibrary.FileSystems.Abstractions;

namespace FileSystem.Server;

public sealed class InMemoryFileSystem : IFileSystem, ISymlinkAwareFileSystem
{
    private const string DirectoryControlFileName = ".directory";
    private readonly object _gate = new();
    private readonly string _name;
    private readonly ulong _capacity;
    private readonly MemoryDirectoryNode _root;
    private readonly Action<InMemoryFileSystem>? _onChanged;
    private readonly Func<string, bool>? _isWritablePath;

    public InMemoryFileSystem(
        string name,
        ulong capacity = 64UL * 1024UL * 1024UL,
        Action<InMemoryFileSystem>? onChanged = null,
        Func<string, bool>? isWritablePath = null)
    {
        _name = name;
        _capacity = capacity;
        _root = new MemoryDirectoryNode(string.Empty);
        _onChanged = onChanged;
        _isWritablePath = isWritablePath;
    }

    public string Name => _name;

    public long Size => (long)_capacity;

    public long FreeSpace
    {
        get
        {
            lock (_gate)
            {
                var used = GetUsedBytes(_root);
                return Math.Max(0L, (long)(_capacity - Math.Min(_capacity, used)));
            }
        }
    }

    public bool SupportsNamedStreams => false;

    public FileSystemEntry GetEntry(string path)
    {
        lock (_gate)
        {
            if (TryGetSyntheticDirectoryControl(path, out var entry))
            {
                return entry;
            }

            var node = GetNode(path);
            return CreateEntry(node);
        }
    }

    public FileSystemEntry CreateFile(string path)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var parent = GetParentDirectory(path, createIfMissing: true);
            var name = GetName(path);
            if (parent.Children.ContainsKey(name))
            {
                throw new IOException($"Path already exists: {path}");
            }

            var file = new MemoryFileNode(name);
            parent.Children.Add(name, file);
            var entry = CreateEntry(file);
            NotifyChanged();
            return entry;
        }
    }

    public FileSystemEntry CreateDirectory(string path)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var parent = GetParentDirectory(path, createIfMissing: true);
            var name = GetName(path);
            if (parent.Children.TryGetValue(name, out var existing))
            {
                if (existing is MemoryDirectoryNode existingDirectory)
                {
                    return CreateEntry(existingDirectory);
                }

                throw new IOException($"File already exists: {path}");
            }

            var directory = new MemoryDirectoryNode(name);
            parent.Children.Add(name, directory);
            var entry = CreateEntry(directory);
            NotifyChanged();
            return entry;
        }
    }

    public void Move(string source, string destination)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(source);
            RejectSyntheticDirectoryControlMutation(destination);
            EnsureWritablePath(source);
            EnsureWritablePath(destination);

            var sourceParent = GetParentDirectory(source, createIfMissing: false);
            var sourceName = GetName(source);
            if (!sourceParent.Children.TryGetValue(sourceName, out var node))
            {
                ThrowNotFound(source);
            }

            var destinationParent = GetParentDirectory(destination, createIfMissing: true);
            var destinationName = GetName(destination);
            if (destinationParent.Children.ContainsKey(destinationName))
            {
                throw new IOException($"Destination already exists: {destination}");
            }

            sourceParent.Children.Remove(sourceName);
            node.Name = destinationName;
            destinationParent.Children[destinationName] = node;
            Touch(node);
            Touch(sourceParent);
            Touch(destinationParent);
            NotifyChanged();
        }
    }

    public void Delete(string path)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var parent = GetParentDirectory(path, createIfMissing: false);
            var name = GetName(path);
            if (!parent.Children.TryGetValue(name, out var node))
            {
                ThrowNotFound(path);
            }

            if (node is MemoryDirectoryNode directory && directory.Children.Count > 0)
            {
                throw new IOException($"Directory is not empty: {path}");
            }

            parent.Children.Remove(name);
            Touch(parent);
            NotifyChanged();
        }
    }

    public List<FileSystemEntry> ListEntriesInDirectory(string path)
    {
        lock (_gate)
        {
            var directory = GetNode(path) as MemoryDirectoryNode;
            if (directory == null)
            {
                throw new DirectoryNotFoundException(path);
            }

            return directory.Children.Values
                .OrderBy(node => node.Name, StringComparer.OrdinalIgnoreCase)
                .Select(CreateEntry)
                .ToList();
        }
    }

    public List<KeyValuePair<string, ulong>> ListDataStreams(string path)
    {
        return new List<KeyValuePair<string, ulong>>();
    }

    public Stream OpenFile(string path, FileMode mode, FileAccess access, FileShare share, FileOptions options)
    {
        lock (_gate)
        {
            var normalized = NormalizePath(path);
            if (access != FileAccess.Read || mode != FileMode.Open)
            {
                EnsureWritablePath(normalized);
            }

            var file = OpenOrCreateFileForMode(normalized, mode);
            file.LastAccessTime = DateTime.UtcNow;

            if (mode == FileMode.Append)
            {
                return new InMemoryNodeStream(this, file, _gate, writable: true, append: true);
            }

            var writable = access != FileAccess.Read;
            return new InMemoryNodeStream(this, file, _gate, writable, append: false);
        }
    }

    public void SetAttributes(string path, bool? isHidden, bool? isReadonly, bool? isArchived)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var node = GetNode(path);
            if (isHidden.HasValue)
            {
                node.IsHidden = isHidden.Value;
            }

            if (isReadonly.HasValue)
            {
                node.IsReadonly = isReadonly.Value;
            }

            if (isArchived.HasValue)
            {
                node.IsArchived = isArchived.Value;
            }

            Touch(node);
            NotifyChanged();
        }
    }

    public void SetDates(string path, DateTime? creationDT, DateTime? lastWriteDT, DateTime? lastAccessDT)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var node = GetNode(path);
            if (creationDT.HasValue)
            {
                node.CreationTime = creationDT.Value.ToUniversalTime();
            }

            if (lastWriteDT.HasValue)
            {
                node.LastWriteTime = lastWriteDT.Value.ToUniversalTime();
            }

            if (lastAccessDT.HasValue)
            {
                node.LastAccessTime = lastAccessDT.Value.ToUniversalTime();
            }

            NotifyChanged();
        }
    }

    public void SeedFile(string path, byte[] content)
    {
        SeedFile(path, content, metadata: null, control: null);
    }

    public void SeedFile(string path, byte[] content, SnapshotMetadata? metadata)
    {
        SeedFile(path, content, metadata, control: null);
    }

    public void SeedFile(string path, byte[] content, SnapshotMetadata? metadata, SnapshotControl? control)
    {
        lock (_gate)
        {
            var parent = GetParentDirectory(path, createIfMissing: true);
            var name = GetName(path);
            var file = new MemoryFileNode(name)
            {
                Content = content.ToArray()
            };
            ApplyMetadata(file, metadata);
            ApplyControl(file, control);
            parent.Children[name] = file;
            Touch(parent);
            NotifyChanged();
        }
    }

    public void SeedSymlink(string path, string journalTargetJson, string projectedTargetPath)
    {
        SeedSymlink(path, journalTargetJson, projectedTargetPath, metadata: null, control: null);
    }

    public void SeedSymlink(string path, string journalTargetJson, string projectedTargetPath, SnapshotMetadata? metadata, SnapshotControl? control)
    {
        lock (_gate)
        {
            var parent = GetParentDirectory(path, createIfMissing: true);
            var name = GetName(path);
            var symlink = new MemorySymlinkNode(name)
            {
                JournalTargetJson = journalTargetJson,
                ProjectedTargetPath = projectedTargetPath
            };
            ApplyMetadata(symlink, metadata);
            ApplyControl(symlink, control);
            parent.Children[name] = symlink;
            Touch(parent);
            NotifyChanged();
        }
    }

    public void SeedDirectory(string path)
    {
        SeedDirectory(path, metadata: null, control: null);
    }

    public void SeedDirectory(string path, SnapshotMetadata? metadata)
    {
        SeedDirectory(path, metadata, control: null);
    }

    public void SeedDirectory(string path, SnapshotMetadata? metadata, SnapshotControl? control)
    {
        if (NormalizePath(path) == "\\")
        {
            return;
        }

        lock (_gate)
        {
            var parent = GetParentDirectory(path, createIfMissing: true);
            var name = GetName(path);
            if (parent.Children.TryGetValue(name, out var existing))
            {
                if (existing is not MemoryDirectoryNode existingDirectory)
                {
                    throw new IOException($"File already exists: {path}");
                }

                ApplyMetadata(existingDirectory, metadata);
                ApplyControl(existingDirectory, control);
                return;
            }

            var directory = new MemoryDirectoryNode(name);
            ApplyMetadata(directory, metadata);
            ApplyControl(directory, control);
            parent.Children.Add(name, directory);
            Touch(parent);
            NotifyChanged();
        }
    }

    public void SetMetadata(string path, SnapshotMetadata metadata)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var node = GetNode(path);
            ApplyMetadata(node, metadata);
            NotifyChanged();
        }
    }

    public void SetControl(string path, SnapshotControl control)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var node = GetNode(path);
            ApplyControl(node, control);
            NotifyChanged();
        }
    }

    public bool TryGetSymlink(string path, out SymlinkEntryInfo symlink)
    {
        lock (_gate)
        {
            if (TryGetNode(path) is MemorySymlinkNode link)
            {
                symlink = new SymlinkEntryInfo(
                    link.ProjectedTargetPath,
                    link.JournalTargetJson,
                    link.Pinned,
                    link.ControlMode,
                    link.ControlUid,
                    link.ControlGid);
                return true;
            }
        }

        symlink = null!;
        return false;
    }

    public void CreateOrUpdateSymlink(string path, string journalTargetJson, string projectedTargetPath)
    {
        lock (_gate)
        {
            RejectSyntheticDirectoryControlMutation(path);
            EnsureWritablePath(path);
            var parent = GetParentDirectory(path, createIfMissing: true);
            var name = GetName(path);
            if (parent.Children.TryGetValue(name, out var existing) && existing is MemoryDirectoryNode directory && directory.Children.Count > 0)
            {
                throw new IOException($"Directory is not empty: {path}");
            }

            var symlink = new MemorySymlinkNode(name)
            {
                JournalTargetJson = journalTargetJson,
                ProjectedTargetPath = projectedTargetPath
            };

            if (existing != null)
            {
                symlink.IsHidden = existing.IsHidden;
                symlink.IsReadonly = existing.IsReadonly;
                symlink.IsArchived = existing.IsArchived;
                symlink.Pinned = existing.Pinned;
                symlink.ControlMode = existing.ControlMode;
                symlink.ControlUid = existing.ControlUid;
                symlink.ControlGid = existing.ControlGid;
            }

            parent.Children[name] = symlink;
            Touch(parent);
            NotifyChanged();
        }
    }

    public Snapshot ExportSnapshot()
    {
        lock (_gate)
        {
            var snapshot = new Snapshot();
            foreach (var child in _root.Children.Values.OrderBy(node => node.Name, StringComparer.OrdinalIgnoreCase))
            {
                CollectSnapshot(child, string.Empty, snapshot);
            }

            return snapshot;
        }
    }

    private MemoryFileNode OpenOrCreateFileForMode(string path, FileMode mode)
    {
        var existing = TryGetNode(path);

        switch (mode)
        {
            case FileMode.CreateNew:
                if (existing != null)
                {
                    throw new IOException($"Path already exists: {path}");
                }

                return CreateFileNode(path);
            case FileMode.Create:
                if (existing is MemoryDirectoryNode)
                {
                    throw new UnauthorizedAccessException(path);
                }

                if (existing is MemoryFileNode createFile)
                {
                    createFile.Content = Array.Empty<byte>();
                    Touch(createFile);
                    return createFile;
                }

                return CreateFileNode(path);
            case FileMode.Open:
                if (existing is MemoryFileNode openFile)
                {
                    return openFile;
                }

                ThrowNotFound(path);
                throw new FileNotFoundException(path);
            case FileMode.OpenOrCreate:
                if (existing is MemoryDirectoryNode)
                {
                    throw new UnauthorizedAccessException(path);
                }

                return existing as MemoryFileNode ?? CreateFileNode(path);
            case FileMode.Truncate:
                if (existing is MemoryFileNode truncateFile)
                {
                    truncateFile.Content = Array.Empty<byte>();
                    Touch(truncateFile);
                    return truncateFile;
                }

                ThrowNotFound(path);
                throw new FileNotFoundException(path);
            case FileMode.Append:
                if (existing is MemoryDirectoryNode)
                {
                    throw new UnauthorizedAccessException(path);
                }

                return existing as MemoryFileNode ?? CreateFileNode(path);
            default:
                throw new NotSupportedException($"Unsupported file mode: {mode}");
        }
    }

    private MemoryFileNode CreateFileNode(string path)
    {
        var parent = GetParentDirectory(path, createIfMissing: true);
        var name = GetName(path);
        var file = new MemoryFileNode(name);
        parent.Children[name] = file;
        Touch(parent);
        return file;
    }

    private MemoryNode GetNode(string path)
    {
        var normalized = NormalizePath(path);
        if (normalized == "\\")
        {
            return _root;
        }

        var current = (MemoryNode)_root;
        foreach (var segment in SplitPath(normalized))
        {
            if (current is not MemoryDirectoryNode directory || !directory.Children.TryGetValue(segment, out current!))
            {
                ThrowNotFound(path);
            }
        }

        return current;
    }

    private MemoryNode? TryGetNode(string path)
    {
        try
        {
            return GetNode(path);
        }
        catch (FileNotFoundException)
        {
            return null;
        }
        catch (DirectoryNotFoundException)
        {
            return null;
        }
    }

    private MemoryDirectoryNode GetParentDirectory(string path, bool createIfMissing)
    {
        var normalized = NormalizePath(path);
        var parentPath = GetParentPath(normalized);
        if (parentPath == "\\")
        {
            return _root;
        }

        var current = _root;
        foreach (var segment in SplitPath(parentPath))
        {
            if (!current.Children.TryGetValue(segment, out var child))
            {
                if (!createIfMissing)
                {
                    throw new DirectoryNotFoundException(path);
                }

                var directory = new MemoryDirectoryNode(segment);
                current.Children.Add(segment, directory);
                current = directory;
                continue;
            }

            if (child is not MemoryDirectoryNode childDirectory)
            {
                throw new DirectoryNotFoundException(path);
            }

            current = childDirectory;
        }

        return current;
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

    private static IEnumerable<string> SplitPath(string normalizedPath)
    {
        return normalizedPath.Trim('\\')
            .Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
    }

    private static string GetName(string path)
    {
        var normalized = NormalizePath(path);
        if (normalized == "\\")
        {
            throw new IOException("Root does not have a leaf name.");
        }

        return SplitPath(normalized).Last();
    }

    private static string? GetNameOrNull(string path)
    {
        var normalized = NormalizePath(path);
        return normalized == "\\" ? null : SplitPath(normalized).Last();
    }

    private static string GetParentPath(string normalizedPath)
    {
        var segments = SplitPath(normalizedPath).ToArray();
        if (segments.Length <= 1)
        {
            return "\\";
        }

        return "\\" + string.Join("\\", segments.Take(segments.Length - 1));
    }

    private static ulong GetUsedBytes(MemoryDirectoryNode directory)
    {
        ulong total = 0;
        foreach (var child in directory.Children.Values)
        {
            if (child is MemoryFileNode file)
            {
                total += (ulong)file.Content.Length;
            }
            else if (child is MemoryDirectoryNode childDirectory)
            {
                total += GetUsedBytes(childDirectory);
            }
        }

        return total;
    }

    private static void ThrowNotFound(string path)
    {
        throw new FileNotFoundException(path);
    }

    private bool TryGetSyntheticDirectoryControl(string path, out FileSystemEntry entry)
    {
        if (TryGetSyntheticDirectoryControl(path, out var candidate, out _))
        {
            entry = candidate;
            return true;
        }

        entry = null!;
        return false;
    }

    private bool TryGetSyntheticDirectoryControl(string path, out FileSystemEntry entry, out MemoryDirectoryNode directory)
    {
        if (GetNameOrNull(path) != DirectoryControlFileName)
        {
            entry = null!;
            directory = null!;
            return false;
        }

        directory = GetParentDirectory(path, createIfMissing: false);
        entry = CreateSyntheticDirectoryControlEntry(directory);
        return true;
    }

    private static void RejectSyntheticDirectoryControlMutation(string path)
    {
        if (GetNameOrNull(path) == DirectoryControlFileName)
        {
            throw CreateReadOnlyPathException(path);
        }
    }

    private void EnsureWritablePath(string path)
    {
        if (_isWritablePath != null && !_isWritablePath(NormalizePath(path)))
        {
            throw CreateReadOnlyPathException(path);
        }
    }

    private static UnauthorizedAccessException CreateReadOnlyPathException(string path)
    {
        return new UnauthorizedAccessException($"Read-only projected path: {NormalizePath(path)}");
    }

    private Stream OpenDirectoryControlFile(MemoryDirectoryNode directory, string path, FileMode mode, FileAccess access)
    {
        if (mode == FileMode.CreateNew)
        {
            throw new IOException($"Path already exists: {path}");
        }

        if (mode == FileMode.Append)
        {
            throw new NotSupportedException(".directory does not support append.");
        }

        var writable = access != FileAccess.Read;
        var content = Encoding.UTF8.GetBytes(BuildDirectoryControlJson(directory));
        return new DirectoryControlStream(this, directory, _gate, content, writable);
    }

    private FileSystemEntry CreateSyntheticDirectoryControlEntry(MemoryDirectoryNode directory)
    {
        var byteCount = Encoding.UTF8.GetByteCount(BuildDirectoryControlJson(directory));
        var node = new MemoryFileNode(DirectoryControlFileName)
        {
            IsHidden = true,
            Content = new byte[byteCount],
            ContentKind = "expression"
        };

        return CreateEntry(node);
    }

    private string BuildDirectoryControlJson(MemoryDirectoryNode directory)
    {
        var document = new DirectoryControlDocument
        {
            Version = 1
        };

        var directoryMeta = BuildDirectoryControlMeta(directory);
        if (directoryMeta != null)
        {
            document.Directory = new DirectoryControlDirectory
            {
                Meta = directoryMeta
            };
        }

        return JsonSerializer.Serialize(document, DirectoryControlSerializerOptions);
    }

    private static DirectoryControlMeta? BuildDirectoryControlMeta(MemoryNode node)
    {
        if (!node.ControlMode.HasValue &&
            !node.ControlUid.HasValue &&
            !node.ControlGid.HasValue)
        {
            return null;
        }

        return new DirectoryControlMeta
        {
            Mode = node.ControlMode,
            Uid = node.ControlUid,
            Gid = node.ControlGid
        };
    }

    private void ApplyDirectoryControlJson(MemoryDirectoryNode directory, string json)
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

        directory.ControlMode = null;
        directory.ControlUid = null;
        directory.ControlGid = null;

        if (root.TryGetProperty("directory", out var directoryElement))
        {
            if (directoryElement.ValueKind != JsonValueKind.Object)
            {
                throw new InvalidDataException(".directory directory must be an object.");
            }

            ValidateOnlyAllowedProperties(directoryElement, new[] { "meta" }, ".directory.directory");
            if (directoryElement.TryGetProperty("meta", out var metaElement))
            {
                ApplyControlMeta(directory, metaElement, ".directory.directory.meta");
            }
        }
    }

    private static void ApplyControlMeta(MemoryNode node, JsonElement element, string context)
    {
        if (element.ValueKind != JsonValueKind.Object)
        {
            throw new InvalidDataException($"{context} must be an object.");
        }

        ValidateOnlyAllowedProperties(element, new[] { "mode", "uid", "gid" }, context);
        node.ControlMode = ReadOptionalInteger(element, "mode", context);
        node.ControlUid = ReadOptionalInteger(element, "uid", context);
        node.ControlGid = ReadOptionalInteger(element, "gid", context);
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

    private static FileSystemEntry CreateEntry(MemoryNode node)
    {
        var entry = (FileSystemEntry?)FormatterServices.GetUninitializedObject(typeof(FileSystemEntry));
        if (entry == null)
        {
            throw new InvalidOperationException("Unable to create FileSystemEntry instance.");
        }

        SetMember(entry, "Name", node.Name);
        SetMember(entry, "CreationTime", node.CreationTime);
        SetMember(entry, "LastAccessTime", node.LastAccessTime);
        SetMember(entry, "LastWriteTime", node.LastWriteTime);
        SetMember(entry, "IsDirectory", node is MemoryDirectoryNode);
        SetMember(entry, "IsHidden", node.IsHidden);
        SetMember(entry, "IsReadonly", node.IsReadonly);
        SetMember(entry, "IsArchived", node.IsArchived);
        var size = node switch
        {
            MemoryFileNode file => (ulong)file.Content.Length,
            MemorySymlinkNode link => (ulong)Encoding.UTF8.GetByteCount(link.ProjectedTargetPath),
            _ => 0UL,
        };
        SetMember(entry, "Size", size);
        return entry;
    }

    private static void SetMember(object instance, string name, object value)
    {
        var type = instance.GetType();
        var property = type.GetProperty(name, System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
        if (property?.SetMethod != null)
        {
            property.SetValue(instance, value);
            return;
        }

        var field = type.GetField(name, System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
        if (field != null)
        {
            field.SetValue(instance, value);
            return;
        }

        throw new MissingMemberException(type.FullName, name);
    }

    private static void Touch(MemoryNode node)
    {
        var now = DateTime.UtcNow;
        node.LastWriteTime = now;
        node.LastAccessTime = now;
        node.IsArchived = true;
    }

    private void NotifyChanged()
    {
        _onChanged?.Invoke(this);
    }

    private static void CollectSnapshot(MemoryNode node, string parentPath, Snapshot snapshot)
    {
        var path = string.IsNullOrEmpty(parentPath) ? node.Name : $"{parentPath}/{node.Name}";
        if (node is MemoryDirectoryNode directory)
        {
            snapshot.Directories.Add(new SnapshotDirectory(path, CreateMetadata(directory), CreateControl(directory)));
            foreach (var child in directory.Children.Values.OrderBy(child => child.Name, StringComparer.OrdinalIgnoreCase))
            {
                CollectSnapshot(child, path, snapshot);
            }
            return;
        }

        if (node is MemoryFileNode file)
        {
            snapshot.Files.Add(new SnapshotFile(path, file.Content.ToArray(), CreateMetadata(file), CreateControl(file)));
            return;
        }

        var symlink = (MemorySymlinkNode)node;
        snapshot.Files.Add(new SnapshotFile(path, Encoding.UTF8.GetBytes(symlink.JournalTargetJson), CreateMetadata(symlink), CreateControl(symlink)));
    }

    private static SnapshotMetadata CreateMetadata(MemoryNode node)
    {
        return new SnapshotMetadata(
            node.IsHidden,
            node.IsReadonly,
            node.IsArchived,
            node.CreationTime,
            node.LastWriteTime,
            node.LastAccessTime);
    }

    private static void ApplyMetadata(MemoryNode node, SnapshotMetadata? metadata)
    {
        if (metadata == null)
        {
            return;
        }

        if (metadata.IsHidden.HasValue)
        {
            node.IsHidden = metadata.IsHidden.Value;
        }

        if (metadata.IsReadonly.HasValue)
        {
            node.IsReadonly = metadata.IsReadonly.Value;
        }

        if (metadata.IsArchived.HasValue)
        {
            node.IsArchived = metadata.IsArchived.Value;
        }

        if (metadata.CreationTimeUtc.HasValue)
        {
            node.CreationTime = metadata.CreationTimeUtc.Value.ToUniversalTime();
        }

        if (metadata.LastWriteTimeUtc.HasValue)
        {
            node.LastWriteTime = metadata.LastWriteTimeUtc.Value.ToUniversalTime();
        }

        if (metadata.LastAccessTimeUtc.HasValue)
        {
            node.LastAccessTime = metadata.LastAccessTimeUtc.Value.ToUniversalTime();
        }
    }

    private static SnapshotControl CreateControl(MemoryNode node)
    {
        return new SnapshotControl(
            node.Pinned,
            node is MemoryFileNode file ? file.ContentKind :
            node is MemorySymlinkNode ? "symlink" :
            null,
            node.ControlMode,
            node.ControlUid,
            node.ControlGid);
    }

    private static void ApplyControl(MemoryNode node, SnapshotControl? control)
    {
        if (control == null)
        {
            return;
        }

        node.Pinned = control.Pinned;
        node.ControlMode = control.Mode;
        node.ControlUid = control.Uid;
        node.ControlGid = control.Gid;
        if (control.Mode.HasValue)
        {
            node.IsReadonly = (control.Mode.Value & 146) == 0;
        }

        if (node is MemoryFileNode file && !string.IsNullOrWhiteSpace(control.ContentKind))
        {
            if (control.ContentKind is not ("bytes" or "expression"))
            {
                throw new InvalidDataException($"Unsupported content-kind: {control.ContentKind}");
            }

            file.ContentKind = control.ContentKind;
        }
        else if (node is MemorySymlinkNode && !string.IsNullOrWhiteSpace(control.ContentKind) && !string.Equals(control.ContentKind, "symlink", StringComparison.Ordinal))
        {
            throw new InvalidDataException($"Unsupported content-kind: {control.ContentKind}");
        }
    }

    private abstract class MemoryNode
    {
        protected MemoryNode(string name)
        {
            Name = name;
            CreationTime = DateTime.UtcNow;
            LastAccessTime = CreationTime;
            LastWriteTime = CreationTime;
        }

        public string Name { get; set; }
        public DateTime CreationTime { get; set; }
        public DateTime LastAccessTime { get; set; }
        public DateTime LastWriteTime { get; set; }
        public bool IsHidden { get; set; }
        public bool IsReadonly { get; set; }
        public bool IsArchived { get; set; }
        public bool? Pinned { get; set; }
        public int? ControlMode { get; set; }
        public int? ControlUid { get; set; }
        public int? ControlGid { get; set; }
    }

    private sealed class MemoryDirectoryNode : MemoryNode
    {
        public MemoryDirectoryNode(string name) : base(name)
        {
        }

        public Dictionary<string, MemoryNode> Children { get; } = new(StringComparer.OrdinalIgnoreCase);
    }

    private sealed class MemoryFileNode : MemoryNode
    {
        public MemoryFileNode(string name) : base(name)
        {
            Content = Array.Empty<byte>();
            ContentKind = "bytes";
        }

        public byte[] Content { get; set; }
        public string ContentKind { get; set; }
    }

    private sealed class MemorySymlinkNode : MemoryNode
    {
        public MemorySymlinkNode(string name) : base(name)
        {
            JournalTargetJson = "[]";
            ProjectedTargetPath = "\\";
        }

        public string JournalTargetJson { get; set; }
        public string ProjectedTargetPath { get; set; }
    }

    private sealed class InMemoryNodeStream : MemoryStream
    {
        private readonly InMemoryFileSystem _fileSystem;
        private readonly MemoryFileNode _node;
        private readonly object _gate;
        private readonly bool _writable;

        public InMemoryNodeStream(InMemoryFileSystem fileSystem, MemoryFileNode node, object gate, bool writable, bool append)
            : base()
        {
            _fileSystem = fileSystem;
            _node = node;
            _gate = gate;
            _writable = writable;
            Write(node.Content, 0, node.Content.Length);
            Position = append ? Length : 0;
            if (!writable)
            {
                _writable = false;
            }

            if (append)
            {
                Position = Length;
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && _writable)
            {
                lock (_gate)
                {
                    _node.Content = ToArray();
                    Touch(_node);
                }
                _fileSystem.NotifyChanged();
            }

            base.Dispose(disposing);
        }
    }

    private sealed class DirectoryControlStream : MemoryStream
    {
        private readonly InMemoryFileSystem _fileSystem;
        private readonly MemoryDirectoryNode _directory;
        private readonly object _gate;
        private readonly bool _writable;
        private bool _dirty;
        private bool _clearedForWrite;

        public DirectoryControlStream(InMemoryFileSystem fileSystem, MemoryDirectoryNode directory, object gate, byte[] content, bool writable)
            : base()
        {
            _fileSystem = fileSystem;
            _directory = directory;
            _gate = gate;
            _writable = writable;
            base.Write(content, 0, content.Length);
            Position = 0;
            _dirty = false;
            _clearedForWrite = false;
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            PrepareForWrite();
            base.Write(buffer, offset, count);
            _dirty = true;
        }

        public override void Write(ReadOnlySpan<byte> buffer)
        {
            PrepareForWrite();
            base.Write(buffer);
            _dirty = true;
        }

        public override void WriteByte(byte value)
        {
            PrepareForWrite();
            base.WriteByte(value);
            _dirty = true;
        }

        public override void SetLength(long value)
        {
            if (_writable)
            {
                PrepareForWrite();
                _dirty = true;
            }

            base.SetLength(value);
        }

        private void PrepareForWrite()
        {
            if (!_writable || _clearedForWrite)
            {
                return;
            }

            _clearedForWrite = true;
            base.SetLength(0);
            Position = 0;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && _writable && _dirty)
            {
                lock (_gate)
                {
                    var json = Encoding.UTF8.GetString(ToArray());
                    _fileSystem.ApplyDirectoryControlJson(_directory, json);
                    Touch(_directory);
                }

                _fileSystem.NotifyChanged();
            }

            base.Dispose(disposing);
        }
    }

    public sealed class Snapshot
    {
        public List<SnapshotDirectory> Directories { get; } = new();
        public List<SnapshotFile> Files { get; } = new();
    }

    public sealed record SnapshotDirectory(string Path, SnapshotMetadata Metadata, SnapshotControl Control);

    public sealed record SnapshotFile(string Path, byte[] Content, SnapshotMetadata Metadata, SnapshotControl Control);

    public sealed record SnapshotMetadata(
        bool? IsHidden,
        bool? IsReadonly,
        bool? IsArchived,
        DateTime? CreationTimeUtc,
        DateTime? LastWriteTimeUtc,
        DateTime? LastAccessTimeUtc);

    public sealed record SnapshotControl(
        bool? Pinned,
        string? ContentKind,
        int? Mode,
        int? Uid,
        int? Gid);

    private sealed class DirectoryControlDocument
    {
        [JsonPropertyName("version")]
        public int Version { get; set; }

        [JsonPropertyName("directory")]
        public DirectoryControlDirectory? Directory { get; set; }
    }

    private sealed class DirectoryControlDirectory
    {
        [JsonPropertyName("meta")]
        public DirectoryControlMeta? Meta { get; set; }
    }

    private sealed class DirectoryControlMeta
    {
        [JsonPropertyName("mode")]
        public int? Mode { get; set; }

        [JsonPropertyName("uid")]
        public int? Uid { get; set; }

        [JsonPropertyName("gid")]
        public int? Gid { get; set; }
    }

    private static readonly JsonSerializerOptions DirectoryControlSerializerOptions = new()
    {
        WriteIndented = true,
        DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
    };
}
