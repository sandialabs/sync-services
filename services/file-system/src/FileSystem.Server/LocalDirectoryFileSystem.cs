using System.Reflection;
using System.Runtime.Serialization;
using DiskAccessLibrary.FileSystems.Abstractions;

namespace FileSystem.Server;

public sealed class LocalDirectoryFileSystem : IFileSystem
{
    private readonly string _rootPath;
    private readonly string _name;

    public LocalDirectoryFileSystem(string rootPath, string name)
    {
        _rootPath = Path.GetFullPath(rootPath);
        _name = name;
        Directory.CreateDirectory(_rootPath);
    }

    public string Name => _name;

    public long Size
    {
        get
        {
            var drive = new DriveInfo(Path.GetPathRoot(_rootPath)!);
            return drive.TotalSize;
        }
    }

    public long FreeSpace
    {
        get
        {
            var drive = new DriveInfo(Path.GetPathRoot(_rootPath)!);
            return drive.AvailableFreeSpace;
        }
    }

    public bool SupportsNamedStreams => false;

    public FileSystemEntry GetEntry(string path)
    {
        var fullPath = ResolvePath(path);
        if (Directory.Exists(fullPath))
        {
            return CreateEntry(new DirectoryInfo(fullPath));
        }

        if (File.Exists(fullPath))
        {
            return CreateEntry(new FileInfo(fullPath));
        }

        ThrowNotFound(fullPath);
        throw new FileNotFoundException();
    }

    public FileSystemEntry CreateFile(string path)
    {
        var fullPath = ResolvePath(path);
        EnsureParentDirectory(fullPath);
        using (File.Open(fullPath, FileMode.CreateNew, FileAccess.ReadWrite, FileShare.ReadWrite))
        {
        }

        return CreateEntry(new FileInfo(fullPath));
    }

    public FileSystemEntry CreateDirectory(string path)
    {
        var fullPath = ResolvePath(path);
        var directory = Directory.CreateDirectory(fullPath);
        return CreateEntry(directory);
    }

    public void Move(string source, string destination)
    {
        var sourcePath = ResolvePath(source);
        var destinationPath = ResolvePath(destination);
        EnsureParentDirectory(destinationPath);

        if (Directory.Exists(sourcePath))
        {
            Directory.Move(sourcePath, destinationPath);
            return;
        }

        if (File.Exists(sourcePath))
        {
            File.Move(sourcePath, destinationPath);
            return;
        }

        ThrowNotFound(sourcePath);
    }

    public void Delete(string path)
    {
        var fullPath = ResolvePath(path);
        if (Directory.Exists(fullPath))
        {
            Directory.Delete(fullPath, recursive: false);
            return;
        }

        if (File.Exists(fullPath))
        {
            File.Delete(fullPath);
            return;
        }

        ThrowNotFound(fullPath);
    }

    public List<FileSystemEntry> ListEntriesInDirectory(string path)
    {
        var fullPath = ResolvePath(path);
        if (!Directory.Exists(fullPath))
        {
            ThrowNotFound(fullPath);
        }

        var entries = new List<FileSystemEntry>();
        var directory = new DirectoryInfo(fullPath);

        foreach (var childDirectory in directory.EnumerateDirectories())
        {
            entries.Add(CreateEntry(childDirectory));
        }

        foreach (var childFile in directory.EnumerateFiles())
        {
            entries.Add(CreateEntry(childFile));
        }

        return entries;
    }

    public List<KeyValuePair<string, ulong>> ListDataStreams(string path)
    {
        return new List<KeyValuePair<string, ulong>>();
    }

    public Stream OpenFile(string path, FileMode mode, FileAccess access, FileShare share, FileOptions options)
    {
        var fullPath = ResolvePath(path);
        if (mode is FileMode.Create or FileMode.CreateNew or FileMode.OpenOrCreate or FileMode.Append)
        {
            EnsureParentDirectory(fullPath);
        }

        return new FileStream(fullPath, mode, access, share, bufferSize: 4096, options);
    }

    public void SetAttributes(string path, bool? isHidden, bool? isReadonly, bool? isArchived)
    {
        var fullPath = ResolvePath(path);
        FileAttributes attributes = File.GetAttributes(fullPath);

        if (isReadonly.HasValue)
        {
            attributes = isReadonly.Value
                ? attributes | FileAttributes.ReadOnly
                : attributes & ~FileAttributes.ReadOnly;
        }

        if (isArchived.HasValue)
        {
            attributes = isArchived.Value
                ? attributes | FileAttributes.Archive
                : attributes & ~FileAttributes.Archive;
        }

        if (isHidden.HasValue)
        {
            attributes = isHidden.Value
                ? attributes | FileAttributes.Hidden
                : attributes & ~FileAttributes.Hidden;
        }

        File.SetAttributes(fullPath, attributes);
    }

    public void SetDates(string path, DateTime? creationDT, DateTime? lastWriteDT, DateTime? lastAccessDT)
    {
        var fullPath = ResolvePath(path);

        if (creationDT.HasValue)
        {
            File.SetCreationTimeUtc(fullPath, creationDT.Value.ToUniversalTime());
        }

        if (lastWriteDT.HasValue)
        {
            File.SetLastWriteTimeUtc(fullPath, lastWriteDT.Value.ToUniversalTime());
        }

        if (lastAccessDT.HasValue)
        {
            File.SetLastAccessTimeUtc(fullPath, lastAccessDT.Value.ToUniversalTime());
        }
    }

    private string ResolvePath(string path)
    {
        var relative = path.Replace('\\', Path.DirectorySeparatorChar).TrimStart(Path.DirectorySeparatorChar);
        var combined = string.IsNullOrEmpty(relative) ? _rootPath : Path.Combine(_rootPath, relative);
        var fullPath = Path.GetFullPath(combined);
        var rootWithSeparator = _rootPath.EndsWith(Path.DirectorySeparatorChar)
            ? _rootPath
            : _rootPath + Path.DirectorySeparatorChar;

        if (!string.Equals(fullPath, _rootPath, StringComparison.Ordinal) &&
            !fullPath.StartsWith(rootWithSeparator, StringComparison.Ordinal))
        {
            throw new UnauthorizedAccessException("Path escapes the shared root.");
        }

        return fullPath;
    }

    private static void EnsureParentDirectory(string fullPath)
    {
        var parent = Path.GetDirectoryName(fullPath);
        if (!string.IsNullOrEmpty(parent))
        {
            Directory.CreateDirectory(parent);
        }
    }

    private static void ThrowNotFound(string fullPath)
    {
        var parent = Path.GetDirectoryName(fullPath);
        if (!string.IsNullOrEmpty(parent) && !Directory.Exists(parent))
        {
            throw new DirectoryNotFoundException(parent);
        }

        throw new FileNotFoundException(fullPath);
    }

    private static FileSystemEntry CreateEntry(FileSystemInfo fileSystemInfo)
    {
        var entry = (FileSystemEntry?)FormatterServices.GetUninitializedObject(typeof(FileSystemEntry));
        if (entry == null)
        {
            throw new InvalidOperationException("Unable to create FileSystemEntry instance.");
        }

        SetMember(entry, "Name", fileSystemInfo.Name);
        SetMember(entry, "CreationTime", fileSystemInfo.CreationTimeUtc);
        SetMember(entry, "LastAccessTime", fileSystemInfo.LastAccessTimeUtc);
        SetMember(entry, "LastWriteTime", fileSystemInfo.LastWriteTimeUtc);
        SetMember(entry, "IsDirectory", fileSystemInfo.Attributes.HasFlag(FileAttributes.Directory));
        SetMember(entry, "IsHidden", fileSystemInfo.Attributes.HasFlag(FileAttributes.Hidden) || fileSystemInfo.Name.StartsWith(".", StringComparison.Ordinal));
        SetMember(entry, "IsReadonly", fileSystemInfo.Attributes.HasFlag(FileAttributes.ReadOnly));
        SetMember(entry, "IsArchived", fileSystemInfo.Attributes.HasFlag(FileAttributes.Archive));
        SetMember(entry, "Size", fileSystemInfo is FileInfo fileInfo ? (ulong)fileInfo.Length : 0UL);
        return entry;
    }

    private static void SetMember(object instance, string name, object value)
    {
        var type = instance.GetType();
        var property = type.GetProperty(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        if (property?.SetMethod != null)
        {
            property.SetValue(instance, value);
            return;
        }

        var field = type.GetField(name, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        if (field != null)
        {
            field.SetValue(instance, value);
            return;
        }

        throw new MissingMemberException(type.FullName, name);
    }
}
