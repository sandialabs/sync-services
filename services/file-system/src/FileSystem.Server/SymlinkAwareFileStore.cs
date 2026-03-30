using System.IO;
using System.Linq;
using System.Text;
using DiskAccessLibrary.FileSystems.Abstractions;
using SMBLibrary;
using SMBLibrary.Adapters;
using SmbFileAttributes = SMBLibrary.FileAttributes;

namespace FileSystem.Server;

public sealed class SymlinkAwareFileStore : INTFileStore
{
    private const int BytesPerSector = 512;
    private const int ClusterSize = 4096;
    private const uint IoReparseTagSymlink = 0xA000000C;

    private readonly IFileSystem _fileSystem;
    private readonly NTFileSystemAdapter _inner;
    private readonly ISymlinkAwareFileSystem? _symlinkAware;

    public SymlinkAwareFileStore(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
        _inner = new NTFileSystemAdapter(fileSystem);
        _symlinkAware = fileSystem as ISymlinkAwareFileSystem;
    }

    public NTStatus CreateFile(out object handle, out FileStatus fileStatus, string path, AccessMask desiredAccess, SmbFileAttributes fileAttributes, ShareAccess shareAccess, CreateDisposition createDisposition, CreateOptions createOptions, SecurityContext securityContext)
    {
        handle = null!;
        fileStatus = FileStatus.FILE_DOES_NOT_EXIST;

        var normalized = NormalizePath(path);
        TraceRootPath($"CreateFile path={normalized} disposition={createDisposition} access={desiredAccess} options={createOptions}");
        if (IsRootPinFilePath(normalized) && IsCreateLikeDisposition(createDisposition))
        {
            try
            {
                _fileSystem.CreateFile(normalized);
            }
            catch (IOException)
            {
                // Existing synthetic pin file is fine; fall through to normal open/create handling.
            }
            catch (UnauthorizedAccessException exception)
            {
                return ToNtStatus(exception);
            }
            catch (NotSupportedException exception)
            {
                return ToNtStatus(exception);
            }
        }

        if (_symlinkAware == null)
        {
            return _inner.CreateFile(out handle, out fileStatus, normalized, desiredAccess, fileAttributes, shareAccess, createDisposition, createOptions, securityContext);
        }

        var openReparsePoint = (createOptions & CreateOptions.FILE_OPEN_REPARSE_POINT) > 0;
        var wantsDeleteSemantics = (desiredAccess & AccessMask.DELETE) != 0;
        if (!openReparsePoint && !wantsDeleteSemantics)
        {
            try
            {
                normalized = ResolveProjectedPath(normalized);
            }
            catch (IOException exception)
            {
                return ToNtStatus(exception);
            }
        }

        if (openReparsePoint && IsCreateLikeDisposition(createDisposition) && !PathExists(normalized))
        {
            handle = new PendingReparseHandle(normalized);
            fileStatus = FileStatus.FILE_CREATED;
            return NTStatus.STATUS_SUCCESS;
        }

        if (!_symlinkAware.TryGetSymlink(normalized, out var symlink))
        {
            return _inner.CreateFile(out handle, out fileStatus, normalized, desiredAccess, fileAttributes, shareAccess, createDisposition, createOptions, securityContext);
        }

        if (!openReparsePoint && !wantsDeleteSemantics)
        {
            return _inner.CreateFile(out handle, out fileStatus, symlink.ProjectedTargetPath, desiredAccess, fileAttributes, shareAccess, createDisposition, createOptions, securityContext);
        }

        if (createDisposition == CreateDisposition.FILE_CREATE)
        {
            fileStatus = FileStatus.FILE_EXISTS;
            return NTStatus.STATUS_OBJECT_NAME_COLLISION;
        }

        if (createDisposition is CreateDisposition.FILE_OVERWRITE or CreateDisposition.FILE_OVERWRITE_IF or CreateDisposition.FILE_SUPERSEDE)
        {
            return NTStatus.STATUS_ACCESS_DENIED;
        }

        try
        {
            var entry = _fileSystem.GetEntry(normalized);
            var targetIsDirectory = TryGetTargetIsDirectory(symlink.ProjectedTargetPath);
            var forceDirectory = (createOptions & CreateOptions.FILE_DIRECTORY_FILE) > 0;
            var forceFile = (createOptions & CreateOptions.FILE_NON_DIRECTORY_FILE) > 0;
            if (targetIsDirectory && forceFile)
            {
                return NTStatus.STATUS_FILE_IS_A_DIRECTORY;
            }

            if (!targetIsDirectory && forceDirectory)
            {
                return NTStatus.STATUS_OBJECT_PATH_INVALID;
            }

            handle = new SymlinkHandle(
                normalized,
                symlink.ProjectedTargetPath,
                symlink.JournalTargetJson,
                targetIsDirectory,
                entry);
            fileStatus = FileStatus.FILE_OPENED;
            return NTStatus.STATUS_SUCCESS;
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or NotSupportedException)
        {
            return ToNtStatus(exception);
        }
    }

    public NTStatus CloseFile(object handle)
    {
        TraceRootHandle("CloseFile", handle);
        if (handle is SymlinkHandle or PendingReparseHandle)
        {
            return NTStatus.STATUS_SUCCESS;
        }

        try
        {
            return _inner.CloseFile(handle);
        }
        catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or NotSupportedException or InvalidDataException or GatewaySemanticException)
        {
            return ToNtStatus(exception);
        }
    }

    public NTStatus ReadFile(out byte[] data, object handle, long offset, int maxCount)
    {
        if (handle is SymlinkHandle or PendingReparseHandle)
        {
            data = null!;
            return NTStatus.STATUS_ACCESS_DENIED;
        }

        return _inner.ReadFile(out data, handle, offset, maxCount);
    }

    public NTStatus WriteFile(out int numberOfBytesWritten, object handle, long offset, byte[] data)
    {
        if (handle is SymlinkHandle or PendingReparseHandle)
        {
            numberOfBytesWritten = 0;
            return NTStatus.STATUS_ACCESS_DENIED;
        }

        return _inner.WriteFile(out numberOfBytesWritten, handle, offset, data);
    }

    public NTStatus FlushFileBuffers(object handle)
    {
        return handle is SymlinkHandle or PendingReparseHandle ? NTStatus.STATUS_SUCCESS : _inner.FlushFileBuffers(handle);
    }

    public NTStatus LockFile(object handle, long byteOffset, long length, bool exclusiveLock) => _inner.LockFile(handle, byteOffset, length, exclusiveLock);

    public NTStatus UnlockFile(object handle, long byteOffset, long length) => _inner.UnlockFile(handle, byteOffset, length);

    public NTStatus QueryDirectory(out List<QueryDirectoryFileInformation> result, object handle, string fileName, FileInformationClass informationClass)
    {
        if (handle is SymlinkHandle)
        {
            result = null!;
            return NTStatus.STATUS_INVALID_PARAMETER;
        }

        var directoryHandle = (FileHandle)handle;
        if (!directoryHandle.IsDirectory)
        {
            result = null!;
            return NTStatus.STATUS_INVALID_PARAMETER;
        }

        if (fileName == string.Empty)
        {
            result = null!;
            return NTStatus.STATUS_INVALID_PARAMETER;
        }

        var path = directoryHandle.Path;
        var findExactName = !ContainsWildcardCharacters(fileName);

        List<FileSystemEntry> entries;
        if (!findExactName)
        {
            try
            {
                entries = _fileSystem.ListEntriesInDirectory(path);
            }
            catch (UnauthorizedAccessException)
            {
                result = null!;
                return NTStatus.STATUS_ACCESS_DENIED;
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or NotSupportedException)
            {
                result = null!;
                return ToNtStatus(exception);
            }

            entries = GetFiltered(entries, fileName);

            var currentDirectory = _fileSystem.GetEntry(path);
            currentDirectory = CloneWithName(currentDirectory, ".");
            var parentDirectory = _fileSystem.GetEntry(GetParentDirectory(path));
            parentDirectory = CloneWithName(parentDirectory, "..");
            entries.Insert(0, parentDirectory);
            entries.Insert(0, currentDirectory);
        }
        else
        {
            try
            {
                var entry = _fileSystem.GetEntry(CombinePath(path, fileName));
                entries = new List<FileSystemEntry> { entry };
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or NotSupportedException)
            {
                result = null!;
                return ToNtStatus(exception);
            }
        }

        try
        {
            result = entries.Select(entry => FromFileSystemEntry(entry, CombinePath(path, entry.Name), informationClass)).ToList();
            return NTStatus.STATUS_SUCCESS;
        }
        catch (UnsupportedInformationLevelException)
        {
            result = null!;
            return NTStatus.STATUS_INVALID_INFO_CLASS;
        }
    }

    public NTStatus GetFileInformation(out FileInformation result, object handle, FileInformationClass informationClass)
    {
        TraceRootHandle($"GetFileInformation infoClass={informationClass}", handle);
        if (handle is PendingReparseHandle pendingHandle)
        {
            var pendingAttributes = SmbFileAttributes.Normal;
            switch (informationClass)
            {
                case FileInformationClass.FileBasicInformation:
                {
                    result = new FileBasicInformation
                    {
                        CreationTime = null,
                        LastAccessTime = null,
                        LastWriteTime = null,
                        ChangeTime = null,
                        FileAttributes = pendingAttributes
                    };
                    return NTStatus.STATUS_SUCCESS;
                }
                case FileInformationClass.FileStandardInformation:
                {
                    result = new FileStandardInformation
                    {
                        AllocationSize = 0,
                        EndOfFile = 0,
                        Directory = false,
                        DeletePending = false
                    };
                    return NTStatus.STATUS_SUCCESS;
                }
                case FileInformationClass.FileNameInformation:
                {
                    result = new FileNameInformation { FileName = Path.GetFileName(pendingHandle.Path) };
                    return NTStatus.STATUS_SUCCESS;
                }
                case FileInformationClass.FileNetworkOpenInformation:
                {
                    result = new FileNetworkOpenInformation
                    {
                        CreationTime = null,
                        LastAccessTime = null,
                        LastWriteTime = null,
                        ChangeTime = null,
                        AllocationSize = 0,
                        EndOfFile = 0,
                        FileAttributes = pendingAttributes
                    };
                    return NTStatus.STATUS_SUCCESS;
                }
                case FileInformationClass.FileAllInformation:
                {
                    var information = new FileAllInformation();
                    information.BasicInformation.FileAttributes = pendingAttributes;
                    information.StandardInformation.AllocationSize = 0;
                    information.StandardInformation.EndOfFile = 0;
                    information.StandardInformation.Directory = false;
                    information.StandardInformation.DeletePending = false;
                    information.NameInformation.FileName = Path.GetFileName(pendingHandle.Path);
                    result = information;
                    return NTStatus.STATUS_SUCCESS;
                }
                case FileInformationClass.FileAttributeTagInformation:
                {
                    result = new FileAttributeTagInformation
                    {
                        FileAttributes = pendingAttributes,
                        ReparsePointTag = 0
                    };
                    return NTStatus.STATUS_SUCCESS;
                }
                default:
                    result = null!;
                    return NTStatus.STATUS_NOT_IMPLEMENTED;
            }
        }

        if (handle is FileHandle fileHandleWithPossibleSymlink &&
            _symlinkAware != null &&
            _symlinkAware.TryGetSymlink(fileHandleWithPossibleSymlink.Path, out var createdSymlink))
        {
            return GetFileInformation(out result, new SymlinkHandle(
                fileHandleWithPossibleSymlink.Path,
                createdSymlink.ProjectedTargetPath,
                createdSymlink.JournalTargetJson,
                TryGetTargetIsDirectory(createdSymlink.ProjectedTargetPath),
                _fileSystem.GetEntry(fileHandleWithPossibleSymlink.Path)), informationClass);
        }

        if (handle is not SymlinkHandle symlinkHandle)
        {
            return _inner.GetFileInformation(out result, handle, informationClass);
        }

        var entry = symlinkHandle.Entry;
        var attributes = GetFileAttributes(entry, isSymlink: true, symlinkHandle.TargetIsDirectory);

        switch (informationClass)
        {
            case FileInformationClass.FileBasicInformation:
            {
                result = new FileBasicInformation
                {
                    CreationTime = entry.CreationTime,
                    LastAccessTime = entry.LastAccessTime,
                    LastWriteTime = entry.LastWriteTime,
                    ChangeTime = entry.LastWriteTime,
                    FileAttributes = attributes
                };
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileStandardInformation:
            {
                result = new FileStandardInformation
                {
                    AllocationSize = (long)GetAllocationSize(entry.Size),
                    EndOfFile = (long)entry.Size,
                    Directory = symlinkHandle.TargetIsDirectory,
                    DeletePending = false
                };
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileInternalInformation:
            {
                result = new FileInternalInformation();
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileEaInformation:
            {
                result = new FileEaInformation { EaSize = 0 };
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileNameInformation:
            {
                result = new FileNameInformation { FileName = entry.Name };
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileAllInformation:
            {
                var information = new FileAllInformation();
                information.BasicInformation.CreationTime = entry.CreationTime;
                information.BasicInformation.LastAccessTime = entry.LastAccessTime;
                information.BasicInformation.LastWriteTime = entry.LastWriteTime;
                information.BasicInformation.ChangeTime = entry.LastWriteTime;
                information.BasicInformation.FileAttributes = attributes;
                information.StandardInformation.AllocationSize = (long)GetAllocationSize(entry.Size);
                information.StandardInformation.EndOfFile = (long)entry.Size;
                information.StandardInformation.Directory = symlinkHandle.TargetIsDirectory;
                information.StandardInformation.DeletePending = false;
                information.NameInformation.FileName = entry.Name;
                result = information;
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileStreamInformation:
            {
                result = new FileStreamInformation();
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileNetworkOpenInformation:
            {
                result = new FileNetworkOpenInformation
                {
                    CreationTime = entry.CreationTime,
                    LastAccessTime = entry.LastAccessTime,
                    LastWriteTime = entry.LastWriteTime,
                    ChangeTime = entry.LastWriteTime,
                    AllocationSize = (long)GetAllocationSize(entry.Size),
                    EndOfFile = (long)entry.Size,
                    FileAttributes = attributes
                };
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileAttributeTagInformation:
            {
                result = new FileAttributeTagInformation
                {
                    FileAttributes = attributes,
                    ReparsePointTag = IoReparseTagSymlink
                };
                return NTStatus.STATUS_SUCCESS;
            }
            case FileInformationClass.FileAlternateNameInformation:
                result = null!;
                return NTStatus.STATUS_OBJECT_NAME_NOT_FOUND;
            default:
                result = null!;
                return NTStatus.STATUS_NOT_IMPLEMENTED;
        }
    }

    public NTStatus SetFileInformation(object handle, FileInformation information)
    {
        TraceRootHandle($"SetFileInformation infoType={information.GetType().Name}", handle);
        if (handle is FileHandle rootFileHandle &&
            IsRootPinFilePath(rootFileHandle.Path) &&
            information is FileEndOfFileInformation)
        {
            return NTStatus.STATUS_SUCCESS;
        }

        if (handle is PendingReparseHandle)
        {
            return NTStatus.STATUS_SUCCESS;
        }

        if (handle is SymlinkHandle symlinkHandle)
        {
            if (information is FileDispositionInformation dispositionInformation && dispositionInformation.DeletePending)
            {
                try
                {
                    _fileSystem.Delete(symlinkHandle.Path);
                    return NTStatus.STATUS_SUCCESS;
                }
                catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or NotSupportedException)
                {
                    return ToNtStatus(exception);
                }
            }

            if (information is FileRenameInformationType2 renameInformation)
            {
                var newFileName = renameInformation.FileName;
                if (!newFileName.StartsWith(@"\", StringComparison.Ordinal))
                {
                    newFileName = @"\" + newFileName;
                }

                try
                {
                    _fileSystem.Move(symlinkHandle.Path, newFileName);
                    return NTStatus.STATUS_SUCCESS;
                }
                catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or NotSupportedException)
                {
                    return ToNtStatus(exception);
                }
            }

            return NTStatus.STATUS_NOT_IMPLEMENTED;
        }

        return _inner.SetFileInformation(handle, information);
    }

    public NTStatus GetFileSystemInformation(out FileSystemInformation result, FileSystemInformationClass informationClass)
    {
        var status = _inner.GetFileSystemInformation(out result, informationClass);
        if (status == NTStatus.STATUS_SUCCESS &&
            informationClass == FileSystemInformationClass.FileFsAttributeInformation &&
            result is FileFsAttributeInformation attributes)
        {
            attributes.FileSystemAttributes |= FileSystemAttributes.SupportsReparsePoints;
        }

        return status;
    }

    public NTStatus SetFileSystemInformation(FileSystemInformation information)
    {
        return _inner.SetFileSystemInformation(information);
    }

    public NTStatus GetSecurityInformation(out SecurityDescriptor result, object handle, SecurityInformation securityInformation)
    {
        return _inner.GetSecurityInformation(out result, handle, securityInformation);
    }

    public NTStatus SetSecurityInformation(object handle, SecurityInformation securityInformation, SecurityDescriptor securityDescriptor)
    {
        return _inner.SetSecurityInformation(handle, securityInformation, securityDescriptor);
    }

    public NTStatus NotifyChange(out object ioRequest, object handle, NotifyChangeFilter completionFilter, bool watchTree, int outputBufferSize, OnNotifyChangeCompleted onNotifyChangeCompleted, object context)
        => _inner.NotifyChange(out ioRequest, handle, completionFilter, watchTree, outputBufferSize, onNotifyChangeCompleted, context);

    public NTStatus Cancel(object ioRequest) => _inner.Cancel(ioRequest);

    public NTStatus DeviceIOControl(object handle, uint ctlCode, byte[] input, out byte[] output, int maxOutputLength)
    {
        TraceRootHandle($"DeviceIOControl ctlCode=0x{ctlCode:X8}", handle);
        if (handle is PendingReparseHandle pendingHandle &&
            _symlinkAware != null &&
            ctlCode == (uint)IoControlCode.FSCTL_SET_REPARSE_POINT)
        {
            try
            {
                var targetPath = ParseSymlinkTargetPath(input);
                var projectedTargetPath = NormalizeCreatedSymlinkTarget(pendingHandle.Path, targetPath);
                if (!JournalPathMapper.TryDecompileProjectedTargetPath(projectedTargetPath, out var journalTargetPath))
                {
                    output = null!;
                    return NTStatus.STATUS_OBJECT_PATH_INVALID;
                }

                var journalTargetJson = System.Text.Json.JsonSerializer.Serialize(journalTargetPath);
                _symlinkAware.CreateOrUpdateSymlink(pendingHandle.Path, journalTargetJson, projectedTargetPath);
                output = Array.Empty<byte>();
                return NTStatus.STATUS_SUCCESS;
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or InvalidDataException or NotSupportedException)
            {
                output = null!;
                return ToNtStatus(exception);
            }
        }

        if (handle is FileHandle writableHandle &&
            _symlinkAware != null &&
            ctlCode == (uint)IoControlCode.FSCTL_SET_REPARSE_POINT)
        {
            try
            {
                var targetPath = ParseSymlinkTargetPath(input);
                var projectedTargetPath = NormalizeCreatedSymlinkTarget(writableHandle.Path, targetPath);
                if (!JournalPathMapper.TryDecompileProjectedTargetPath(projectedTargetPath, out var journalTargetPath))
                {
                    output = null!;
                    return NTStatus.STATUS_OBJECT_PATH_INVALID;
                }

                var journalTargetJson = System.Text.Json.JsonSerializer.Serialize(journalTargetPath);
                _symlinkAware.CreateOrUpdateSymlink(writableHandle.Path, journalTargetJson, projectedTargetPath);
                if (writableHandle.Stream is ISuppressibleCommitStream suppressibleCommitStream)
                {
                    suppressibleCommitStream.SuppressCommit();
                }

                output = Array.Empty<byte>();
                return NTStatus.STATUS_SUCCESS;
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException or InvalidDataException or NotSupportedException)
            {
                output = null!;
                return ToNtStatus(exception);
            }
        }

        if (handle is not SymlinkHandle symlinkHandle)
        {
            return _inner.DeviceIOControl(handle, ctlCode, input, out output, maxOutputLength);
        }

        if (ctlCode != (uint)IoControlCode.FSCTL_GET_REPARSE_POINT)
        {
            output = null!;
            return NTStatus.STATUS_NOT_SUPPORTED;
        }

        output = BuildSymlinkReparseBuffer(symlinkHandle.ProjectedTargetPath, maxOutputLength);
        return output.Length > maxOutputLength ? NTStatus.STATUS_BUFFER_OVERFLOW : NTStatus.STATUS_SUCCESS;
    }

    private string ResolveProjectedPath(string path, int depth = 0)
    {
        if (_symlinkAware == null)
        {
            return NormalizePath(path);
        }

        if (depth > 16)
        {
            throw new IOException("Too many levels of symbolic links.");
        }

        var normalized = NormalizePath(path);
        var segments = normalized.Trim('\\')
            .Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        var current = "\\";
        for (var index = 0; index < segments.Length; index++)
        {
            current = CombinePath(current, segments[index]);
            if (_symlinkAware.TryGetSymlink(current, out var symlink))
            {
                var remainder = segments.Skip(index + 1);
                var combined = remainder.Aggregate(NormalizePath(symlink.ProjectedTargetPath), CombinePath);
                return ResolveProjectedPath(combined, depth + 1);
            }
        }

        return normalized;
    }

    private bool TryGetTargetIsDirectory(string projectedTargetPath)
    {
        try
        {
            return _fileSystem.GetEntry(projectedTargetPath).IsDirectory;
        }
        catch
        {
            return false;
        }
    }

    private static byte[] BuildSymlinkReparseBuffer(string targetPath, int maxOutputLength)
    {
        var printName = targetPath.Replace('/', '\\');
        var substituteName = printName;
        var substituteBytes = Encoding.Unicode.GetBytes(substituteName);
        var printBytes = Encoding.Unicode.GetBytes(printName);
        var pathBuffer = new byte[substituteBytes.Length + printBytes.Length];
        Buffer.BlockCopy(substituteBytes, 0, pathBuffer, 0, substituteBytes.Length);
        Buffer.BlockCopy(printBytes, 0, pathBuffer, substituteBytes.Length, printBytes.Length);

        var dataLength = 12 + pathBuffer.Length;
        var totalLength = 8 + dataLength;
        var buffer = new byte[Math.Min(totalLength, maxOutputLength)];
        WriteUInt32(buffer, 0, IoReparseTagSymlink);
        WriteUInt16(buffer, 4, (ushort)dataLength);
        WriteUInt16(buffer, 6, 0);
        WriteUInt16(buffer, 8, 0);
        WriteUInt16(buffer, 10, (ushort)substituteBytes.Length);
        WriteUInt16(buffer, 12, (ushort)substituteBytes.Length);
        WriteUInt16(buffer, 14, (ushort)printBytes.Length);
        WriteUInt32(buffer, 16, 0);
        Buffer.BlockCopy(pathBuffer, 0, buffer, 20, Math.Max(0, Math.Min(pathBuffer.Length, buffer.Length - 20)));
        return buffer;
    }

    private static FileSystemEntry CloneWithName(FileSystemEntry source, string name)
    {
        var clone = (FileSystemEntry)typeof(FileSystemEntry)
            .GetMethod("MemberwiseClone", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic)!
            .Invoke(source, null)!;
        var type = source.GetType();
        var property = type.GetProperty("Name", System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
        if (property?.SetMethod != null)
        {
            property.SetValue(clone, name);
            return clone;
        }

        var field = type.GetField("Name", System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
        field?.SetValue(clone, name);
        return clone;
    }

    private static List<FileSystemEntry> GetFiltered(List<FileSystemEntry> entries, string expression)
    {
        if (expression == "*")
        {
            return entries;
        }

        return entries.Where(entry => IsFileNameInExpression(entry.Name, expression)).ToList();
    }

    private static bool ContainsWildcardCharacters(string expression)
    {
        return expression.Contains("?") || expression.Contains("*") || expression.Contains("\"") || expression.Contains(">") || expression.Contains("<");
    }

    private static bool IsFileNameInExpression(string fileName, string expression)
    {
        if (expression == "*")
        {
            return true;
        }

        if (expression.EndsWith("*", StringComparison.Ordinal))
        {
            var desiredStart = expression[..^1];
            var exactWithoutExtension = false;
            if (desiredStart.EndsWith("\"", StringComparison.Ordinal))
            {
                exactWithoutExtension = true;
                desiredStart = desiredStart[..^1];
            }

            if (!exactWithoutExtension)
            {
                return fileName.StartsWith(desiredStart, StringComparison.OrdinalIgnoreCase);
            }

            return fileName.StartsWith(desiredStart + ".", StringComparison.OrdinalIgnoreCase) ||
                   string.Equals(fileName, desiredStart, StringComparison.OrdinalIgnoreCase);
        }

        if (expression.StartsWith("<", StringComparison.Ordinal))
        {
            return fileName.EndsWith(expression[1..], StringComparison.OrdinalIgnoreCase);
        }

        return string.Equals(fileName, expression, StringComparison.OrdinalIgnoreCase);
    }

    private QueryDirectoryFileInformation FromFileSystemEntry(FileSystemEntry entry, string fullPath, FileInformationClass informationClass)
    {
        SymlinkEntryInfo? symlink = null;
        var isSymlink = _symlinkAware != null && _symlinkAware.TryGetSymlink(fullPath, out symlink);
        var attributes = GetFileAttributes(entry, isSymlink, isSymlink && symlink != null && TryGetTargetIsDirectory(symlink.ProjectedTargetPath));

        return informationClass switch
        {
            FileInformationClass.FileBothDirectoryInformation => new FileBothDirectoryInformation
            {
                CreationTime = entry.CreationTime,
                LastAccessTime = entry.LastAccessTime,
                LastWriteTime = entry.LastWriteTime,
                ChangeTime = entry.LastWriteTime,
                EndOfFile = (long)entry.Size,
                AllocationSize = (long)GetAllocationSize(entry.Size),
                FileAttributes = attributes,
                EaSize = 0,
                FileName = entry.Name,
            },
            FileInformationClass.FileDirectoryInformation => new FileDirectoryInformation
            {
                CreationTime = entry.CreationTime,
                LastAccessTime = entry.LastAccessTime,
                LastWriteTime = entry.LastWriteTime,
                ChangeTime = entry.LastWriteTime,
                EndOfFile = (long)entry.Size,
                AllocationSize = (long)GetAllocationSize(entry.Size),
                FileAttributes = attributes,
                FileName = entry.Name,
            },
            FileInformationClass.FileFullDirectoryInformation => new FileFullDirectoryInformation
            {
                CreationTime = entry.CreationTime,
                LastAccessTime = entry.LastAccessTime,
                LastWriteTime = entry.LastWriteTime,
                ChangeTime = entry.LastWriteTime,
                EndOfFile = (long)entry.Size,
                AllocationSize = (long)GetAllocationSize(entry.Size),
                FileAttributes = attributes,
                EaSize = 0,
                FileName = entry.Name,
            },
            FileInformationClass.FileIdBothDirectoryInformation => new FileIdBothDirectoryInformation
            {
                CreationTime = entry.CreationTime,
                LastAccessTime = entry.LastAccessTime,
                LastWriteTime = entry.LastWriteTime,
                ChangeTime = entry.LastWriteTime,
                EndOfFile = (long)entry.Size,
                AllocationSize = (long)GetAllocationSize(entry.Size),
                FileAttributes = attributes,
                EaSize = 0,
                FileId = 0,
                FileName = entry.Name,
            },
            FileInformationClass.FileIdFullDirectoryInformation => new FileIdFullDirectoryInformation
            {
                CreationTime = entry.CreationTime,
                LastAccessTime = entry.LastAccessTime,
                LastWriteTime = entry.LastWriteTime,
                ChangeTime = entry.LastWriteTime,
                EndOfFile = (long)entry.Size,
                AllocationSize = (long)GetAllocationSize(entry.Size),
                FileAttributes = attributes,
                EaSize = 0,
                FileId = 0,
                FileName = entry.Name,
            },
            FileInformationClass.FileNamesInformation => new FileNamesInformation
            {
                FileName = entry.Name,
            },
            _ => throw new UnsupportedInformationLevelException(),
        };
    }

    private static SmbFileAttributes GetFileAttributes(FileSystemEntry entry, bool isSymlink, bool targetIsDirectory)
    {
        var attributes = (SmbFileAttributes)0;
        if (entry.IsHidden)
        {
            attributes |= SmbFileAttributes.Hidden;
        }
        if (entry.IsReadonly)
        {
            attributes |= SmbFileAttributes.ReadOnly;
        }
        if (entry.IsArchived)
        {
            attributes |= SmbFileAttributes.Archive;
        }
        if (entry.IsDirectory || targetIsDirectory)
        {
            attributes |= SmbFileAttributes.Directory;
        }
        if (isSymlink)
        {
            attributes |= SmbFileAttributes.ReparsePoint;
        }
        if (attributes == 0)
        {
            attributes = SmbFileAttributes.Normal;
        }
        return attributes;
    }

    private static ulong GetAllocationSize(ulong size)
    {
        if (size == 0)
        {
            return 0;
        }

        var remainder = size % ClusterSize;
        return remainder == 0 ? size : size + ClusterSize - remainder;
    }

    private static NTStatus ToNtStatus(Exception exception)
    {
        return exception switch
        {
            DirectoryNotFoundException => NTStatus.STATUS_OBJECT_PATH_NOT_FOUND,
            FileNotFoundException => NTStatus.STATUS_OBJECT_PATH_NOT_FOUND,
            UnauthorizedAccessException => NTStatus.STATUS_ACCESS_DENIED,
            NotSupportedException => NTStatus.STATUS_NOT_SUPPORTED,
            InvalidDataException => NTStatus.STATUS_INVALID_PARAMETER,
            GatewaySemanticException semanticException when semanticException.Code.Contains("not-found", StringComparison.OrdinalIgnoreCase) || semanticException.Code.Contains("missing", StringComparison.OrdinalIgnoreCase) || semanticException.Message.Contains("not found", StringComparison.OrdinalIgnoreCase) || semanticException.Message.Contains("missing", StringComparison.OrdinalIgnoreCase) => NTStatus.STATUS_OBJECT_PATH_NOT_FOUND,
            GatewaySemanticException => NTStatus.STATUS_INVALID_PARAMETER,
            IOException ioException when ioException.Message.Contains("already exists", StringComparison.OrdinalIgnoreCase) => NTStatus.STATUS_OBJECT_NAME_COLLISION,
            IOException ioException when ioException.Message.Contains("not empty", StringComparison.OrdinalIgnoreCase) => NTStatus.STATUS_DIRECTORY_NOT_EMPTY,
            IOException => NTStatus.STATUS_DATA_ERROR,
            _ => NTStatus.STATUS_DATA_ERROR
        };
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

    private static bool IsRootPinFilePath(string path)
    {
        var normalized = NormalizePath(path);
        return string.Equals(normalized, @"\root\pin", StringComparison.OrdinalIgnoreCase);
    }

    private static void TraceRootPath(string message)
    {
        if (!message.Contains(@"\root\pin", StringComparison.OrdinalIgnoreCase))
        {
            return;
        }

        Console.WriteLine($"[SymlinkAwareFileStore] {message}");
    }

    private static void TraceRootHandle(string operation, object handle)
    {
        switch (handle)
        {
            case FileHandle fileHandle when IsRootPinFilePath(fileHandle.Path):
                Console.WriteLine($"[SymlinkAwareFileStore] {operation} path={fileHandle.Path}");
                break;
            case SymlinkHandle symlinkHandle when IsRootPinFilePath(symlinkHandle.Path):
                Console.WriteLine($"[SymlinkAwareFileStore] {operation} path={symlinkHandle.Path}");
                break;
            case PendingReparseHandle pendingHandle when IsRootPinFilePath(pendingHandle.Path):
                Console.WriteLine($"[SymlinkAwareFileStore] {operation} path={pendingHandle.Path}");
                break;
        }
    }

    private static string CombinePath(string basePath, string segment)
    {
        var normalizedBase = NormalizePath(basePath);
        return normalizedBase == "\\" ? "\\" + segment : normalizedBase + "\\" + segment;
    }

    private static string GetParentDirectory(string path)
    {
        var normalized = NormalizePath(path);
        var lastSlash = normalized.LastIndexOf('\\');
        return lastSlash <= 0 ? "\\" : normalized[..lastSlash];
    }

    private static void WriteUInt16(byte[] buffer, int offset, ushort value)
    {
        if (offset + 2 > buffer.Length)
        {
            return;
        }

        buffer[offset] = (byte)(value & 0xFF);
        buffer[offset + 1] = (byte)(value >> 8);
    }

    private static void WriteUInt32(byte[] buffer, int offset, uint value)
    {
        if (offset + 4 > buffer.Length)
        {
            return;
        }

        buffer[offset] = (byte)(value & 0xFF);
        buffer[offset + 1] = (byte)((value >> 8) & 0xFF);
        buffer[offset + 2] = (byte)((value >> 16) & 0xFF);
        buffer[offset + 3] = (byte)(value >> 24);
    }

    private static ushort ReadUInt16(byte[] buffer, int offset)
    {
        return (ushort)(buffer[offset] | (buffer[offset + 1] << 8));
    }

    private static uint ReadUInt32(byte[] buffer, int offset)
    {
        return (uint)(buffer[offset] |
            (buffer[offset + 1] << 8) |
            (buffer[offset + 2] << 16) |
            (buffer[offset + 3] << 24));
    }

    private static string ParseSymlinkTargetPath(byte[] input)
    {
        if (input.Length < 20)
        {
            throw new InvalidDataException("Reparse buffer is too short.");
        }

        var tag = ReadUInt32(input, 0);
        if (tag != IoReparseTagSymlink)
        {
            throw new InvalidDataException($"Unsupported reparse tag: 0x{tag:X8}");
        }

        var substituteOffset = ReadUInt16(input, 8);
        var substituteLength = ReadUInt16(input, 10);
        var printOffset = ReadUInt16(input, 12);
        var printLength = ReadUInt16(input, 14);
        var pathBufferOffset = 20;

        var chosenOffset = printLength > 0 ? printOffset : substituteOffset;
        var chosenLength = printLength > 0 ? printLength : substituteLength;
        if (pathBufferOffset + chosenOffset + chosenLength > input.Length)
        {
            throw new InvalidDataException("Reparse target extends beyond input length.");
        }

        return Encoding.Unicode.GetString(input, pathBufferOffset + chosenOffset, chosenLength);
    }

    private static string NormalizeCreatedSymlinkTarget(string linkPath, string targetPath)
    {
        var target = targetPath.Replace('/', '\\').Trim();
        if (target.StartsWith(@"\??\", StringComparison.Ordinal))
        {
            target = target[4..];
        }

        if (target.StartsWith(@"UNC\", StringComparison.OrdinalIgnoreCase) ||
            target.Contains(':', StringComparison.Ordinal))
        {
            throw new IOException($"Unsupported symlink target: {targetPath}");
        }

        if (target.StartsWith("\\", StringComparison.Ordinal))
        {
            return NormalizePath(target);
        }

        var parent = GetParentDirectory(linkPath);
        var combined = CombinePath(parent, target);
        return CollapseDotSegments(combined);
    }

    private static string CollapseDotSegments(string path)
    {
        var normalized = NormalizePath(path);
        var stack = new List<string>();
        foreach (var segment in normalized.Trim('\\').Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries))
        {
            if (segment == ".")
            {
                continue;
            }

            if (segment == "..")
            {
                if (stack.Count > 0)
                {
                    stack.RemoveAt(stack.Count - 1);
                }
                continue;
            }

            stack.Add(segment);
        }

        return stack.Count == 0 ? "\\" : "\\" + string.Join("\\", stack);
    }

    private sealed record SymlinkHandle(
        string Path,
        string ProjectedTargetPath,
        string JournalTargetJson,
        bool TargetIsDirectory,
        FileSystemEntry Entry);

    private sealed record PendingReparseHandle(string Path);

    private sealed class UnsupportedInformationLevelException : Exception;

    private bool PathExists(string path)
    {
        try
        {
            _fileSystem.GetEntry(path);
            return true;
        }
        catch
        {
            return false;
        }
    }

    private static bool IsCreateLikeDisposition(CreateDisposition createDisposition)
    {
        return createDisposition is CreateDisposition.FILE_CREATE
            or CreateDisposition.FILE_OPEN_IF
            or CreateDisposition.FILE_OVERWRITE_IF
            or CreateDisposition.FILE_SUPERSEDE;
    }

}
