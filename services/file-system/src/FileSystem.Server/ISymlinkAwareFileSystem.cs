namespace FileSystem.Server;

public interface ISymlinkAwareFileSystem
{
    bool TryGetSymlink(string path, out SymlinkEntryInfo symlink);

    void CreateOrUpdateSymlink(string path, string journalTargetJson, string projectedTargetPath);
}

public sealed record SymlinkEntryInfo(
    string ProjectedTargetPath,
    string JournalTargetJson,
    bool? Pinned,
    int? Mode,
    int? Uid,
    int? Gid);
