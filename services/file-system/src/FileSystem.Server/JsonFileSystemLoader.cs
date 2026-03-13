using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace FileSystem.Server;

public static class JsonFileSystemLoader
{
    public static InMemoryFileSystem LoadFromFile(string path, string name)
    {
        return LoadFromFile(path, name, null);
    }

    public static InMemoryFileSystem LoadFromFile(string path, string name, Action<InMemoryFileSystem>? onChanged)
    {
        var json = File.ReadAllText(path);
        return LoadFromJson(
            json,
            name,
            onChanged ?? (fileSystem => SaveToFile(fileSystem, path)));
    }

    public static InMemoryFileSystem LoadFromJson(string json, string name)
    {
        return LoadFromJson(json, name, null);
    }

    public static InMemoryFileSystem LoadFromJson(string json, string name, Action<InMemoryFileSystem>? onChanged)
    {
        var entries = LoadEntriesFromJson(json);
        return CreateFileSystem(entries, name, onChanged);
    }

    public static List<FixtureEntry> LoadEntriesFromFile(string path)
    {
        return LoadEntriesFromJson(File.ReadAllText(path));
    }

    public static List<FixtureEntry> LoadEntriesFromJson(string json)
    {
        using var document = JsonDocument.Parse(json, new JsonDocumentOptions
        {
            AllowTrailingCommas = true,
            CommentHandling = JsonCommentHandling.Skip
        });

        if (document.RootElement.ValueKind != JsonValueKind.Array)
        {
            throw new InvalidDataException("Fixture root must be a JSON array.");
        }

        var seenPaths = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var entries = new List<FixtureEntry>();

        foreach (var entryElement in document.RootElement.EnumerateArray())
        {
            var entry = ParseEntry(entryElement);
            if (!seenPaths.Add(entry.SharePath))
            {
                throw new InvalidDataException($"Duplicate fixture path: {entry.SharePath}");
            }

            entries.Add(entry);
        }

        return entries;
    }

    public static InMemoryFileSystem CreateFileSystem(
        IEnumerable<FixtureEntry> entries,
        string name,
        Action<InMemoryFileSystem>? onChanged = null)
    {
        var fileSystem = new InMemoryFileSystem(name, onChanged: onChanged, isWritablePath: JournalPathMapper.IsWritableProjectedPath);
        foreach (var entry in entries)
        {
            ApplyEntry(fileSystem, entry);
        }

        return fileSystem;
    }

    public static void SaveToFile(InMemoryFileSystem fileSystem, string path)
    {
        SaveEntriesToFile(ExportEntries(fileSystem), path);
    }

    public static void SaveEntriesToFile(IEnumerable<FixtureEntry> entries, string path)
    {
        var outputDirectory = Path.GetDirectoryName(path);
        if (!string.IsNullOrEmpty(outputDirectory))
        {
            Directory.CreateDirectory(outputDirectory);
        }

        var tempPath = path + ".tmp";
        using (var stream = File.Create(tempPath))
        using (var writer = new Utf8JsonWriter(stream, new JsonWriterOptions { Indented = true }))
        {
            writer.WriteStartArray();

            var orderedEntries = entries
                .OrderBy(entry => entry.SharePath, StringComparer.OrdinalIgnoreCase)
                .ToList();
            var children = BuildDirectoryChildren(orderedEntries);

            foreach (var directory in orderedEntries.Where(entry => entry.IsDirectory))
            {
                if (!ShouldPersistFixturePath(directory.SharePath.TrimStart('\\').Replace('\\', '/')) ||
                    !JournalPathMapper.TryDecompileProjectedPath(directory.SharePath, out _))
                {
                    continue;
                }

                WriteEntryStart(writer, directory.SharePath);
                WriteGatewayValue(writer, directory, children, details: true);
                writer.WriteEndObject();
                writer.WriteEndArray();
            }

            foreach (var file in orderedEntries.Where(entry => !entry.IsDirectory))
            {
                if (!ShouldPersistFixturePath(file.SharePath.TrimStart('\\').Replace('\\', '/')) ||
                    !JournalPathMapper.TryDecompileProjectedPath(file.SharePath, out _))
                {
                    continue;
                }

                WriteEntryStart(writer, file.SharePath);
                WriteGatewayValue(writer, file, children, details: true);
                writer.WriteEndObject();
                writer.WriteEndArray();
            }

            writer.WriteEndArray();
        }

        File.Move(tempPath, path, overwrite: true);
    }

    public static List<FixtureEntry> ExportEntries(InMemoryFileSystem fileSystem)
    {
        var snapshot = fileSystem.ExportSnapshot();
        var result = new List<FixtureEntry>();

        foreach (var directory in snapshot.Directories.OrderBy(x => x.Path, StringComparer.OrdinalIgnoreCase))
        {
            if (!ShouldPersistFixturePath(directory.Path) ||
                !JournalPathMapper.TryDecompileProjectedPath("\\" + directory.Path.TrimStart('\\'), out _))
            {
                continue;
            }

            result.Add(new FixtureEntry(
                "\\" + directory.Path.Replace('/', '\\'),
                true,
                directory.Control.Pinned ?? false,
                Array.Empty<byte>(),
                null,
                null,
                null,
                null));
        }

        foreach (var file in snapshot.Files.OrderBy(x => x.Path, StringComparer.OrdinalIgnoreCase))
        {
            if (!ShouldPersistFixturePath(file.Path) ||
                !JournalPathMapper.TryDecompileProjectedPath("\\" + file.Path.TrimStart('\\'), out _))
            {
                continue;
            }

            result.Add(new FixtureEntry(
                "\\" + file.Path.Replace('/', '\\'),
                false,
                file.Control.Pinned ?? false,
                file.Content.ToArray(),
                file.Control.ContentKind,
                file.Control.Mode,
                file.Control.Uid,
                file.Control.Gid));
        }

        return result;
    }

    public static JsonNode BuildGatewayValue(
        FixtureEntry entry,
        IReadOnlyList<FixtureEntry> allEntries,
        bool details)
    {
        var children = BuildDirectoryChildren(allEntries);
        JsonNode contentNode;
        if (entry.IsDirectory)
        {
            contentNode = BuildDirectoryContentNode(entry, children);
        }
        else if (string.Equals(entry.ContentKind, "symlink", StringComparison.OrdinalIgnoreCase))
        {
            contentNode = CreateSymlinkContentNode(Encoding.UTF8.GetString(entry.ContentBytes), entry.Mode, entry.Uid, entry.Gid);
        }
        else
        {
            contentNode = BuildFileContentNode(entry);
        }

        var value = new JsonObject
        {
            ["content"] = contentNode,
        };
        if (details)
        {
            value["pinned?"] = entry.Pinned;
        }
        return details ? value : value["content"]!.DeepClone();
    }

    public static FixtureEntry CreateEntryFromContent(string sharePath, JsonNode contentNode, bool pinned)
    {
        var normalizedPath = sharePath.StartsWith("\\", StringComparison.Ordinal)
            ? sharePath
            : "\\" + sharePath.Replace('/', '\\');
        var json = contentNode.ToJsonString();
        using var document = JsonDocument.Parse(json);
        var contentElement = document.RootElement;

        if (IsDirectoryContent(contentElement))
        {
            TryParseDirectoryEnvelopeMeta(contentElement, out var directoryMode, out var directoryUid, out var directoryGid);
            return new FixtureEntry(normalizedPath, true, pinned, Array.Empty<byte>(), null, directoryMode, directoryUid, directoryGid);
        }

        if (TryParseFileEnvelope(contentElement, out var bytes, out var contentKind, out var mode, out var uid, out var gid))
        {
            return new FixtureEntry(normalizedPath, false, pinned, bytes, contentKind, mode, uid, gid);
        }

        if (TryParseSymlinkEnvelope(contentElement, out var targetJsonBytes, out mode, out uid, out gid))
        {
            return new FixtureEntry(normalizedPath, false, pinned, targetJsonBytes, "symlink", mode, uid, gid);
        }

        var expressionBytes = Encoding.UTF8.GetBytes(contentElement.GetRawText());
        return new FixtureEntry(normalizedPath, false, pinned, expressionBytes, "expression", null, null, null);
    }

    public static bool IsNothingContent(JsonNode contentNode)
    {
        var json = contentNode.ToJsonString();
        using var document = JsonDocument.Parse(json);
        return IsNothingContent(document.RootElement);
    }

    public static JsonNode CreateByteFileContentNode(byte[] contentBytes)
    {
        return CreateByteFileContentNode(contentBytes, null, null, null);
    }

    public static JsonNode CreateByteFileContentNode(byte[] contentBytes, int? mode, int? uid, int? gid)
    {
        var byteVector = new JsonObject
        {
            ["*type/byte-vector*"] = Convert.ToHexString(contentBytes).ToLowerInvariant()
        };

        if (!mode.HasValue && !uid.HasValue && !gid.HasValue)
        {
            return byteVector;
        }

        var file = new JsonObject
        {
            ["content"] = byteVector
        };

        if (mode.HasValue || uid.HasValue || gid.HasValue)
        {
            var meta = new JsonObject();
            if (mode.HasValue)
            {
                meta["mode"] = mode.Value;
            }

            if (uid.HasValue)
            {
                meta["uid"] = uid.Value;
            }

            if (gid.HasValue)
            {
                meta["gid"] = gid.Value;
            }

            file["meta"] = meta;
        }

        return new JsonObject
        {
            ["*file-system/file*"] = file
        };
    }

    public static JsonNode CreateDirectoryContentNode()
    {
        return new JsonArray("directory", new JsonObject(), false);
    }

    public static JsonNode CreateDirectoryMarkerContentNode()
    {
        return CreateDirectoryMarkerContentNode(null, null, null);
    }

    public static JsonNode CreateDirectoryMarkerContentNode(int? mode, int? uid, int? gid)
    {
        var directory = new JsonObject();
        if (mode.HasValue || uid.HasValue || gid.HasValue)
        {
            var meta = new JsonObject();
            if (mode.HasValue)
            {
                meta["mode"] = mode.Value;
            }
            if (uid.HasValue)
            {
                meta["uid"] = uid.Value;
            }
            if (gid.HasValue)
            {
                meta["gid"] = gid.Value;
            }

            directory["meta"] = meta;
        }

        return new JsonObject
        {
            ["*file-system/directory*"] = directory
        };
    }

    public static JsonNode CreateSymlinkContentNode(string journalTargetJson, int? mode = null, int? uid = null, int? gid = null)
    {
        var targetNode = JsonNode.Parse(journalTargetJson) ?? throw new InvalidDataException("Symlink target JSON must parse to a JSON node.");
        var symlink = new JsonObject
        {
            ["target"] = targetNode
        };

        if (mode.HasValue || uid.HasValue || gid.HasValue)
        {
            var meta = new JsonObject();
            if (mode.HasValue)
            {
                meta["mode"] = mode.Value;
            }
            if (uid.HasValue)
            {
                meta["uid"] = uid.Value;
            }
            if (gid.HasValue)
            {
                meta["gid"] = gid.Value;
            }

            symlink["meta"] = meta;
        }

        return new JsonObject
        {
            ["*file-system/symlink*"] = symlink
        };
    }

    public static bool TryParseDirectoryEnvelopeMeta(JsonNode contentNode, out int? mode, out int? uid, out int? gid)
    {
        var json = contentNode.ToJsonString();
        using var document = JsonDocument.Parse(json);
        return TryParseDirectoryEnvelopeMeta(document.RootElement, out mode, out uid, out gid);
    }

    public static JsonNode CreateNothingContentNode()
    {
        return new JsonArray("nothing");
    }

    private static void ApplyEntry(InMemoryFileSystem fileSystem, FixtureEntry entry)
    {
        if (TryMirrorStageEntryToLedger(entry, out var mirrored))
        {
            ApplySingleEntry(fileSystem, mirrored);
        }

        ApplySingleEntry(fileSystem, entry);
    }

    private static void ApplySingleEntry(InMemoryFileSystem fileSystem, FixtureEntry entry)
    {
        if (entry.IsDirectory)
        {
            if (entry.SharePath == "\\")
            {
                return;
            }

            fileSystem.SeedDirectory(
                entry.SharePath,
                metadata: null,
                control: new InMemoryFileSystem.SnapshotControl(entry.Pinned, null, null, null, null));
            return;
        }

        if (string.Equals(entry.ContentKind, "symlink", StringComparison.OrdinalIgnoreCase))
        {
            var journalTargetJson = Encoding.UTF8.GetString(entry.ContentBytes);
            var projectedTargetPath = JournalPathMapper.CompileProjectedPath(JsonSerializer.Deserialize<JsonElement>(journalTargetJson));
            fileSystem.SeedSymlink(
                entry.SharePath,
                journalTargetJson,
                projectedTargetPath,
                metadata: null,
                control: new InMemoryFileSystem.SnapshotControl(
                    entry.Pinned,
                    entry.ContentKind,
                    entry.Mode,
                    entry.Uid,
                    entry.Gid));
            return;
        }

        fileSystem.SeedFile(
            entry.SharePath,
            entry.ContentBytes,
            metadata: null,
            control: new InMemoryFileSystem.SnapshotControl(
                entry.Pinned,
                entry.ContentKind,
                entry.Mode,
                entry.Uid,
                entry.Gid));
    }

    private static bool TryMirrorStageEntryToLedger(FixtureEntry entry, out FixtureEntry mirrored)
    {
        mirrored = entry;
        if (!entry.SharePath.Equals("\\stage", StringComparison.OrdinalIgnoreCase) &&
            !entry.SharePath.StartsWith("\\stage\\", StringComparison.OrdinalIgnoreCase))
        {
            return false;
        }

        var ledgerPath = "\\ledger\\state" + entry.SharePath["\\stage".Length..];
        mirrored = entry with { SharePath = ledgerPath };
        return true;
    }

    private static FixtureEntry ParseEntry(JsonElement entryElement)
    {
        if (entryElement.ValueKind != JsonValueKind.Array)
        {
            throw new InvalidDataException("Fixture entry must be a two-item array.");
        }

        var items = entryElement.EnumerateArray().ToArray();
        if (items.Length != 2)
        {
            throw new InvalidDataException("Fixture entry must contain exactly [path, value].");
        }

        var sharePath = JournalPathMapper.CompileProjectedPath(items[0]);
        var value = items[1];
        if (value.ValueKind != JsonValueKind.Object)
        {
            throw new InvalidDataException("Fixture entry value must be an object.");
        }

        if (!value.TryGetProperty("content", out var contentElement))
        {
            throw new InvalidDataException("Fixture entry value must contain content.");
        }

        var pinned = false;
        if (value.TryGetProperty("pinned?", out var pinnedElement))
        {
            if (pinnedElement.ValueKind is not JsonValueKind.True and not JsonValueKind.False)
            {
                throw new InvalidDataException("Fixture pinned? must be a boolean.");
            }

            pinned = pinnedElement.GetBoolean();
        }

        if (IsDirectoryContent(contentElement))
        {
            return new FixtureEntry(sharePath, true, pinned, Array.Empty<byte>(), null, null, null, null);
        }

        if (TryParseFileEnvelope(contentElement, out var bytes, out var contentKind, out var mode, out var uid, out var gid))
        {
            return new FixtureEntry(sharePath, false, pinned, bytes, contentKind, mode, uid, gid);
        }

        if (TryParseSymlinkEnvelope(contentElement, out var symlinkTargetBytes, out mode, out uid, out gid))
        {
            return new FixtureEntry(sharePath, false, pinned, symlinkTargetBytes, "symlink", mode, uid, gid);
        }

        var expressionBytes = Encoding.UTF8.GetBytes(contentElement.GetRawText());
        return new FixtureEntry(sharePath, false, pinned, expressionBytes, "expression", null, null, null);
    }

    private static bool IsNothingContent(JsonElement element)
    {
        return element.ValueKind == JsonValueKind.Array &&
               element.GetArrayLength() == 1 &&
               element[0].ValueKind == JsonValueKind.String &&
               string.Equals(element[0].GetString(), "nothing", StringComparison.Ordinal);
    }

    private static void WriteEntryStart(Utf8JsonWriter writer, string sharePath)
    {
        writer.WriteStartArray();
        WriteJournalPath(writer, sharePath);
        writer.WriteStartObject();
    }

    private static bool ShouldPersistFixturePath(string path)
    {
        return !path.Equals("ledger/state", StringComparison.OrdinalIgnoreCase) &&
            !path.StartsWith("ledger/state/", StringComparison.OrdinalIgnoreCase);
    }

    private static void WriteJournalPath(Utf8JsonWriter writer, string sharePath)
    {
        writer.WriteStartArray();
        foreach (var part in JournalPathMapper.DecompileProjectedPath(sharePath))
        {
            if (part is int index)
            {
                writer.WriteNumberValue(index);
                continue;
            }

            var block = (object[])part;
            writer.WriteStartArray();
            foreach (var item in block)
            {
                writer.WriteStringValue((string)item);
            }
            writer.WriteEndArray();
        }
        writer.WriteEndArray();
    }

    private static void WriteGatewayValue(
        Utf8JsonWriter writer,
        FixtureEntry entry,
        IReadOnlyDictionary<string, SortedDictionary<string, string>> children,
        bool details)
    {
        writer.WritePropertyName("content");
        if (entry.IsDirectory)
        {
            BuildDirectoryContentNode(entry, children).WriteTo(writer);
        }
        else if (string.Equals(entry.ContentKind, "symlink", StringComparison.OrdinalIgnoreCase))
        {
            CreateSymlinkContentNode(Encoding.UTF8.GetString(entry.ContentBytes), entry.Mode, entry.Uid, entry.Gid).WriteTo(writer);
        }
        else
        {
            BuildFileContentNode(entry).WriteTo(writer);
        }

        if (details)
        {
            writer.WriteBoolean("pinned?", entry.Pinned);
        }
    }

    private static JsonNode BuildDirectoryContentNode(
        FixtureEntry entry,
        IReadOnlyDictionary<string, SortedDictionary<string, string>> children)
    {
        var content = new JsonArray
        {
            "directory",
        };

        var childObject = new JsonObject();
        foreach (var child in children.GetValueOrDefault(NormalizeFixtureLookupPath(entry.SharePath), new SortedDictionary<string, string>(StringComparer.Ordinal)))
        {
            childObject[child.Key] = child.Value;
        }

        content.Add(childObject);
        content.Add(false);
        return content;
    }

    private static JsonNode BuildFileContentNode(FixtureEntry entry)
    {
        if (!HasMeta(entry))
        {
            if (string.Equals(entry.ContentKind, "expression", StringComparison.OrdinalIgnoreCase))
            {
                return BuildExpressionContentNode(entry.ContentBytes);
            }

            return new JsonObject
            {
                ["*type/byte-vector*"] = Convert.ToHexString(entry.ContentBytes).ToLowerInvariant()
            };
        }

        var fileObject = new JsonObject();
        var envelope = new JsonObject();
        fileObject["*file-system/file*"] = envelope;

        if (string.Equals(entry.ContentKind, "expression", StringComparison.OrdinalIgnoreCase))
        {
            envelope["content"] = BuildExpressionContentNode(entry.ContentBytes);
        }
        else
        {
            envelope["content"] = new JsonObject
            {
                ["*type/byte-vector*"] = Convert.ToHexString(entry.ContentBytes).ToLowerInvariant()
            };
        }

        var meta = new JsonObject();
        if (entry.Mode.HasValue)
        {
            meta["mode"] = entry.Mode.Value;
        }
        if (entry.Uid.HasValue)
        {
            meta["uid"] = entry.Uid.Value;
        }
        if (entry.Gid.HasValue)
        {
            meta["gid"] = entry.Gid.Value;
        }
        envelope["meta"] = meta;

        return fileObject;
    }

    private static JsonNode BuildExpressionContentNode(byte[] content)
    {
        try
        {
            using var document = JsonDocument.Parse(content);
            return JsonNode.Parse(document.RootElement.GetRawText())!;
        }
        catch
        {
            if (TryDecodeUtf8(content, out var text))
            {
                return JsonValue.Create(text)!;
            }

            return new JsonObject
            {
                ["*type/byte-vector*"] = Convert.ToHexString(content).ToLowerInvariant()
            };
        }
    }

    private static bool TryDecodeUtf8(byte[] content, out string text)
    {
        try
        {
            text = Encoding.UTF8.GetString(content);
            var roundTrip = Encoding.UTF8.GetBytes(text);
            return roundTrip.SequenceEqual(content);
        }
        catch
        {
            text = string.Empty;
            return false;
        }
    }

    private static bool HasMeta(FixtureEntry entry)
    {
        return entry.Mode.HasValue || entry.Uid.HasValue || entry.Gid.HasValue;
    }

    private static bool IsDirectoryContent(JsonElement contentElement)
    {
        if (contentElement.ValueKind == JsonValueKind.Object &&
            contentElement.TryGetProperty("*file-system/directory*", out var directoryEnvelope) &&
            directoryEnvelope.ValueKind == JsonValueKind.Object)
        {
            return true;
        }

        if (contentElement.ValueKind != JsonValueKind.Array)
        {
            return false;
        }

        var items = contentElement.EnumerateArray().ToArray();
        return items.Length >= 1 &&
            items[0].ValueKind == JsonValueKind.String &&
            string.Equals(items[0].GetString(), "directory", StringComparison.Ordinal);
    }

    private static bool TryParseDirectoryEnvelopeMeta(JsonElement contentElement, out int? mode, out int? uid, out int? gid)
    {
        mode = null;
        uid = null;
        gid = null;

        if (contentElement.ValueKind != JsonValueKind.Object ||
            !contentElement.TryGetProperty("*file-system/directory*", out var directoryElement) ||
            directoryElement.ValueKind != JsonValueKind.Object)
        {
            return false;
        }

        if (directoryElement.TryGetProperty("meta", out var metaElement))
        {
            if (metaElement.ValueKind != JsonValueKind.Object)
            {
                throw new InvalidDataException("Directory envelope meta must be an object.");
            }

            mode = ReadOptionalInteger(metaElement, "mode");
            uid = ReadOptionalInteger(metaElement, "uid");
            gid = ReadOptionalInteger(metaElement, "gid");
        }

        return true;
    }

    private static bool TryParseFileEnvelope(
        JsonElement contentElement,
        out byte[] contentBytes,
        out string contentKind,
        out int? mode,
        out int? uid,
        out int? gid)
    {
        contentBytes = Array.Empty<byte>();
        contentKind = "bytes";
        mode = null;
        uid = null;
        gid = null;

        if (contentElement.ValueKind == JsonValueKind.Object &&
            contentElement.TryGetProperty("*type/byte-vector*", out var directByteVectorElement) &&
            directByteVectorElement.ValueKind == JsonValueKind.String)
        {
            contentBytes = Convert.FromHexString(directByteVectorElement.GetString() ?? string.Empty);
            return true;
        }

        if (contentElement.ValueKind != JsonValueKind.Object ||
            !contentElement.TryGetProperty("*file-system/file*", out var fileElement) ||
            fileElement.ValueKind != JsonValueKind.Object)
        {
            return false;
        }

        if (!fileElement.TryGetProperty("content", out var payloadElement))
        {
            throw new InvalidDataException("File envelope is missing content.");
        }

        if (payloadElement.ValueKind == JsonValueKind.Object &&
            payloadElement.TryGetProperty("*type/byte-vector*", out var byteVectorElement) &&
            byteVectorElement.ValueKind == JsonValueKind.String)
        {
            contentBytes = Convert.FromHexString(byteVectorElement.GetString() ?? string.Empty);
            contentKind = "bytes";
        }
        else
        {
            contentBytes = Encoding.UTF8.GetBytes(payloadElement.GetRawText());
            contentKind = "expression";
        }

        if (fileElement.TryGetProperty("meta", out var metaElement))
        {
            if (metaElement.ValueKind != JsonValueKind.Object)
            {
                throw new InvalidDataException("File envelope meta must be an object.");
            }

            mode = ReadOptionalInteger(metaElement, "mode");
            uid = ReadOptionalInteger(metaElement, "uid");
            gid = ReadOptionalInteger(metaElement, "gid");
        }

        return true;
    }

    private static bool TryParseSymlinkEnvelope(
        JsonElement contentElement,
        out byte[] targetJsonBytes,
        out int? mode,
        out int? uid,
        out int? gid)
    {
        targetJsonBytes = Array.Empty<byte>();
        mode = null;
        uid = null;
        gid = null;

        if (contentElement.ValueKind != JsonValueKind.Object ||
            !contentElement.TryGetProperty("*file-system/symlink*", out var symlinkElement) ||
            symlinkElement.ValueKind != JsonValueKind.Object)
        {
            return false;
        }

        if (!symlinkElement.TryGetProperty("target", out var targetElement))
        {
            throw new InvalidDataException("Symlink envelope is missing target.");
        }

        targetJsonBytes = Encoding.UTF8.GetBytes(targetElement.GetRawText());

        if (symlinkElement.TryGetProperty("meta", out var metaElement))
        {
            if (metaElement.ValueKind != JsonValueKind.Object)
            {
                throw new InvalidDataException("Symlink envelope meta must be an object.");
            }

            mode = ReadOptionalInteger(metaElement, "mode");
            uid = ReadOptionalInteger(metaElement, "uid");
            gid = ReadOptionalInteger(metaElement, "gid");
        }

        return true;
    }

    private static int? ReadOptionalInteger(JsonElement element, string name)
    {
        if (!element.TryGetProperty(name, out var property))
        {
            return null;
        }

        if (property.ValueKind != JsonValueKind.Number || !property.TryGetInt32(out var value))
        {
            throw new InvalidDataException($"Expected integer for {name}.");
        }

        return value;
    }

    private static Dictionary<string, SortedDictionary<string, string>> BuildDirectoryChildren(IReadOnlyList<FixtureEntry> entries)
    {
        var result = new Dictionary<string, SortedDictionary<string, string>>(StringComparer.OrdinalIgnoreCase);

        foreach (var directory in entries.Where(entry => entry.IsDirectory))
        {
            result[NormalizeFixtureLookupPath(directory.SharePath)] = new SortedDictionary<string, string>(StringComparer.Ordinal);
        }

        foreach (var directory in entries.Where(entry => entry.IsDirectory))
        {
            var parent = GetParentFixturePath(directory.SharePath);
            if (parent == null)
            {
                continue;
            }

            if (!result.TryGetValue(parent, out var children))
            {
                children = new SortedDictionary<string, string>(StringComparer.Ordinal);
                result[parent] = children;
            }

            children[GetLeafName(directory.SharePath)] = "directory";
        }

        foreach (var file in entries.Where(entry => !entry.IsDirectory))
        {
            var parent = GetParentFixturePath(file.SharePath);
            if (parent == null)
            {
                continue;
            }

            if (!result.TryGetValue(parent, out var children))
            {
                children = new SortedDictionary<string, string>(StringComparer.Ordinal);
                result[parent] = children;
            }

            children[GetLeafName(file.SharePath)] = "value";
        }

        return result;
    }

    private static string? GetParentFixturePath(string path)
    {
        var segments = NormalizeFixtureLookupPath(path)
            .Split('/', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        if (segments.Length <= 1)
        {
            return null;
        }

        return string.Join("/", segments.Take(segments.Length - 1));
    }

    private static string GetLeafName(string path)
    {
        return NormalizeFixtureLookupPath(path)
            .Split('/', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .Last();
    }

    private static string NormalizeFixtureLookupPath(string path)
    {
        return path.Replace('\\', '/').Trim('/');
    }

    public sealed record FixtureEntry(
        string SharePath,
        bool IsDirectory,
        bool Pinned,
        byte[] ContentBytes,
        string? ContentKind,
        int? Mode,
        int? Uid,
        int? Gid);
}
