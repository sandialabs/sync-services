using System.Text.Json;

namespace FileSystem.Server;

public static class JournalPathMapper
{
    public static string CompileProjectedPath(IReadOnlyList<object> path)
    {
        var element = JsonSerializer.SerializeToElement(path);
        return CompileProjectedPath(element);
    }

    public static string CompileProjectedPath(JsonElement pathElement)
    {
        if (pathElement.ValueKind != JsonValueKind.Array)
        {
            throw new InvalidDataException("Fixture path must be an array.");
        }

        var parts = pathElement.EnumerateArray().ToArray();
        if (parts.Length == 0)
        {
            throw new InvalidDataException("Fixture path must not be empty.");
        }

        if (parts.Length == 1)
        {
            return CompileStagePath(parts[0]);
        }

        var segments = new List<string> { "ledger" };
        var cursor = 0;

        if (parts[cursor].ValueKind == JsonValueKind.Number)
        {
            var firstIndex = parts[cursor].GetInt32();
            cursor++;

            if (cursor >= parts.Length)
            {
                throw new InvalidDataException("Ledger fixture path is incomplete.");
            }

            if (IsBridgeBlock(parts[cursor]))
            {
                segments.AddRange(CompileLedgerNodePath(parts, cursor));
                return JoinProjectedPath(segments);
            }

            segments.Add("previous");
            segments.Add(firstIndex.ToString());
            segments.AddRange(CompileLedgerNodePath(parts, cursor));
            return JoinProjectedPath(segments);
        }

        throw new InvalidDataException("Fixture path must be a stage path or a ledger path rooted by an index.");
    }

    public static bool IsWritableProjectedPath(string path)
    {
        var normalized = NormalizeProjectedPath(path);
        return normalized == "\\stage" || normalized.StartsWith("\\stage\\", StringComparison.OrdinalIgnoreCase);
    }

    public static IReadOnlyList<object> DecompileProjectedPath(string path)
    {
        if (!TryDecompileProjectedPath(path, out var result))
        {
            throw new InvalidDataException($"Projected path is not representable as a journal fixture entry: {path}");
        }

        return result;
    }

    public static bool TryDecompileProjectedTargetPath(string path, out IReadOnlyList<object> result)
    {
        if (TryDecompileProjectedPath(path, out result))
        {
            return true;
        }

        var normalized = NormalizeProjectedPath(path);
        var segments = normalized.Trim('\\')
            .Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);

        if (segments.Length >= 2 &&
            string.Equals(segments[0], "ledger", StringComparison.OrdinalIgnoreCase) &&
            string.Equals(segments[1], "state", StringComparison.OrdinalIgnoreCase))
        {
            result = new object[]
            {
                -1,
                new object[] { "*state*" }.Concat(segments.Skip(2).Cast<object>()).ToArray()
            };
            return true;
        }

        result = Array.Empty<object>();
        return false;
    }

    public static bool TryDecompileProjectedPath(string path, out IReadOnlyList<object> result)
    {
        var normalized = NormalizeProjectedPath(path);
        var segments = normalized.Trim('\\')
            .Split('\\', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);

        if (segments.Length == 0)
        {
            result = Array.Empty<object>();
            return false;
        }

        if (string.Equals(segments[0], "stage", StringComparison.OrdinalIgnoreCase))
        {
            result = new object[]
            {
                new object[] { "*state*" }.Concat(segments.Skip(1).Cast<object>()).ToArray()
            };
            return true;
        }

        if (!string.Equals(segments[0], "ledger", StringComparison.OrdinalIgnoreCase))
        {
            result = Array.Empty<object>();
            return false;
        }

        if (segments.Length >= 2 && string.Equals(segments[1], "state", StringComparison.OrdinalIgnoreCase))
        {
            result = Array.Empty<object>();
            return false;
        }

        if (!TryDecompileLedgerNodePath(segments, 1, out var parts))
        {
            result = Array.Empty<object>();
            return false;
        }

        if (parts.Count == 0)
        {
            result = Array.Empty<object>();
            return false;
        }

        if (parts[0] is int)
        {
            result = parts;
            return true;
        }

        var prefixed = new List<object> { -1 };
        prefixed.AddRange(parts);
        result = prefixed;
        return true;
    }

    private static string CompileStagePath(JsonElement block)
    {
        var segments = ReadBlockSegments(block);
        if (segments.Length == 0 || !string.Equals(segments[0], "*state*", StringComparison.Ordinal))
        {
            throw new InvalidDataException("Fixture stage path must begin with [\"*state*\", ...].");
        }

        return JoinProjectedPath(new[] { "stage" }.Concat(segments.Skip(1)));
    }

    private static IEnumerable<string> CompileLedgerNodePath(JsonElement[] parts, int cursor)
    {
        var segments = new List<string>();

        while (cursor < parts.Length)
        {
            var part = parts[cursor];
            if (part.ValueKind == JsonValueKind.Number)
            {
                segments.Add("previous");
                segments.Add(part.GetInt32().ToString());
                cursor++;
                continue;
            }

            if (IsBridgeBlock(part))
            {
                var blockSegments = ReadBlockSegments(part);
                segments.Add("bridge");
                segments.Add(blockSegments[1]);
                cursor++;
                continue;
            }

            if (IsStateBlock(part))
            {
                segments.Add("state");
                segments.AddRange(ReadBlockSegments(part).Skip(1));
                cursor++;
                if (cursor != parts.Length)
                {
                    throw new InvalidDataException("State block must terminate the fixture path.");
                }
                return segments;
            }

            throw new InvalidDataException("Unsupported ledger fixture path segment.");
        }

        throw new InvalidDataException("Ledger fixture path must terminate in a state block.");
    }

    private static bool TryDecompileLedgerNodePath(string[] segments, int cursor, out List<object> parts)
    {
        parts = new List<object>();
        if (cursor >= segments.Length)
        {
            return false;
        }

        while (cursor < segments.Length)
        {
            var segment = segments[cursor];
            if (string.Equals(segment, "state", StringComparison.OrdinalIgnoreCase))
            {
                parts.Add(new object[] { "*state*" }.Concat(segments.Skip(cursor + 1).Cast<object>()).ToArray());
                return true;
            }

            if (string.Equals(segment, "previous", StringComparison.OrdinalIgnoreCase))
            {
                if (cursor + 1 >= segments.Length || !int.TryParse(segments[cursor + 1], out var index))
                {
                    return false;
                }

                parts.Add(index);
                cursor += 2;
                continue;
            }

            if (string.Equals(segment, "bridge", StringComparison.OrdinalIgnoreCase))
            {
                if (cursor + 1 >= segments.Length)
                {
                    return false;
                }

                parts.Add(new object[] { "*bridge*", segments[cursor + 1], "chain" });
                cursor += 2;
                continue;
            }

            return false;
        }

        return false;
    }

    private static bool IsStateBlock(JsonElement element)
    {
        var segments = TryReadBlockSegments(element);
        return segments != null &&
            segments.Length > 0 &&
            string.Equals(segments[0], "*state*", StringComparison.Ordinal);
    }

    private static bool IsBridgeBlock(JsonElement element)
    {
        var segments = TryReadBlockSegments(element);
        return segments != null &&
            segments.Length == 3 &&
            string.Equals(segments[0], "*bridge*", StringComparison.Ordinal) &&
            string.Equals(segments[2], "chain", StringComparison.Ordinal);
    }

    private static string[]? TryReadBlockSegments(JsonElement block)
    {
        if (block.ValueKind != JsonValueKind.Array)
        {
            return null;
        }

        var segments = new List<string>();
        foreach (var element in block.EnumerateArray())
        {
            if (element.ValueKind != JsonValueKind.String)
            {
                return null;
            }

            segments.Add(element.GetString() ?? string.Empty);
        }

        return segments.ToArray();
    }

    private static string[] ReadBlockSegments(JsonElement block)
    {
        var segments = TryReadBlockSegments(block);
        if (segments == null)
        {
            throw new InvalidDataException("Fixture path block must be an array of strings.");
        }

        return segments;
    }

    private static string JoinProjectedPath(IEnumerable<string> segments)
    {
        var filtered = segments.Where(segment => !string.IsNullOrWhiteSpace(segment)).ToArray();
        return filtered.Length == 0 ? "\\" : "\\" + string.Join("\\", filtered);
    }

    private static string NormalizeProjectedPath(string path)
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
}
