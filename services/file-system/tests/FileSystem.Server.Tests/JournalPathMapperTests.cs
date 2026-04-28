using System.Text.Json;
using FileSystem.Server;
using Xunit;

namespace FileSystem.Server.Tests;

public sealed class JournalPathMapperTests
{
    [Theory]
    [InlineData("""[["*state*","docs","guide.txt"]]""", @"\stage\docs\guide.txt")]
    [InlineData("""[3,["*state*","archive.txt"]]""", @"\ledger\3\state\archive.txt")]
    [InlineData("""[-1,["*state*","latest.txt"]]""", @"\ledger\-1\state\latest.txt")]
    [InlineData("""[9,["*bridge*","alice","chain"],["*state*","current-remote.txt"]]""", @"\ledger\9\bridge\alice\state\current-remote.txt")]
    [InlineData("""[9,["*bridge*","alice","chain"],2,["*state*","remote-note.txt"]]""", @"\ledger\9\bridge\alice\2\state\remote-note.txt")]
    [InlineData("""[9,["*bridge*","alice","chain"],-1,["*bridge*","bob","chain"],4,["*state*","deep.txt"]]""", @"\ledger\9\bridge\alice\-1\bridge\bob\4\state\deep.txt")]
    public void CompileProjectedPath_MapsJournalShapeToProjectedPath(string pathJson, string expectedPath)
    {
        using var document = JsonDocument.Parse(pathJson);

        var projectedPath = JournalPathMapper.CompileProjectedPath(document.RootElement);

        Assert.Equal(expectedPath, projectedPath);
    }

    [Theory]
    [InlineData(@"\stage\docs\guide.txt", """[["*state*","docs","guide.txt"]]""")]
    [InlineData(@"\ledger\3\state\archive.txt", """[3,["*state*","archive.txt"]]""")]
    [InlineData(@"\ledger\-1\state\latest.txt", """[-1,["*state*","latest.txt"]]""")]
    [InlineData(@"\ledger\-1\bridge\alice\state\current-remote.txt", """[-1,["*bridge*","alice","chain"],["*state*","current-remote.txt"]]""")]
    [InlineData(@"\ledger\-1\bridge\alice\2\state\remote-note.txt", """[-1,["*bridge*","alice","chain"],2,["*state*","remote-note.txt"]]""")]
    [InlineData(@"\ledger\-1\bridge\alice\-1\bridge\bob\4\state\deep.txt", """[-1,["*bridge*","alice","chain"],-1,["*bridge*","bob","chain"],4,["*state*","deep.txt"]]""")]
    public void TryDecompileProjectedPath_MapsProjectedPathToJournalShape(string projectedPath, string expectedJson)
    {
        var success = JournalPathMapper.TryDecompileProjectedPath(projectedPath, out var result);

        Assert.True(success);
        Assert.Equal(expectedJson, JsonSerializer.Serialize(result));
    }

    [Theory]
    [InlineData(@"\ledger\bridge")]
    [InlineData(@"\ledger\bridge\alice\state\file.txt")]
    [InlineData(@"\ledger\abc\state\file.txt")]
    [InlineData(@"\ledger\2")]
    public void TryDecompileProjectedPath_RejectsInvalidProjectedPaths(string projectedPath)
    {
        var success = JournalPathMapper.TryDecompileProjectedPath(projectedPath, out _);

        Assert.False(success);
    }
}
