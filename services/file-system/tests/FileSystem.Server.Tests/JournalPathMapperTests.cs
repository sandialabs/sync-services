using System.Text.Json;
using FileSystem.Server;
using Xunit;

namespace FileSystem.Server.Tests;

public sealed class JournalPathMapperTests
{
    [Theory]
    [InlineData("""[["*state*","docs","guide.txt"]]""", @"\stage\docs\guide.txt")]
    [InlineData("""[3,["*state*","archive.txt"]]""", @"\ledger\previous\3\state\archive.txt")]
    [InlineData("""[-1,["*state*","latest.txt"]]""", @"\ledger\previous\-1\state\latest.txt")]
    [InlineData("""[9,["*peer*","alice","chain"],["*state*","current-remote.txt"]]""", @"\ledger\peer\alice\state\current-remote.txt")]
    [InlineData("""[9,["*peer*","alice","chain"],2,["*state*","remote-note.txt"]]""", @"\ledger\peer\alice\previous\2\state\remote-note.txt")]
    [InlineData("""[9,["*peer*","alice","chain"],-1,["*peer*","bob","chain"],4,["*state*","deep.txt"]]""", @"\ledger\peer\alice\previous\-1\peer\bob\previous\4\state\deep.txt")]
    public void CompileProjectedPath_MapsJournalShapeToProjectedPath(string pathJson, string expectedPath)
    {
        using var document = JsonDocument.Parse(pathJson);

        var projectedPath = JournalPathMapper.CompileProjectedPath(document.RootElement);

        Assert.Equal(expectedPath, projectedPath);
    }

    [Theory]
    [InlineData(@"\stage\docs\guide.txt", """[["*state*","docs","guide.txt"]]""")]
    [InlineData(@"\ledger\previous\3\state\archive.txt", """[3,["*state*","archive.txt"]]""")]
    [InlineData(@"\ledger\previous\-1\state\latest.txt", """[-1,["*state*","latest.txt"]]""")]
    [InlineData(@"\ledger\peer\alice\state\current-remote.txt", """[-1,["*peer*","alice","chain"],["*state*","current-remote.txt"]]""")]
    [InlineData(@"\ledger\peer\alice\previous\2\state\remote-note.txt", """[-1,["*peer*","alice","chain"],2,["*state*","remote-note.txt"]]""")]
    [InlineData(@"\ledger\peer\alice\previous\-1\peer\bob\previous\4\state\deep.txt", """[-1,["*peer*","alice","chain"],-1,["*peer*","bob","chain"],4,["*state*","deep.txt"]]""")]
    public void TryDecompileProjectedPath_MapsProjectedPathToJournalShape(string projectedPath, string expectedJson)
    {
        var success = JournalPathMapper.TryDecompileProjectedPath(projectedPath, out var result);

        Assert.True(success);
        Assert.Equal(expectedJson, JsonSerializer.Serialize(result));
    }

    [Theory]
    [InlineData(@"\ledger\peer")]
    [InlineData(@"\ledger\previous\abc\state\file.txt")]
    [InlineData(@"\ledger\previous\2")]
    public void TryDecompileProjectedPath_RejectsInvalidProjectedPaths(string projectedPath)
    {
        var success = JournalPathMapper.TryDecompileProjectedPath(projectedPath, out _);

        Assert.False(success);
    }
}
