using System.Net;
using DiskAccessLibrary.FileSystems.Abstractions;
using Microsoft.Extensions.Options;
using SMBLibrary;
using SMBLibrary.Authentication.GSSAPI;
using SMBLibrary.Authentication.NTLM;
using SMBLibrary.Server;

namespace FileSystem.Server;

public sealed class StaticSmbWorker : BackgroundService
{
    private readonly ILogger<StaticSmbWorker> _logger;
    private readonly IHostApplicationLifetime _lifetime;
    private readonly ServerOptions _options;
    private SMBServer? _server;

    public StaticSmbWorker(
        ILogger<StaticSmbWorker> logger,
        IHostApplicationLifetime lifetime,
        IOptions<ServerOptions> options)
    {
        _logger = logger;
        _lifetime = lifetime;
        _options = options.Value;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        var fileSystem = CreateFileSystem();
        var fileStore = new SymlinkAwareFileStore(fileSystem);
        var share = new FileSystemShare(_options.ShareName, fileStore);
        share.AccessRequested += (_, args) =>
        {
            args.Allow = true;
        };

        var shares = new SMBShareCollection
        {
            share
        };

        var securityProvider = new GSSProvider(
            new IndependentNTLMAuthenticationProvider(GetUserPassword));

        _server = new SMBServer(shares, securityProvider);
        _server.ConnectionRequested += (_, args) =>
        {
            _logger.LogInformation("connection requested from {RemoteEndPoint}", args.IPEndPoint);
            args.Accept = true;
        };

        _logger.LogInformation(
            "starting static SMB server backend={Backend} share={ShareName} root={StaticRoot} directTcpPort=445 smb1={EnableSmb1} smb2={EnableSmb2} smb3={EnableSmb3}",
            _options.Backend,
            _options.ShareName,
            _options.StaticRoot,
            _options.EnableSmb1,
            _options.EnableSmb2,
            _options.EnableSmb3);

        _server.Start(IPAddress.Any, SMBTransportType.DirectTCPTransport, _options.EnableSmb1, _options.EnableSmb2, _options.EnableSmb3);

        if (_options.ExitAfterStartup)
        {
            _logger.LogInformation("static SMB server started; stopping because ExitAfterStartup=true");
            _lifetime.StopApplication();
            return;
        }

        try
        {
            await Task.Delay(Timeout.Infinite, stoppingToken);
        }
        catch (OperationCanceledException)
        {
        }
    }

    public override Task StopAsync(CancellationToken cancellationToken)
    {
        if (_server != null)
        {
            _logger.LogInformation("stopping static SMB server");
            _server.Stop();
            _server = null;
        }

        return base.StopAsync(cancellationToken);
    }

    private string? GetUserPassword(string userName)
    {
        if (_options.AllowGuest && string.Equals(userName, _options.GuestAccountName, StringComparison.OrdinalIgnoreCase))
        {
            return string.Empty;
        }

        if (!string.IsNullOrWhiteSpace(_options.UserName) &&
            string.Equals(userName, _options.UserName, StringComparison.OrdinalIgnoreCase))
        {
            return _options.Password ?? string.Empty;
        }

        return null;
    }

    private IFileSystem CreateFileSystem()
    {
        if (string.Equals(_options.Backend, "json", StringComparison.OrdinalIgnoreCase) ||
            string.Equals(_options.Backend, "mock-gateway", StringComparison.OrdinalIgnoreCase))
        {
            var gateway = new MockGatewayClient(_options.JsonFixturePath, _logger);
            return gateway.CreateProjectionFileSystem("syncfs-mock-gateway");
        }

        if (string.Equals(_options.Backend, "mock-gateway-readonly", StringComparison.OrdinalIgnoreCase))
        {
            var gateway = new MockGatewayClient(_options.JsonFixturePath, _logger);
            return new GatewayProjectionFileSystem("syncfs-mock-gateway-readonly", gateway);
        }

        if (string.Equals(_options.Backend, "http-gateway-readonly", StringComparison.OrdinalIgnoreCase))
        {
            return new GatewayProjectionFileSystem("syncfs-http-gateway-readonly", new HttpGatewayClient(_options));
        }

        if (string.Equals(_options.Backend, "http-gateway-stage", StringComparison.OrdinalIgnoreCase))
        {
            return new GatewayProjectionFileSystem("syncfs-http-gateway-stage", new HttpGatewayClient(_options), enableStageWrites: true);
        }

        if (string.Equals(_options.Backend, "http-journal-readonly", StringComparison.OrdinalIgnoreCase))
        {
            return new GatewayProjectionFileSystem("syncfs-http-journal-readonly", new HttpJournalClient(_options));
        }

        if (string.Equals(_options.Backend, "http-journal-stage", StringComparison.OrdinalIgnoreCase))
        {
            return new GatewayProjectionFileSystem("syncfs-http-journal-stage", new HttpJournalClient(_options), enableStageWrites: true);
        }

        if (string.Equals(_options.Backend, "memory", StringComparison.OrdinalIgnoreCase))
        {
            var memory = new InMemoryFileSystem("syncfs-memory");
            SeedInMemory(memory);
            return memory;
        }

        Directory.CreateDirectory(_options.StaticRoot);
        return new LocalDirectoryFileSystem(_options.StaticRoot, "syncfs-disk");
    }

    private void SeedInMemory(InMemoryFileSystem fileSystem)
    {
        fileSystem.SeedFile(
            "\\hello.txt",
            System.Text.Encoding.UTF8.GetBytes("Synchronic file-system bootstrap container.\nThis is sample static content for the future hello-world SMB share.\n"));
        fileSystem.SeedFile(
            "\\README.txt",
            System.Text.Encoding.UTF8.GetBytes("Stage 1 static SMB share backed by an in-memory tree.\n"));
    }
}
