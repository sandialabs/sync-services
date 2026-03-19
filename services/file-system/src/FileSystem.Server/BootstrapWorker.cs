using Microsoft.Extensions.Options;

namespace FileSystem.Server;

public sealed class BootstrapWorker : BackgroundService
{
    private readonly ILogger<BootstrapWorker> _logger;
    private readonly ServerOptions _options;

    public BootstrapWorker(ILogger<BootstrapWorker> logger, IOptions<ServerOptions> options)
    {
        _logger = logger;
        _options = options.Value;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        EnsureStaticRoot();
        LogStartupSummary();

        while (!stoppingToken.IsCancellationRequested)
        {
            await Task.Delay(TimeSpan.FromSeconds(30), stoppingToken);
            _logger.LogInformation(
                "bootstrap heartbeat mode={Mode} share={ShareName} staticRoot={StaticRoot}",
                _options.Mode,
                _options.ShareName,
                _options.StaticRoot);
        }
    }

    private void EnsureStaticRoot()
    {
        Directory.CreateDirectory(_options.StaticRoot);

        var helloPath = Path.Combine(_options.StaticRoot, "hello.txt");
        if (!File.Exists(helloPath))
        {
            File.WriteAllText(
                helloPath,
                "Synchronic file-system bootstrap container.\nSMB serving is not implemented yet.\n");
        }

        var readmePath = Path.Combine(_options.StaticRoot, "README.txt");
        if (!File.Exists(readmePath))
        {
            File.WriteAllText(
                readmePath,
                "Stage 0 bootstrap complete. Stage 1 is a minimal SMB server over a static directory.\n");
        }
    }

    private void LogStartupSummary()
    {
        _logger.LogInformation("starting file-system bootstrap service");
        _logger.LogInformation("mode={Mode}", _options.Mode);
        _logger.LogInformation("shareName={ShareName}", _options.ShareName);
        _logger.LogInformation("port={Port}", _options.Port);
        _logger.LogInformation("staticRoot={StaticRoot}", _options.StaticRoot);
        _logger.LogInformation("gatewayBaseUrl={GatewayBaseUrl}", _options.GatewayBaseUrl);
        _logger.LogInformation("journalJsonUrl={JournalJsonUrl}", _options.JournalJsonUrl);
        _logger.LogInformation(
            "gatewayAuthTokenConfigured={GatewayAuthTokenConfigured}",
            !string.IsNullOrWhiteSpace(_options.GatewayAuthToken));
    }
}
