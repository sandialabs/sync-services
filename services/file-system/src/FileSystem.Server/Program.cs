using FileSystem.Server;

var builder = Host.CreateApplicationBuilder(args);
var mode = Environment.GetEnvironmentVariable("SYNC_FS_Mode") ?? "bootstrap";

builder.Configuration.AddEnvironmentVariables(prefix: "SYNC_FS_");
builder.Services
    .AddOptions<ServerOptions>()
    .Bind(builder.Configuration)
    .ValidateDataAnnotations();
if (string.Equals(mode, "probe", StringComparison.OrdinalIgnoreCase))
{
    builder.Services.AddHostedService<SmbLibraryProbeWorker>();
}
else if (string.Equals(mode, "validate", StringComparison.OrdinalIgnoreCase))
{
    builder.Services.AddHostedService<GrammarValidationWorker>();
}
else if (string.Equals(mode, "static-smb", StringComparison.OrdinalIgnoreCase))
{
    builder.Services.AddHostedService<StaticSmbWorker>();
}
else
{
    builder.Services.AddHostedService<BootstrapWorker>();
}

var app = builder.Build();
await app.RunAsync();
