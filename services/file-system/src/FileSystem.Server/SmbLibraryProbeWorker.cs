using System.Reflection;
using System.Runtime.Loader;
using Microsoft.Extensions.DependencyModel;
using Microsoft.Extensions.Options;

namespace FileSystem.Server;

public sealed class SmbLibraryProbeWorker : BackgroundService
{
    private readonly ILogger<SmbLibraryProbeWorker> _logger;
    private readonly IHostApplicationLifetime _lifetime;
    private readonly ServerOptions _options;

    public SmbLibraryProbeWorker(
        ILogger<SmbLibraryProbeWorker> logger,
        IHostApplicationLifetime lifetime,
        IOptions<ServerOptions> options)
    {
        _logger = logger;
        _lifetime = lifetime;
        _options = options.Value;
    }

    protected override Task ExecuteAsync(CancellationToken stoppingToken)
    {
        LogAssembly("SMBLibrary");
        LogAssembly("SMBLibrary.Adapters");
        LogDependencyAssemblies("DiskAccessLibrary");
        LogCandidateAssemblies(
            "DiskAccessLibrary",
            "DiskAccessLibrary.Core",
            "DiskAccessLibrary.FileSystems",
            "DiskAccessLibrary.FileSystems.Abstractions",
            "DiskAccessLibrary.LogicalDiskManager");
        LogSpecificTypes(
            "DiskAccessLibrary.FileSystems.Abstractions.IFileSystem, DiskAccessLibrary",
            "DiskAccessLibrary.FileSystems.Abstractions.FileSystem, DiskAccessLibrary",
            "DiskAccessLibrary.FileSystems.Abstractions.FileSystemEntry, DiskAccessLibrary");

        if (_options.ExitAfterStartup)
        {
            _logger.LogInformation("probe complete; stopping host because ExitAfterStartup=true");
            _lifetime.StopApplication();
        }

        return Task.CompletedTask;
    }

    private void LogAssembly(string assemblyName)
    {
        try
        {
            var assembly = Assembly.Load(assemblyName);
            _logger.LogInformation("loaded assembly {AssemblyName}: {FullName}", assemblyName, assembly.FullName);

            var interestingTypes = assembly
                .GetExportedTypes()
                .Where(type =>
                    type.Name.Contains("Server", StringComparison.OrdinalIgnoreCase) ||
                    type.Name.Contains("Share", StringComparison.OrdinalIgnoreCase) ||
                    type.Name.Contains("FileSystem", StringComparison.OrdinalIgnoreCase) ||
                    type.Name.Contains("Store", StringComparison.OrdinalIgnoreCase) ||
                    type.Name.Contains("Directory", StringComparison.OrdinalIgnoreCase) ||
                    type.Name.Contains("Disk", StringComparison.OrdinalIgnoreCase))
                .OrderBy(type => type.FullName)
                .ToArray();

            foreach (var type in interestingTypes)
            {
                _logger.LogInformation("type: {TypeName}", type.FullName);
                foreach (var constructor in type.GetConstructors())
                {
                    _logger.LogInformation("ctor: {Signature}", DescribeConstructor(constructor));
                }
                foreach (var method in type.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly))
                {
                    if (!IsInterestingMethod(method))
                    {
                        continue;
                    }

                    _logger.LogInformation("method: {Signature}", DescribeMethod(method));
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "failed to inspect assembly {AssemblyName}", assemblyName);
        }
    }

    private void LogDependencyAssemblies(string prefix)
    {
        try
        {
            var runtimeLibraries = DependencyContext.Default?.RuntimeLibraries
                .Where(library => library.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                .OrderBy(library => library.Name)
                .ToArray()
                ?? Array.Empty<RuntimeLibrary>();

            if (runtimeLibraries.Length == 0)
            {
                _logger.LogInformation("no runtime libraries matched prefix {Prefix}", prefix);
            }

            foreach (var runtimeLibrary in runtimeLibraries)
            {
                _logger.LogInformation("runtime library: {LibraryName}", runtimeLibrary.Name);
                try
                {
                    var assembly = AssemblyLoadContext.Default.LoadFromAssemblyName(new AssemblyName(runtimeLibrary.Name));
                    LogAssembly(assembly.GetName().Name!);
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "failed to load runtime library {LibraryName}", runtimeLibrary.Name);
                }
            }

            var loadedAssemblies = AppDomain.CurrentDomain.GetAssemblies()
                .Where(assembly => assembly.GetName().Name?.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) == true)
                .OrderBy(assembly => assembly.GetName().Name)
                .ToArray();

            if (loadedAssemblies.Length > 0)
            {
                _logger.LogInformation("loaded assemblies after dependency probe:");
            }

            foreach (var assembly in loadedAssemblies)
            {
                _logger.LogInformation("loaded assembly: {AssemblyName}", assembly.GetName().Name);
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "failed to inspect dependency assemblies with prefix {Prefix}", prefix);
        }
    }

    private void LogCandidateAssemblies(params string[] assemblyNames)
    {
        foreach (var assemblyName in assemblyNames)
        {
            LogAssembly(assemblyName);
        }
    }

    private void LogSpecificTypes(params string[] typeNames)
    {
        foreach (var typeName in typeNames)
        {
            try
            {
                var type = Type.GetType(typeName, throwOnError: false);
                if (type == null)
                {
                    _logger.LogInformation("specific type not found: {TypeName}", typeName);
                    continue;
                }

                _logger.LogInformation("specific type: {TypeName}", type.FullName);
                foreach (var constructor in type.GetConstructors())
                {
                    _logger.LogInformation("ctor: {Signature}", DescribeConstructor(constructor));
                }

                foreach (var property in type.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static))
                {
                    _logger.LogInformation(
                        "property: {PropertyType} {DeclaringType}.{PropertyName}",
                        property.PropertyType.FullName,
                        property.DeclaringType?.FullName,
                        property.Name);
                }

                foreach (var method in type.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly))
                {
                    _logger.LogInformation("method: {Signature}", DescribeMethod(method));
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "failed to inspect specific type {TypeName}", typeName);
            }
        }
    }

    private static bool IsInterestingMethod(MethodInfo method)
    {
        var name = method.Name;
        return name.Contains("Start", StringComparison.OrdinalIgnoreCase) ||
               name.Contains("Stop", StringComparison.OrdinalIgnoreCase) ||
               name.Contains("Connect", StringComparison.OrdinalIgnoreCase) ||
               name.Contains("Create", StringComparison.OrdinalIgnoreCase) ||
               name.Contains("Open", StringComparison.OrdinalIgnoreCase);
    }

    private static string DescribeConstructor(ConstructorInfo constructor)
    {
        var parameters = constructor
            .GetParameters()
            .Select(parameter => $"{parameter.ParameterType.FullName} {parameter.Name}");

        return $"{constructor.DeclaringType?.FullName}({string.Join(", ", parameters)})";
    }

    private static string DescribeMethod(MethodInfo method)
    {
        var parameters = method
            .GetParameters()
            .Select(parameter => $"{parameter.ParameterType.FullName} {parameter.Name}");

        return $"{method.ReturnType.FullName} {method.DeclaringType?.FullName}.{method.Name}({string.Join(", ", parameters)})";
    }
}
