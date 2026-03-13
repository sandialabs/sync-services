# File System Service

Native SMB server service for exposing Synchronic journal state as a network-mounted filesystem.

## Documents

- [ai-spec.md](/code/sync-services/services/file-system/ai-spec.md): target behavior and MVP requirements

## Docker-first workflow

Build the image:

```bash
./tests/docker-build.sh
```

Run the container:

```bash
./tests/docker-run.sh
```

If you provide both `SYNC_FS_GATEWAY_BASE_URL` and `SYNC_FS_GATEWAY_AUTH_TOKEN` without setting `SYNC_FS_BACKEND`, `docker-run.sh` now auto-selects `http-gateway-stage`.

The default backend is now `json`, using:

```bash
tests/static-tree.json
```

That file is mounted into the container at runtime, so you can edit it locally and restart the container to try different directory/file layouts.
When `SYNC_FS_Backend=json`, SMB mutations also write the current projected tree back to that JSON file.
Avoid manually editing the fixture while the container is running, because the server currently rewrites the file on each mutation.
The committed namespace baseline lives in `tests/static-tree.baseline.json`, and `tests/reset-static-tree.sh` restores `tests/static-tree.json` from it for deterministic smoke runs.

The JSON fixture is now a flat list of:
- `[<journal-style path>, <details-like value>]`

The loader now supports journal-style entries for:
- `*state*` for `/stage/...`
- `[i, ["*state*", ...]]` for `/ledger/previous/<i>/state/...`
- `[rootIndex, ["*peer*", name, "chain"], ..., ["*state*", ...]]` for recursive `/ledger/peer/<name>/.../state/...`

The projected namespace now exposes three top-level areas:
- `/stage`
- `/ledger`
- `/control`

The current control-plane feature is:
- `/control/pin`
  - a single synthetic UTF-8 text control file
  - reads are discovery-based and render lines like `pinned /ledger/...` or `unpinned /ledger/...`
  - writes accept explicit directives in the same form
  - only `/ledger/...` paths are valid in this file
  - `/stage/...` is never pinnable through the filesystem surface

Each value object is expected to resemble a `get(..., details?=true)` result:
- `content`
- `pinned?`

The fixture currently omits `proof`.

Supported `content` shapes are:
- directory content: `["directory", { ...children... }, false]`
- file envelope: `{"*file-system/file*":{"content":...,"meta":{...}}}`
- fallback expression content: any other JSON value is treated as expression-backed file content

Supported file envelope metadata fields are:
- `mode`
- `uid`
- `gid`

The loader compiles journal-style paths into projected `/stage` and recursive `/ledger/...` SMB namespace paths and rejects duplicate entries.
The fixture model now also supports symlink entries:
- `{"*file-system/symlink*":{"target":[...journal path list...],"meta":{...}}}`
- `tests/symlink-smoke.sh` exercises SMB-side listing, info, and read-follow behavior for projected symlinks

Directory metadata lives behind the scenes on hidden `*directory*` marker entries in the backing model. That marker is not exposed to filesystem users as a projected `.directory` file.
Projected writes are currently allowed only under `/stage`; `/ledger/...` is treated as read-only in the local mock model.
Pin/unpin is the current exception: it is exposed through `/control/pin`, not through file metadata or sidecars in the content tree.
The current `json` backend now runs through a mock gateway abstraction shaped like `POST /api/v1/general/get` and `POST /api/v1/general/set`, so the SMB projection path already matches the real gateway contract more closely.

Try connecting with `smbclient` after the container starts:

```bash
smbclient //127.0.0.1/sync -N
```

On macOS, `mount_smbfs` has also been observed to work for a more native filesystem view:

```bash
mkdir -p /tmp/sync-mount
mount_smbfs //guest:@127.0.0.1/sync /tmp/sync-mount
```

That is currently the recommended manual client path when you want the share to behave more like a native mounted filesystem during exploratory testing.

## Tests

Fast deterministic tests:

```bash
dotnet test tests/FileSystem.Server.Tests/FileSystem.Server.Tests.csproj
```

Targeted local SMB integration checks:

```bash
./tests/smbclient-smoke.sh
./tests/json-projection-smoke.sh
./tests/symlink-smoke.sh
./tests/pin-control-smoke.sh
./tests/symlink-smoke.sh
```

Broader stack integration lives at the `sync-services` level:

```bash
cd /code/sync-services
./tests/local-compose.sh smoke
```

Run SMBLibrary probe mode when you need API-surface diagnostics:

```bash
SYNC_FS_MODE=probe SYNC_FS_EXIT_AFTER_STARTUP=true ./tests/docker-run.sh
docker logs sync-services-file-system-dev
```

Symlink note:
- canonical symlink targets are stored as journal path lists
- current-peer canonical targets use leading `-1` rather than a synthetic `0`
- the service now uses a custom SMB `INTFileStore` wrapper for symlink-aware projection instead of relying only on `NTFileSystemAdapter`

## Configuration

Environment variables use the `SYNC_FS_` prefix.

- `SYNC_FS_Mode`
  - default: `static-smb`
  - current supported values: `bootstrap`, `probe`, `validate`, `static-smb`
- `SYNC_FS_Port`
  - default: `445`
  - currently informational only; the first static SMB host uses SMBLibrary Direct TCP on port `445`
- `SYNC_FS_ShareName`
  - default: `sync`
- `SYNC_FS_StaticRoot`
  - default: `/srv/share`
- `SYNC_FS_Backend`
  - default: `json`
  - supported values: `json`, `mock-gateway`, `mock-gateway-readonly`, `http-gateway-readonly`, `http-gateway-stage`, `memory`, `disk`
  - helper behavior: if `SYNC_FS_GATEWAY_BASE_URL` and `SYNC_FS_GATEWAY_AUTH_TOKEN` are both set and `SYNC_FS_BACKEND` is unset, `./tests/docker-run.sh` auto-selects `http-gateway-stage`
- `SYNC_FS_JsonFixturePath`
  - default: `/workspace/tests/static-tree.json`
  - used when `SYNC_FS_Backend=json`
- `SYNC_FS_GatewayBaseUrl`
  - default: `http://gateway/api/v1`
- `SYNC_FS_GatewayAuthToken`
  - optional for now; required later for real gateway integration
- `SYNC_FS_GatewayTimeoutMs`
  - default: `10000`
- `SYNC_FS_ExitAfterStartup`
  - default: `false`
  - useful for one-shot probe runs
- `SYNC_FS_EnableSmb1`
  - default: `false`
- `SYNC_FS_EnableSmb2`
  - default: `true`
- `SYNC_FS_EnableSmb3`
  - default: `false`
- `SYNC_FS_AllowGuest`
  - default: `true`
- `SYNC_FS_GuestAccountName`
  - default: `Guest`
- `SYNC_FS_UserName`
  - optional named-user login
- `SYNC_FS_Password`
  - optional named-user password

## Notes

- Host-installed `.NET` is not required for the default workflow.
- This service is intentionally starting small. The current JSON backend is a projection scaffold that now resembles ledger `details?=true` results more closely, but it is not yet the final journal-backed namespace model.
- `json` currently runs through a mock gateway client backed by `tests/static-tree.json`, so the projection path is now aligned with future gateway integration instead of direct fixture wiring.
- A real `HttpGatewayClient` now exists and is used by the gateway-backed backends.
- Live `/stage` directory mutation now uses the spec’s hidden `*directory*` marker path in the gateway layer rather than writing a direct directory value at the visible directory path.
- The gateway-backed `/stage` write path normalizes gateway auth/missing/transport failures into filesystem-style exceptions instead of leaking raw gateway errors out of commit-time writes.
