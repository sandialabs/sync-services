# Synchronic File System AI Spec

## Objective
Expose journal-backed records as a network-mounted filesystem using a native SMB server service (SMBLibrary/.NET), with journal state as the only persistent source of truth.

## 1) Core Architecture

### Authority and persistence
- Journal is authoritative.
- SMB service is a projection/root layer.
- No local durable mirror of journal content.

### Access path
- SMB operation handlers call gateway (`/api/v1/general/*`) for reads/writes.
- User-facing typed value projection/editing is JSON-only (no Lisp text mode).

### Namespace model
- Root exposes exactly three top-level namespaces:
  - `/stage`: local mutable working tree
  - `/ledger`: immutable recursive committed-ledger view
  - `/root`: explicit non-content operational surface
- Any ledger peer view exposes exactly three structural children:
  - `/state`: committed document tree for that ledger peer view
  - `/peer/<name>`: related ledger peer
  - `/previous/<index>`: prior ledger peer view by signed integer index
- Grammar terminates once `/state` is entered.
  - All path segments after `/stage/...` or `/ledger/.../state/...` are ordinary document names.
  - Names like `stage`, `ledger`, `state`, `peer`, and `previous` are only structural before entering `state`.
- Mutability:
  - `/stage` is read-write.
  - Everything under `/ledger` is immutable for content mutation.
  - Immutable ledger paths may still expose first-class journal properties such as pin state where supported.
  - Pin/unpin is in-scope for MVP as a first-class filesystem capability, but it must map directly to journal pin state rather than any synthetic metadata sidecar.
- Root namespace:
- `/root/pin` is a synthetic root file for pin/unpin.
- `/stage` is never pinnable through the filesystem surface.

### Empty directory convention
- Directory existence marker: `*directory*` entry under directory path.
- Marker is hidden from listings.
- Legacy marker value `true` is accepted.

### SMB scope
- Service presents one or more SMB shares that expose the namespace model.
- Clients connect from Windows/macOS/Linux SMB clients.

## 2) Data Model

### Envelope tags
- `*file-system/file*`
- `*file-system/directory*`
- `*file-system/symlink*`

### Canonical value forms
- Default file value: `{"*type/byte-vector*":"<hex>"}`
- File with non-default file metadata: `{"*file-system/file*":{"content":<payload>,"meta":{...}}}`
- Directory marker value: `{"*file-system/directory*":{"meta":{...}}}`
- Symlink: `{"*file-system/symlink*":{"target":<canonical-path>,"meta":{...}}}`

### Content model (two-bucket)
- Bucket A (canonical writable file payload): `{"*type/byte-vector*":"<hex>"}`
- Bucket B (everything else, including `*type/string*`): typed value
  - exposed as deterministic JSON bytes for reads
  - treated as typed data unless explicitly converted
- Ordinary new file creation and overwrite should persist Bucket A directly.
- `*file-system/file*` should only be introduced when non-default file metadata must be persisted.

### Content-kind derivation
- `content-kind` is not stored as separate filesystem root metadata.
- It is derived from the actual journal content representation:
  - byte-vector payload => `bytes`
  - any non-byte typed value => `expression`
- New files default to byte-vector payload storage.
- Changing between `bytes` and `expression` is a content mutation, not a metadata toggle.

### Metadata fields (initial)
- `mode`, `uid`, `gid`
- `meta` is optional; defaults synthesized when absent.
- File-specific metadata belongs on `*file-system/file*`.
- Directory-specific metadata belongs on `*file-system/directory*`.
- Default file persistence should avoid `*file-system/file*` entirely when no file metadata is present.
- `mtime` is not persisted in MVP.
- SMB-visible modification time is a read-only projected default:
  - explicit per-entry time metadata is not supported
  - when clients require an `mtime`, the projection should synthesize it from `(*state* *time*)`
- Read/access time is not a persisted metadata concern in MVP.
- Hidden directory markers are not the persistence surface for time metadata.

### UID/GID interpretation (MVP)
- `uid`/`gid` are advisory metadata, not auth authority.
- Authorization is namespace + gateway auth context.

### Legacy compatibility
- Legacy non-envelope values are readable as files.
- Metadata mutation may upgrade plain file values into `*file-system/file*` format when file metadata must be persisted.

## 3) Path Grammar and Translation

### Structural grammar
- Root:
  - `/stage/<path...>`
  - `/ledger/<ledger-peer-path...>`
- Ledger peer view:
  - `/ledger/state/<path...>`
  - `/ledger/peer/<name>/<ledger-peer-path...>`
  - `/ledger/previous/<signed-integer>/<ledger-peer-path...>`
- Recursive rule:
  - after `/ledger/peer/<name>` the next segment sequence is another ledger peer view
  - after `/ledger/previous/<index>` the next segment sequence is another ledger peer view
- Signed previous indices:
  - positive and negative integers are both valid
  - negative indices are resolved directly by the server path grammar relative to the current ledger peer view
  - negative indices are not modeled as synthetic filesystem symlinks
- Terminal rule:
  - entering `/state` ends the structural grammar and all following segments are ordinary document names

### Journal translation
- Stage:
  - Projected: `/stage/<path...>`
  - Journal: `[["*state*", <path...>]]` against the mutable stage view
- Current committed ledger peer view:
  - Projected: `/ledger/state/<path...>`
  - Journal: `[rootIndex, ["*state*", <path...>]]`
- Previous ledger peer view:
  - Projected: `/ledger/previous/<i>/state/<path...>`
  - Journal: `[i, ["*state*", <path...>]]`
- Node ledger view:
  - Projected: `/ledger/peer/<name>/state/<path...>`
  - Journal:
    `[rootIndex, ["*peer*", name, "chain"], ["*state*", <path...>]]`
- Multi-hop peer + previous traversal:
  - Projected:
    `/ledger/peer/<name1>/previous/<i>/peer/<name2>/previous/<j>/state/<path...>`
  - Journal:
    `[rootIndex, ["*peer*", name1, "chain"], i, ["*peer*", name2, "chain"], j, ["*state*", <path...>]]`

### Validation failures
- Root paths other than `/stage` or `/ledger` => `EINVAL`
- Missing peer name after `/peer` => `EINVAL`
- Missing or non-integer segment after `/previous` => `EINVAL`
- Missing required terminal `state` within a ledger-peer traversal => `EINVAL`
- Any structural segment other than `state`, `peer`, or `previous` while still in ledger grammar => `EINVAL`

## 4) SMB Semantics Mapping

### Core operation mapping
- SMB read/list/stat operations map to journal `get(..., true)`.
- `/stage` content/metadata mutations map to `set!` or composed operations.
- Delete => `set!(..., ["nothing"])`.
- `mkdir` => create `*directory*` marker value.
- Pin/unpin, where exposed, must map directly to first-class journal pin/unpin behavior.
- `/root/pin` writes map to gateway `pin` / `unpin`, not `set!`.

### Read-only rules
- Content mutation outside `/stage` => `EROFS`.
- First-class journal properties such as pin state are not mutated through any directory sidecar.
- Pin/unpin may still be supported on immutable ledger paths because it is not a content mutation.
- `/root/pin` is writable only as a whole-file root surface for replacing the desired pinned set.

### Write buffering
- Per-handle in-memory buffer for `/stage` writes.
- Commit on SMB close/flush equivalent.
- Rename in `/stage` may be copy-then-delete in MVP (not crash-atomic).

### Content write semantics (`/stage`)
- Raw file writes persist bytes as byte-vector payloads by default.
- Non-byte typed values remain readable as deterministic JSON bytes.
- Expression-vs-bytes transitions are represented by content changes in the journal itself, not by directory sidecars.

### Symlinks
- Stored target is canonical journal path, not Unix text.
- Create/link operations compile projected absolute block path to canonical path.
- Read-link operations decompile canonical path to projected absolute block path.
- Loop/depth overflow => `ELOOP`; broken follow target => `ENOENT`.
- Symlink create/modify is `/stage` only.
- Relative previous-index addressing is part of path resolution, not a symlink feature.

## 5) Directory Metadata Channel

### Goal
Expose only the minimum filesystem-projection metadata that is not already represented as a first-class journal property or derivable from journal content, without requiring POSIX xattr support on clients.

### Excluded from directory metadata
- `pinned` is a first-class journal property and must not be duplicated in directory metadata.
- `content-kind` is derived from the journal content representation and must not be duplicated in directory metadata.
- Per-child file or directory metadata must not be duplicated in directory metadata.
- Time metadata must not be persisted through directory metadata.

### Recommended transport (MVP)
Use the hidden `*directory*` marker value as the persistence surface for directory-self metadata.

The projected filesystem must not expose a user-visible `.directory` file.

The hidden marker may carry:
- `meta` (optional object: `mode`, `uid`, `gid`)

Write rules:
- normal directory operations and normal SMB/POSIX-style metadata operations are the only supported user-facing mutation surface
- metadata updates apply only to the directory itself
- child entries must never be described through directory metadata
- invalid structure/key/value in marker data => `EINVAL`

### Notes
- The hidden marker exists primarily because the journal model needs an explicit empty-directory convention while SMB/POSIX clients expect directories as first-class objects.
- The hidden marker is not a manifest of the directory's contents.
- `mtime` remains a read-only projected value in MVP rather than persisted directory metadata.
- The internal metadata model remains key/value based on the hidden marker.
- Any pin/unpin user experience must be separate from hidden directory-marker metadata.

## 6) Pin Control Namespace

### Namespace shape
- `/root/pin` is a single synthetic UTF-8 text file.
- It is the only user-facing pin/unpin surface in the filesystem.
- The pin namespace must never mirror `/stage/...`.

### Read behavior
- Reading `/root/pin` returns newline-delimited pin-state records for ledger paths the filesystem has already discovered by reading those ledger entries.
- Each line is one canonical projected ledger path prefixed by either `pinned` or `unpinned`.
- The file is discovery-based rather than globally complete:
  - reading ledger files and directories may add entries to the current rendered view
  - directory listing alone does not need to enumerate the full ledger into `/root/pin`
- Blank lines are allowed but ignored on parse.
- The rendered file should be canonicalized:
  - normalized projected `/ledger/...` paths only
  - sorted deterministically
  - duplicates removed

### Write behavior
- Writing `/root/pin` applies explicit pin-state directives rather than replacing a globally complete set.
- The file content is parsed as UTF-8 newline-delimited directives:
  - `pinned /ledger/...`
  - `unpinned /ledger/...`
- Only canonical `/ledger/...` paths are allowed.
- `/stage/...` paths are invalid in this file.
- Invalid lines reject the whole write with `EINVAL`.
- Repeated directives for the same path are resolved by last-line-wins within that write.
- Rename and sidecar root files are not part of the pin UX.

### Journal mapping
- Each listed projected `/ledger/...` path decompiles to the corresponding canonical ledger journal path.
- `pinned /ledger/...` => gateway `POST /api/v1/general/pin` with `{ "path": <canonical-path> }`
- `unpinned /ledger/...` => gateway `POST /api/v1/general/unpin` with `{ "path": <canonical-path> }`
- Directory entries use the same direct ledger path mapping; recursive semantics are delegated to the journal's first-class `pin!` / `unpin!`.

## 7) Caching and Consistency

### Cache policy
- `/stage`: no cache.
- `/ledger/...`: memory-only immutable-read cache allowed.
- Per-handle partial-read streaming buffer allowed for large reads.

### Cache constraints
- Immutable caches evicted by capacity (LRU).
- Directory-marker metadata is non-cacheable.

### Failure behavior
- `/stage` upstream failure => `EIO`.
- `/ledger/...` read failure => cached value if present, else `EIO`.
- Hidden directory-marker reads/writes never use stale-cache fallback.

## 8) Error Mapping (Deterministic)
- journal `["nothing"]` => `ENOENT`
- journal `["unknown"]` => `EIO`
- auth failure => `EACCES`
- invalid path/envelope/value => `EINVAL`
- invalid hidden directory-marker data => `EINVAL`
- read-only content mutation => `EROFS`
- unsupported/disallowed root metadata op => `EOPNOTSUPP`
- unsupported operation inside `/root/pin/...` (for example rename or file-content write) => `EOPNOTSUPP`
- upstream timeout/transport failure => `EIO`

## 9) Deployment and Runtime

### Development posture
- Docker-centric development is required.
- Contributors must be able to build, run, and test the service through Docker without installing `.NET` on the host machine.
- Local developer workflows should provide a simple push-button/containerized path for:
  - building the file-system service image
  - starting the service against the existing compose stack
  - running validation/smoke tests from containers or other repo-managed helper scripts
- Host-installed `.NET` may be supported as a convenience only; it is not a required developer dependency.

### Container architecture
- Single container runs native SMB service.
- No FUSE mount.
- No Samba daemon required.

### Why this architecture
- avoids `/dev/fuse` and privileged mount requirements
- keeps compose deployment unprivileged and simpler

### Runtime requirements
- SMB service port exposure (typically 445/tcp)
- network reachability to gateway service

### Network posture
- Compose-internal HTTP to gateway for now.
- TLS for internal service-to-service traffic deferred project-wide.

## 9) Security Model (MVP)

### Auth
- Service-level gateway credentials configured at startup.
- SMB client auth model for MVP is guest mode only.
- No per-end-user passthrough to gateway in MVP.

### Enforcement boundary
- Namespace + gateway policy enforce data access.
- `uid`/`gid` do not enforce access in MVP.

### Secret hygiene
- Never persist secrets in journal content/envelopes.
- Redact secrets from logs.

### Deferred
- per-user gateway identity passthrough
- per-user ACL enforcement against journal policy
- TLS and cert lifecycle
- key rotation automation

## 10) Observability and Resilience

### Logging
Structured per-op logs include:
- operation
- namespace
- normalized path/hash
- latency
- upstream result class
- returned errno/status

No secrets or full payload content by default.

### Health
- Liveness: process active.
- Readiness: gateway reachable + basic journal call succeeds.

### Retry
- No automatic mutation retries.
- At most one bounded retry for transient read transport errors.

### Shutdown
- Graceful: best-effort flush of buffered writes.
- Crash: in-flight unflushed buffers may be lost.

### Deferred
- metrics collection/export

## 11) SMB Client Interoperability

### Target clients
- Windows native SMB client
- macOS Finder SMB client
- Linux CIFS/SMB client

### Interop expectations
- `/ledger/...` content remains read-only.
- Directory metadata is stored behind the scenes on hidden `*directory*` marker entries rather than projected root files.
- Case sensitivity rules must avoid ambiguous collisions.
- Hidden directory markers do not mirror first-class journal properties like pin state or derived content classification.
- Hidden directory markers do not mirror metadata about child entries.

### Ownership visibility note
- Client-visible ownership is often SMB stack/idmap derived and may not match envelope `uid`/`gid`.

## 12) Testing and MVP Acceptance

### Test layers
- Unit:
  - recursive ledger-peer path parser
  - signed-integer `previous/<index>` parsing
  - envelope codec/validation + legacy compatibility
  - deterministic errno mapping
- Integration (gateway + SMB service):
  - `/stage` read/write/delete/rename/mkdir/rmdir via SMB clients
  - `/ledger/...` content read-only behavior
  - pin/unpin behavior through the chosen first-class filesystem surface
  - non-byte typed values readable as deterministic JSON projections
  - byte writes produce byte-vector content
  - new file defaults to byte-vector payload storage
  - non-byte journal values remain readable without directory sidecars
  - hidden marker data rejects malformed schema/value updates (`EINVAL`)
  - hidden marker data rejects attempts to set `pinned` or `content-kind` (`EINVAL`)
  - hidden marker data rejects attempts to describe child entries (`EINVAL`)
  - hidden `*directory*` behavior
  - symlink create/read/follow with canonical path targets
  - malformed input => `EINVAL`
  - auth failure => `EACCES`
  - upstream failure => `EIO`

### Developer workflow requirement
- The default development/test workflow must be runnable with Docker-only host prerequisites.
- Repository-managed helper commands/scripts should cover the common path for build, startup, and smoke validation.
- MVP is not complete unless a developer without a host `.NET` toolchain can exercise the service through the documented container workflow.

### MVP acceptance criteria
1. Namespace correctness
- `/stage` supports content mutation.
- `/ledger/...` content is read-only.

2. Journal authority
- No local durable mirror; persistence behavior derives from journal.

3. Directory convention
- Empty directory markers work and are hidden from listings.

4. Envelope and content model
- New writes emit namespaced envelopes.
- Legacy values are readable and upgraded when needed.
- Two-bucket content model enforced.

5. Control metadata
- Hidden directory markers are limited to filesystem-projection metadata that is not already first-class or derivable from journal content.
- No user-visible `.directory` file is projected.
- Hidden directory markers do not duplicate `pinned`.
- Hidden directory markers do not duplicate `content-kind`.
- Hidden directory markers do not contain per-child metadata.
- Hidden directory markers enforce strict directory-self schema validation.

6. Symlink model
- Stored symlink target is canonical journal path.
- Client-visible read-link returns projected block path.
- Loop handling returns `ELOOP`.

7. Pin state model
- Pin/unpin is exposed, if at all, as a first-class journal-backed filesystem action rather than through file or directory sidecars.
- Pin state remains sourced from the journal and is not duplicated into hidden directory markers or file metadata sidecars.

8. Error determinism
- Stable/tested mappings for `ENOENT`, `EIO`, `EACCES`, `EINVAL`, `EROFS`, `EOPNOTSUPP`.

9. Compose interoperability
- Works in current non-TLS compose stack with unprivileged container runtime.

10. Docker-first developer usability
- A contributor can build and run the file-system service and execute its basic validation flow without installing `.NET` locally.

11. UID/GID semantics
- `uid`/`gid` persist as advisory metadata.
- Access enforcement remains namespace/gateway based.

12. Cross-network multihop reads
- At least two peer hops resolve/read correctly via recursive `/ledger/peer/...` path shape.

### Deferred test scope
- performance benchmarking
- metrics validation
- distributed lock semantics and client matrix depth

## 13) Implementation Stack

### Language/runtime
- C# / .NET

### Toolchain expectation
- The implementation language remains C# / .NET, but the required developer toolchain is Docker, not a host `.NET` SDK installation.
- Build/test helper artifacts should prefer Dockerfiles and repo-managed scripts over instructions that assume local SDK setup.

### Primary libraries
- SMB server: `SMBLibrary`
- HTTP client: `HttpClient`
- Serialization: `System.Text.Json`
- Logging: `Microsoft.Extensions.Logging`
- Config: `Microsoft.Extensions.Configuration`

### Suggested project layout
- `src/FileSystem.Server/`
  - SMB server host, share wiring, operation dispatch
- `src/FileSystem.Gateway/`
  - journal gateway client + mapping
- `src/FileSystem.Pathing/`
  - grammar/parser/compiler
- `src/FileSystem.Envelope/`
  - envelope codec + legacy conversion
- `src/FileSystem.Control/`
  - minimal hidden directory-marker metadata channel
- `src/FileSystem.Errors/`
  - deterministic error mapping
- `src/FileSystem.Cache/`
  - immutable cache + stream buffers
- `tests/`
  - unit + integration + client interop suites
