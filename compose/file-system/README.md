# File System Service

This service provides a file system backend for cross-platform file exploration of the synchronic web using [FUSE](https://en.wikipedia.org/wiki/Filesystem_in_Userspace) and [SAMBA](https://en.wikipedia.org/wiki/Samba_(software)).
In the most basic use case, users can interact with it as if it were any mounted Linux drive.
The distinguishing feature is the integration is the invocation and integration with a synchronic journal to provide secure versioning and temporal references to historic file system artifacts.

## Extra Record Functionality

- Transparent hash thing
- Permissions??
- Directories (default files)

## Minimal Functionality (near-term)

- [ ] Invocation of journal (ledger interface) on every read/write
- [ ] Hash-based storage of historical document states
- [ ] Mapping between synchronic web records and stored documents
- [ ] Interface for browsing historical documents
- [ ] Interface for browsing remote documents

## Desired Features (long-term)

- [ ] Space-efficient compression of versioned files
- [ ] Integration with enterprise-grade authentciation and authorization
