# Synchronic Web Services

This repository contains services and deployment configurations for the [Synchronic Web Journal](https://github.com/sandialabs/sync-journal) and [Synchronic Web Records](https://github.com/sandialabs/sync-records). It provides both ready-to-deploy, all-in-one Compose environments and individual microservice components that support Synchronic Web applications.

---

## Repository Structure

- **compose/**  
  Pre-packaged, batteries-included Docker Compose environments for deploying Synchronic Web journals and supporting services.  
  - Each subdirectory (e.g., `ledger/`, `ontology/`) contains everything needed to launch a complete application stack, including configuration, orchestration, and example scripts.
  - Ideal for users who want to quickly deploy a working Synchronic Web journal with minimal setup.

- **services/**  
  Standalone microservice components that provide additional functionality to composed deployments.  
  - These are not full applications by themselves, but are designed to be integrated into Compose environments or other orchestration systems.
  - Example: the `explorer` service provides a point-and-click front-end for exploring Synchronic Web record and ledger interfaces.

---

## Getting Started

To get started, choose a Compose environment that matches your use case:

- [compose/ledger/README.md](compose/ledger/README.md): Deploy a ledger journal for cryptographically verifiable record-keeping.
- [compose/ontology/README.md](compose/ontology/README.md): Deploy an ontology journal for semantic data and provenance workflows.

Each Compose README provides step-by-step instructions, required environment variables, and example usage. These environments are designed for rapid deployment and experimentation—just follow the linked guides for details.

If you want to run or develop individual microservices, see the README in each service directory (e.g., [services/explorer/README.md](services/explorer/README.md)).

---

## Related Projects

- **[sync-journal](https://github.com/sandialabs/sync-journal):**  
  The core Synchronic Web Journal SDK. Provides the main ledger, record, and evaluation engine.

- **[sync-records](https://github.com/sandialabs/sync-records):**  
  A collection of reusable Scheme modules and test suites for advanced record and ledger operations.

- **[sync-analysis](https://github.com/sandialabs/sync-analysis):**  
  Frameworks and tools for analyzing Synchronic Web experiments and deployments.

---

## Contributing

Contributions are welcome! Please open issues or pull requests for new services, improvements to Compose environments, or documentation updates.
