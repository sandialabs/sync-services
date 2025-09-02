import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";

//TODO: make this a named argument
const LEDGER_API_BASE = process.argv[2]; //"http://localhost:8192/.interface/";
const USER_AGENT = "ontology-app/1.0";

// Create server instance
const server = new McpServer({
  name: "ontology",
  version: "1.0.0",
  capabilities: {
    resources: {},
    tools: {},
  },
});

// Helper function for making ledger API requests
async function makeOntologyRequest<T>(url: string, postBody: string): Promise<T | null> {
  const headers = {
    "User-Agent": USER_AGENT,
    Accept: "application/x-lisp",
  };

  try {
    const response = await fetch(url, { method: 'POST', headers, body: postBody });
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return (await response.text()) as T;
  } catch (error) {
    console.error("Error making Ledger request:", error);
    return null;
  }
}

interface OntologyResponse {
  properties: {
    response: string
  };
}

server.tool(
  "ontology-dfs",
  "Perform a depth first search through a knowledge graph",
  {
    ref: z.string().describe("TODO: describe this arg"),
    depth: z.string().describe("the depth of the search"),
    some_boolean: z.string().describe("TODO: describe this arg"),
  },
  async ({ ref, depth, some_boolean }) => {
    //TODO: make local journal password a named argument instead of process.argv[3]
    //TODO: figure out how to do the ref to refer to something like (ref (*peers* some-system *state* some-path name))
    //TODO: figure out what some_boolean_should be actually called
    //TODO: pass some_boolean as #f or #t
    const postBody = '(*local* "' + process.argv[3] + '" (ontology-dfs ' + ref + ' ' + depth + ' ' + some_boolean + ')';
    const journalUrl = `${LEDGER_API_BASE}`;
    const journalData = await makeOntologyRequest<OntologyResponse>(journalUrl, postBody);

    return {
      content: [
        {
          type: "text",
          text: `${journalData}`,
        },
      ],
    };
  });

server.tool(
  "ontology-insert-batch!",
  "Write to the knowledge graph",
  {
    triples: z.string().describe("A list of RDF triples"),
  },
  async ({ triples }) => {
    //TODO: make local journal password a named argument instead of process.argv[3]
    const postBody = '(*local* "' + process.argv[3] + '" (ontology-insert-batch! ' + triples + ')';
    const journalUrl = `${LEDGER_API_BASE}`;
    const journalData = await makeOntologyRequest<OntologyResponse>(journalUrl, postBody);

    return {
      content: [
        {
          type: "text",
          text: `${journalData}`,
        },
      ],
    };
  });

async function main() {
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error("Synchronic Web Ontology Interface MCP Server running on stdio");
}

main().catch((error) => {
  console.error("Fatal error in main():", error);
  process.exit(1);
});

