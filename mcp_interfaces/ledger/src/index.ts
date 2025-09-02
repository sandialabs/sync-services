import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";

//TODO: make this a named argument
const LEDGER_API_BASE = process.argv[2]; //"http://localhost:8192/.interface/";
const USER_AGENT = "ledger-app/1.0";

// Create server instance
const server = new McpServer({
  name: "ledger",
  version: "1.0.0",
  capabilities: {
    resources: {},
    tools: {},
  },
});

// Helper function for making ledger API requests
async function makeLedgerRequest<T>(url: string, postBody: string): Promise<T | null> {
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

interface LedgerResponse {
  properties: {
    response: string
  };
}

server.tool(
  "ledger_get",
  "Read from the journal ledger",
  {
    path: z.string().describe("The ledger path/key"),
  },
  async ({ path }) => {
    //TODO: make local journal password a named argument instead of process.argv[3]
    const postBody = '(*local* "' + process.argv[3] + '" (ledger-get (*state* ' + path + ')))';
    const journalUrl = `${LEDGER_API_BASE}`;
    const journalData = await makeLedgerRequest<LedgerResponse>(journalUrl, postBody);

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
  "ledger_set",
  "Write to the journal ledger",
  {
    path: z.string().describe("The ledger path/key"),
    value: z.string().describe("The value to store at the path/key"),
  },
  async ({ path, value }) => {
    //TODO: make local journal password a named argument instead of process.argv[3]
    const postBody = '(*local* "' + process.argv[3] + '" (ledger-set! (*state* ' + path + ') ' + value + '))';
    const journalUrl = `${LEDGER_API_BASE}`;
    const journalData = await makeLedgerRequest<LedgerResponse>(journalUrl, postBody);

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
  console.error("Synchronic Web Ledger Interface MCP Server running on stdio");
}

main().catch((error) => {
  console.error("Fatal error in main():", error);
  process.exit(1);
});

