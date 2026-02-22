/**
 * Execute a query against the journal endpoint
 */
export const executeQuery = async (
  endpoint: string,
  query: string
): Promise<{ result: any; request: string; response: string; error?: string }> => {
  const requestBody = query.trim();
  const requestInfo = `POST ${endpoint}\nContent-Type: text/plain\n\n${requestBody}`;

  try {
    const response = await fetch(endpoint, {
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain',
      },
      body: requestBody,
    });

    const responseText = await response.text();
    const responseInfo = `HTTP ${response.status} ${response.statusText}\n\n${responseText}`;

    if (!response.ok) {
      return {
        result: null,
        request: requestInfo,
        response: responseInfo,
        error: `Request failed: ${response.status} ${response.statusText}`,
      };
    }

    let result: any;
    try {
      result = JSON.parse(responseText);
    } catch {
      result = responseText;
    }

    return {
      result,
      request: requestInfo,
      response: responseInfo,
    };
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    return {
      result: null,
      request: requestInfo,
      response: `Error: ${errorMessage}`,
      error: errorMessage,
    };
  }
};
