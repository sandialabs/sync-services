/**
 * Prettify Scheme code with proper indentation
 */
export const prettifyScheme = (code: string): string => {
  // Tokenize the code
  const tokens: string[] = [];
  let i = 0;
  const len = code.length;

  while (i < len) {
    const ch = code[i];

    // Skip whitespace
    if (/\s/.test(ch)) {
      i++;
      continue;
    }

    // Comments
    if (ch === ';') {
      let comment = '';
      while (i < len && code[i] !== '\n') {
        comment += code[i];
        i++;
      }
      tokens.push(comment);
      continue;
    }

    // String literals
    if (ch === '"') {
      let str = '"';
      i++;
      while (i < len && code[i] !== '"') {
        if (code[i] === '\\' && i + 1 < len) {
          str += code[i] + code[i + 1];
          i += 2;
        } else {
          str += code[i];
          i++;
        }
      }
      if (i < len) {
        str += '"';
        i++;
      }
      tokens.push(str);
      continue;
    }

    // Handle #u(...) byte vector syntax - keep the whole thing together
    if (ch === '#' && i + 1 < len && code[i + 1] === 'u' && i + 2 < len && code[i + 2] === '(') {
      let vec = '#u(';
      i += 3;
      let depth = 1;
      while (i < len && depth > 0) {
        if (code[i] === '(') {
          depth++;
        } else if (code[i] === ')') {
          depth--;
        }
        vec += code[i];
        i++;
      }
      tokens.push(vec);
      continue;
    }

    // Parentheses and special characters
    if (ch === '(' || ch === ')' || ch === '[' || ch === ']' || ch === '{' || ch === '}') {
      tokens.push(ch);
      i++;
      continue;
    }

    // Quote, quasiquote, unquote, unquote-splicing
    if (ch === "'" || ch === '`') {
      tokens.push(ch);
      i++;
      continue;
    }
    if (ch === ',') {
      if (i + 1 < len && code[i + 1] === '@') {
        tokens.push(',@');
        i += 2;
      } else {
        tokens.push(',');
        i++;
      }
      continue;
    }

    // Symbols, numbers, etc.
    let token = '';
    while (i < len && !/[\s()\[\]{}'"`,;]/.test(code[i])) {
      token += code[i];
      i++;
    }
    if (token) {
      tokens.push(token);
    }
  }

  // Format tokens with proper indentation
  let result = '';
  let indent = 0;
  const indentStr = '  ';
  let lineStart = true;
  let prevToken = '';

  for (let t = 0; t < tokens.length; t++) {
    const token = tokens[t];

    if (token.startsWith(';')) {
      // Comment - put on its own line
      if (!lineStart) {
        result += '\n';
      }
      result += indentStr.repeat(indent) + token + '\n';
      lineStart = true;
      prevToken = token;
      continue;
    }

    if (token === '(') {
      if (!lineStart && prevToken !== '(' && prevToken !== "'" && prevToken !== '`' && prevToken !== ',' && prevToken !== ',@') {
        result += '\n' + indentStr.repeat(indent);
      } else if (lineStart) {
        result += indentStr.repeat(indent);
      }
      result += '(';
      indent++;
      lineStart = false;
      prevToken = token;
      continue;
    }

    if (token === ')') {
      indent = Math.max(0, indent - 1);
      result += ')';
      lineStart = false;
      prevToken = token;
      continue;
    }

    if (token === "'" || token === '`' || token === ',' || token === ',@') {
      if (lineStart) {
        result += indentStr.repeat(indent);
      }
      result += token;
      lineStart = false;
      prevToken = token;
      continue;
    }

    // Regular token (including #u(...) vectors which are kept as single tokens)
    if (lineStart) {
      result += indentStr.repeat(indent);
    } else if (prevToken !== '(' && prevToken !== "'" && prevToken !== '`' && prevToken !== ',' && prevToken !== ',@') {
      result += ' ';
    }
    result += token;
    lineStart = false;
    prevToken = token;
  }

  return result.trim();
};
