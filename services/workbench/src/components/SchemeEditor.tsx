import React, { useMemo } from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { StreamLanguage } from '@codemirror/language';
import { scheme } from '@codemirror/legacy-modes/mode/scheme';
import { json } from '@codemirror/lang-json';
import { createTheme } from '@uiw/codemirror-themes';
import { tags as t } from '@lezer/highlight';
import { keymap, EditorView } from '@codemirror/view';
import { Prec } from '@codemirror/state';
import { vim } from '@replit/codemirror-vim';

// Custom dark theme inspired by Dracula with prominent keywords
const schemeDarkTheme = createTheme({
  theme: 'dark',
  settings: {
    background: '#1e1e1e',
    foreground: '#cdd6f4',
    caret: '#f5e0dc',
    selection: '#45475a',
    selectionMatch: '#585b70',
    lineHighlight: '#2a2a2a',
    gutterBackground: '#1e1e1e',
    gutterForeground: '#6c7086',
    gutterBorder: 'transparent',
  },
  styles: [
    // Keywords (define, lambda, let, if, cond, etc.) - bright pink/magenta
    { tag: t.keyword, color: '#ff79c6', fontWeight: 'bold' },
    { tag: t.definitionKeyword, color: '#ff79c6', fontWeight: 'bold' },
    { tag: t.controlKeyword, color: '#ff79c6', fontWeight: 'bold' },
    { tag: t.operatorKeyword, color: '#ff79c6', fontWeight: 'bold' },
    { tag: t.moduleKeyword, color: '#ff79c6', fontWeight: 'bold' },
    
    // Function names and definitions - bright green
    { tag: t.function(t.variableName), color: '#50fa7b' },
    { tag: t.definition(t.variableName), color: '#50fa7b' },
    { tag: t.function(t.definition(t.variableName)), color: '#50fa7b', fontWeight: 'bold' },
    
    // Variables - light purple
    { tag: t.variableName, color: '#bd93f9' },
    
    // Strings - yellow
    { tag: t.string, color: '#f1fa8c' },
    
    // Numbers - orange
    { tag: t.number, color: '#ffb86c' },
    { tag: t.integer, color: '#ffb86c' },
    { tag: t.float, color: '#ffb86c' },
    
    // Comments - muted gray
    { tag: t.comment, color: '#6272a4', fontStyle: 'italic' },
    { tag: t.lineComment, color: '#6272a4', fontStyle: 'italic' },
    { tag: t.blockComment, color: '#6272a4', fontStyle: 'italic' },
    
    // Booleans and special values - cyan
    { tag: t.bool, color: '#8be9fd' },
    { tag: t.special(t.variableName), color: '#8be9fd' },
    { tag: t.atom, color: '#8be9fd' },
    
    // Operators - bright white
    { tag: t.operator, color: '#f8f8f2' },
    
    // Brackets/parens - subtle but visible
    { tag: t.paren, color: '#f8f8f2' },
    { tag: t.bracket, color: '#f8f8f2' },
    { tag: t.brace, color: '#f8f8f2' },
    { tag: t.squareBracket, color: '#f8f8f2' },
    
    // Punctuation
    { tag: t.punctuation, color: '#f8f8f2' },
    { tag: t.separator, color: '#f8f8f2' },
    
    // Meta and special
    { tag: t.meta, color: '#ff79c6' },
    { tag: t.special(t.string), color: '#f1fa8c' },
    
    // Property names (for JSON)
    { tag: t.propertyName, color: '#8be9fd' },
    
    // Invalid
    { tag: t.invalid, color: '#ff5555' },
    
    // Builtin - for scheme builtins
    { tag: t.standard(t.variableName), color: '#50fa7b' },
    { tag: t.special(t.brace), color: '#f8f8f2' },
    
    // Additional tags that legacy modes might use
    { tag: t.name, color: '#bd93f9' },
    { tag: t.labelName, color: '#50fa7b' },
    { tag: t.namespace, color: '#ff79c6' },
    { tag: t.macroName, color: '#ff79c6', fontWeight: 'bold' },
    { tag: t.literal, color: '#ffb86c' },
    { tag: t.inserted, color: '#50fa7b' },
    { tag: t.deleted, color: '#ff5555' },
    { tag: t.changed, color: '#f1fa8c' },
    { tag: t.heading, color: '#ff79c6', fontWeight: 'bold' },
    { tag: t.contentSeparator, color: '#6272a4' },
    { tag: t.list, color: '#f8f8f2' },
    { tag: t.quote, color: '#f1fa8c' },
    { tag: t.emphasis, fontStyle: 'italic' },
    { tag: t.strong, fontWeight: 'bold' },
    { tag: t.link, color: '#8be9fd', textDecoration: 'underline' },
    { tag: t.monospace, fontFamily: 'monospace' },
    { tag: t.content, color: '#cdd6f4' },
  ],
});

// Custom light theme with prominent keywords
const schemeLightTheme = createTheme({
  theme: 'light',
  settings: {
    background: '#fafafa',
    foreground: '#383a42',
    caret: '#526eff',
    selection: '#e5e5e6',
    selectionMatch: '#d5d5d6',
    lineHighlight: '#f0f0f0',
    gutterBackground: '#fafafa',
    gutterForeground: '#9d9d9f',
    gutterBorder: 'transparent',
  },
  styles: [
    // Keywords (define, lambda, let, if, cond, etc.) - bold purple
    { tag: t.keyword, color: '#a626a4', fontWeight: 'bold' },
    { tag: t.definitionKeyword, color: '#a626a4', fontWeight: 'bold' },
    { tag: t.controlKeyword, color: '#a626a4', fontWeight: 'bold' },
    { tag: t.operatorKeyword, color: '#a626a4', fontWeight: 'bold' },
    { tag: t.moduleKeyword, color: '#a626a4', fontWeight: 'bold' },
    
    // Function names and definitions - blue
    { tag: t.function(t.variableName), color: '#4078f2' },
    { tag: t.definition(t.variableName), color: '#4078f2' },
    { tag: t.function(t.definition(t.variableName)), color: '#4078f2', fontWeight: 'bold' },
    
    // Variables - dark red/maroon
    { tag: t.variableName, color: '#e45649' },
    
    // Strings - green
    { tag: t.string, color: '#50a14f' },
    
    // Numbers - orange/brown
    { tag: t.number, color: '#c18401' },
    { tag: t.integer, color: '#c18401' },
    { tag: t.float, color: '#c18401' },
    
    // Comments - gray italic
    { tag: t.comment, color: '#a0a1a7', fontStyle: 'italic' },
    { tag: t.lineComment, color: '#a0a1a7', fontStyle: 'italic' },
    { tag: t.blockComment, color: '#a0a1a7', fontStyle: 'italic' },
    
    // Booleans and special values - cyan/teal
    { tag: t.bool, color: '#0184bc' },
    { tag: t.special(t.variableName), color: '#0184bc' },
    { tag: t.atom, color: '#0184bc' },
    
    // Operators
    { tag: t.operator, color: '#383a42' },
    
    // Brackets/parens
    { tag: t.paren, color: '#383a42' },
    { tag: t.bracket, color: '#383a42' },
    { tag: t.brace, color: '#383a42' },
    { tag: t.squareBracket, color: '#383a42' },
    
    // Punctuation
    { tag: t.punctuation, color: '#383a42' },
    { tag: t.separator, color: '#383a42' },
    
    // Meta and special
    { tag: t.meta, color: '#a626a4' },
    { tag: t.special(t.string), color: '#50a14f' },
    
    // Property names (for JSON)
    { tag: t.propertyName, color: '#0184bc' },
    
    // Invalid
    { tag: t.invalid, color: '#e45649' },
    
    // Builtin - for scheme builtins
    { tag: t.standard(t.variableName), color: '#4078f2' },
    { tag: t.special(t.brace), color: '#383a42' },
    
    // Additional tags that legacy modes might use
    { tag: t.name, color: '#e45649' },
    { tag: t.labelName, color: '#4078f2' },
    { tag: t.namespace, color: '#a626a4' },
    { tag: t.macroName, color: '#a626a4', fontWeight: 'bold' },
    { tag: t.literal, color: '#c18401' },
    { tag: t.inserted, color: '#50a14f' },
    { tag: t.deleted, color: '#e45649' },
    { tag: t.changed, color: '#c18401' },
    { tag: t.heading, color: '#a626a4', fontWeight: 'bold' },
    { tag: t.contentSeparator, color: '#a0a1a7' },
    { tag: t.list, color: '#383a42' },
    { tag: t.quote, color: '#50a14f' },
    { tag: t.emphasis, fontStyle: 'italic' },
    { tag: t.strong, fontWeight: 'bold' },
    { tag: t.link, color: '#0184bc', textDecoration: 'underline' },
    { tag: t.monospace, fontFamily: 'monospace' },
    { tag: t.content, color: '#383a42' },
  ],
});

// Create the Scheme language support
const schemeLanguage = StreamLanguage.define(scheme);

interface SchemeEditorProps {
  value: string;
  onChange?: (value: string) => void;
  theme: 'light' | 'dark';
  readOnly?: boolean;
  placeholder?: string;
  language?: 'scheme' | 'json' | 'text';
  onRun?: () => void;
  vimMode?: boolean;
}

export const SchemeEditor: React.FC<SchemeEditorProps> = ({
  value,
  onChange,
  theme,
  readOnly = false,
  placeholder,
  language = 'scheme',
  onRun,
  vimMode = false,
}) => {
  const editorTheme = useMemo(() => (theme === 'dark' ? schemeDarkTheme : schemeLightTheme), [theme]);

  const extensions = useMemo(() => {
    const exts: any[] = [];
    
    // Add vim mode if enabled
    if (vimMode) {
      exts.push(vim());
    }
    
    // Enable line wrapping
    exts.push(EditorView.lineWrapping);
    
    // Add language support
    if (language === 'json') {
      exts.push(json());
    } else if (language === 'scheme') {
      exts.push(schemeLanguage);
    }
    
    // Add Ctrl/Cmd+Enter keymap for running queries
    if (onRun) {
      const runKeymap = keymap.of([
        {
          key: 'Mod-Enter',
          run: () => {
            onRun();
            return true;
          },
        },
      ]);
      // Use high precedence to ensure our keymap takes priority
      exts.push(Prec.highest(runKeymap));
    }
    
    return exts;
  }, [language, onRun, vimMode]);

  return (
    <CodeMirror
      value={value}
      height="100%"
      theme={editorTheme}
      extensions={extensions}
      onChange={onChange}
      editable={!readOnly}
      placeholder={placeholder}
      basicSetup={{
        lineNumbers: true,
        foldGutter: true,
        highlightActiveLine: !readOnly,
        highlightSelectionMatches: true,
        autocompletion: false,
        closeBrackets: false,
        tabSize: 2,
        syntaxHighlighting: true,
      }}
    />
  );
};
