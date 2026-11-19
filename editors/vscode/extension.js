// Minimal LSP client. No npm dependencies: spawn the binary, frame messages, map requests.
const vscode = require("vscode");
const cp = require("child_process");

let child = null;
let seq = 0;
let buffer = Buffer.alloc(0);
const pending = new Map();

function send(message) {
  const body = Buffer.from(JSON.stringify(message));
  child.stdin.write(`Content-Length: ${body.length}\r\n\r\n`);
  child.stdin.write(body);
}

function request(method, params) {
  const id = ++seq;
  return new Promise((resolve) => {
    pending.set(id, resolve);
    send({ jsonrpc: "2.0", id, method, params });
  });
}

function receive(chunk) {
  buffer = Buffer.concat([buffer, chunk]);
  for (;;) {
    const sep = buffer.indexOf("\r\n\r\n");
    if (sep < 0) return;
    const header = buffer.slice(0, sep).toString();
    const match = /Content-Length:\s*(\d+)/i.exec(header);
    if (!match) return;
    const length = Number(match[1]);
    if (buffer.length < sep + 4 + length) return;
    const message = JSON.parse(
      buffer.slice(sep + 4, sep + 4 + length).toString(),
    );
    buffer = buffer.slice(sep + 4 + length);
    if (message.id !== undefined && pending.has(message.id)) {
      pending.get(message.id)(message.result);
      pending.delete(message.id);
    }
  }
}

const lspKind = { 5: 4, 10: 9, 11: 10, 12: 11, 23: 22, 6: 5, 2: 1 };

function location(raw) {
  const range = new vscode.Range(
    raw.range.start.line,
    raw.range.start.character,
    raw.range.end.line,
    raw.range.end.character,
  );
  return new vscode.Location(vscode.Uri.parse(raw.uri), range);
}

function activate(context) {
  const root = vscode.workspace.workspaceFolders?.[0];
  if (!root) return;
  const binary = vscode.workspace
    .getConfiguration("repowatch")
    .get("path", "repowatch");
  child = cp.spawn(binary, ["lsp"], { cwd: root.uri.fsPath });
  child.stdout.on("data", receive);
  child.stderr.on("data", () => {});
  child.on("error", () => {
    child = null;
  });
  child.on("exit", () => {
    child = null;
  });

  request("initialize", {
    rootUri: root.uri.toString(),
    capabilities: {},
  }).then(() => {
    send({ jsonrpc: "2.0", method: "initialized", params: {} });
  });

  const selector = { scheme: "file" };
  context.subscriptions.push(
    vscode.languages.registerDefinitionProvider(selector, {
      async provideDefinition(document, position) {
        if (!child) return [];
        const result = await request("textDocument/definition", {
          textDocument: { uri: document.uri.toString() },
          position: { line: position.line, character: position.character },
        });
        return (result || []).map(location);
      },
    }),
    vscode.languages.registerReferenceProvider(selector, {
      async provideReferences(document, position) {
        if (!child) return [];
        const result = await request("textDocument/references", {
          textDocument: { uri: document.uri.toString() },
          position: { line: position.line, character: position.character },
        });
        return (result || []).map(location);
      },
    }),
    vscode.languages.registerWorkspaceSymbolProvider({
      async provideWorkspaceSymbols(query) {
        if (!child) return [];
        const result = await request("workspace/symbol", { query });
        return (result || []).map(
          (item) =>
            new vscode.SymbolInformation(
              item.name,
              lspKind[item.kind] || vscode.SymbolKind.Variable,
              "",
              location(item.location),
            ),
        );
      },
    }),
    {
      dispose() {
        if (child) child.kill();
      },
    },
  );
}

function deactivate() {
  if (child) child.kill();
}

module.exports = { activate, deactivate };
