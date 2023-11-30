const vscode = require("vscode");
const {
   LanguageClient,
   CompletionTriggerKind,
} = require("vscode-languageclient/node");

/** @type {string} */
let server_path;

/** @type {ReturnType<typeof setTimeout>} */
let timeout;

function getStatusBar() {
   if (this.statusBar === undefined) {
      this.statusBar = vscode.window.createStatusBarItem(
         vscode.StatusBarAlignment.Left
      );
      this.statusBar.name = "Whirlwind";
   }
   return this.statusBar;
}

var triggerRestart = false;
function getLanguageClient() {
   if (this.client == undefined || triggerRestart) {
      this.client = new LanguageClient(
         "Whirlwind",
         "Whirlwind Language Server",
         {
            command: server_path,
            debug: {
               command: server_path,
            },
         },
         {
            outputChannelName: "Whirlwind LS",
            documentSelector: [{ scheme: "file", language: "wrl" }],
            middleware: {
               // todo: bounce didchange events in comments and strings.
               provideCompletionItem(document, position, context, token, next) {
                  // Ignore comments and strings while providing completions.
                  let line = document.lineAt(position.line).text;
                  let beforePosition = line.slice(0, position.character);
                  let isInComment = beforePosition.includes("//");
                  // todo: also disable for multiline strings.
                  let doubleQuoteBefore =
                     beforePosition.includes('"') &&
                     beforePosition.indexOf('"') ==
                        beforePosition.lastIndexOf('"');
                  let singleQuoteBefore =
                     beforePosition.includes("'") &&
                     beforePosition.indexOf("'") ==
                        beforePosition.lastIndexOf("'");
                  let isInString = doubleQuoteBefore || singleQuoteBefore;
                  if (isInComment || isInString) {
                     return null;
                  }
                  let newContext = {
                     triggerKind: CompletionTriggerKind.TriggerCharacter,
                     triggerCharacter: context.triggerCharacter,
                  };
                  let whitespaceTrigger = context.triggerCharacter == " ";
                  beforePosition = beforePosition.trimEnd();
                  if (beforePosition.endsWith(".")) {
                     newContext = {
                        ...newContext,
                        triggerCharacter: ".",
                     };
                  }
                  // Customized completion for use imports.
                  if (beforePosition.endsWith(" use") && whitespaceTrigger) {
                     newContext = {
                        ...newContext,
                        triggerCharacter: "use ",
                     };
                  }
                  // Customized completion for new model instances.
                  else if (
                     beforePosition.endsWith("new") &&
                     whitespaceTrigger
                  ) {
                     newContext = {
                        ...newContext,
                        triggerCharacter: "new ",
                     };
                     // return null;
                  }
                  // Customized completion for type labels.
                  else if (beforePosition.endsWith(":") && whitespaceTrigger) {
                     newContext = {
                        ...newContext,
                        triggerCharacter: ": ",
                     };
                  }
                  // Customized completion for implementations.
                  else if (
                     beforePosition.endsWith("implements") &&
                     whitespaceTrigger
                  ) {
                     newContext = {
                        ...newContext,
                        triggerCharacter: "implements ",
                     };
                  }
                  return next(document, position, newContext, token);
               },
            },
            synchronize: {
               fileEvents:
                  vscode.workspace.createFileSystemWatcher("**/.clientrc"),
            },
         }
      );
   }
   return this.client;
}

exports.startLanguageServer = async () => {
   const statusBar = getStatusBar();
   const client = getLanguageClient();
   await client.start();
   statusBar.text = "Whirlwind";
   statusBar.tooltip = "Whirlwind Language Server";
   statusBar.show();
};

exports.restartServer = async () => {
   await exports.stopLanguageServer();
   triggerRestart = true;
   await exports.startLanguageServer();
   triggerRestart = false;
};

exports.stopLanguageServer = async () => {
   const statusBar = getStatusBar();
   const client = getLanguageClient();
   statusBar.text = "Stopping server";
   await client.stop();
   if (timeout !== undefined) {
      clearTimeout(timeout);
      timeout = undefined;
   }
   statusBar.dispose();
};

/**
 * Extension activation function.
 * @param {vscode.ExtensionContext} context
 */
exports.activate = async (context) => {
   require("dotenv").config({
      path: context.asAbsolutePath(".env.sample"),
   });

   server_path = context.asAbsolutePath(process.env.WHIRL_LS_PATH);
   vscode.commands.registerCommand(
      "whirlwind-server-start",
      exports.startLanguageServer
   );
   vscode.commands.registerCommand(
      "whirlwind-server-stop",
      exports.stopLanguageServer
   );
   vscode.commands.registerCommand(
      "whirlwind-server-restart",
      exports.restartServer
   );
   exports.startLanguageServer();
};

exports.deactivate = async () => {
   const statusBar = getStatusBar();
   const client = getLanguageClient();
   try {
      if (client) await client.stop();
   } catch (e) {}
   if (statusBar) statusBar.dispose();
};
