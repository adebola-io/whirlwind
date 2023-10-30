const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

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

function getLanguageClient() {
   if (this.client == undefined) {
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
            documentSelector: [{ scheme: "file", language: "wrl" }],
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
   await exports.startLanguageServer();
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
