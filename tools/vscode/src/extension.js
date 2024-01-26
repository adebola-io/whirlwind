const vscode = require("vscode");
const fs = require("fs");
const { LanguageClient } = require("vscode-languageclient/node");
const { provideCompletionItem } = require("./completion");

/**
 * @type {string}
 * Path to the Language Server executable.
 */
let server_path;
/**
 * @type {string}
 * Path to the entry file of the Core Library.
 */
let core_lib_entry_path;
/** @type {ReturnType<typeof setTimeout>} */
let timeout;
let outputChannel = vscode.window.createOutputChannel("Whirlwind LS");
let showErrorMessage = vscode.window.showErrorMessage;
let showWarningMessage = vscode.window.showWarningMessage;

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
   if (this.client === undefined) {
      /** @type {import("vscode-languageclient/node").Executable} */
      let executable = {
         command: server_path,
         args: [core_lib_entry_path],
      };
      this.client = new LanguageClient(
         "Whirlwind",
         {
            run: executable,
            debug: executable,
         },
         {
            outputChannel,
            documentSelector: [{ scheme: "file", language: "wrl" }],
            middleware: {
               // todo: bounce didchange events in comments and strings.
               provideCompletionItem,
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
   outputChannel.clear();
   await client.start();
   statusBar.text = "Whirlwind";
   statusBar.tooltip = "Whirlwind Language Server";
   statusBar.show();
};

exports.restartServer = async () => {
   const statusBar = getStatusBar();
   let previousText = statusBar.text;
   statusBar.text = "Restarting Whirlwind";
   await exports.stopLanguageServer();
   await exports.startLanguageServer();
   statusBar.text = previousText;
};

exports.stopLanguageServer = async () => {
   const statusBar = getStatusBar();
   const client = getLanguageClient();
   statusBar.text = "Stopping server";
   if (!client.isRunning()) {
      showWarningMessage("Whirlwind LS Server is not running.");
   } else {
      await client.stop();
   }
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
   server_path = context.asAbsolutePath(process.env.WHIRL_LS_PATH);
   core_lib_entry_path = context.asAbsolutePath(process.env.CORE_LIBRARY_PATH);
   outputChannel.appendLine(`Paths: ${[server_path, core_lib_entry_path]}`);
   if (!fs.existsSync(server_path)) {
      showErrorMessage("Server Executable not found.");
   } else {
      if (!fs.existsSync(core_lib_entry_path)) {
         showErrorMessage(
            "Core Library is either non-existent or installed incorrectly."
         );
      }
      exports.startLanguageServer();
   }
};

exports.deactivate = async () => {
   const statusBar = getStatusBar();
   const client = getLanguageClient();
   try {
      if (client) await client.stop();
   } catch (e) {}
   if (statusBar) statusBar.dispose();
};
