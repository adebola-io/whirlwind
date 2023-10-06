const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

/**
 * @type {InstanceType<typeof LanguageClient>}
 */
let client;
/**
 * Extension activation function.
 * @param {vscode.ExtensionContext} context
 */
exports.activate = async (context) => {
   require("dotenv").config({
      path: context.asAbsolutePath(".env"),
   });

   client = new LanguageClient(
      "Whirlwind",
      "Whirlwind Language Server",
      {
         command: context.asAbsolutePath(process.env.WHIRL_LS_PATH),
         debug: {
            command: context.asAbsolutePath(process.env.WHIRL_LS_PATH),
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
   await client.start();
};

exports.deactivate = async () => {
   if (!client) return;

   await client.stop();
};
