const vscode = require("vscode");
const { CompletionTriggerKind } = require("vscode-languageclient");

/**
 * Middleware completion function.
 * @param {vscode.TextDocument} document
 * @param {vscode.Position} position
 * @param {vscode.CompletionContext} context
 * @param {vscode.CancellationToken} token
 * @param {import("vscode-languageclient").ProvideCompletionItemsSignature} next
 */
module.exports.provideCompletionItem = (
   document,
   position,
   context,
   token,
   next
) => {
   // Ignore comments and strings while providing completions.
   let line = document.lineAt(position.line).text;
   let beforePosition = line.slice(0, position.character);
   let isInComment = beforePosition.includes("//");
   // todo: also disable for multiline strings.
   let doubleQuoteBefore =
      beforePosition.includes('"') &&
      beforePosition.indexOf('"') == beforePosition.lastIndexOf('"');
   let singleQuoteBefore =
      beforePosition.includes("'") &&
      beforePosition.indexOf("'") == beforePosition.lastIndexOf("'");
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
   else if (beforePosition.endsWith("new") && whitespaceTrigger) {
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
   else if (beforePosition.endsWith("implements") && whitespaceTrigger) {
      newContext = {
         ...newContext,
         triggerCharacter: "implements ",
      };
   }
   return next(document, position, newContext, token);
};
