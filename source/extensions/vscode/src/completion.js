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
   const line = document.lineAt(position.line).text;
   let beforePosition = line.slice(0, position.character);
   const isInComment = beforePosition.includes("//");
   // todo: also disable for multiline strings.
   const doubleQuoteBefore =
      beforePosition.includes('"') &&
      beforePosition.indexOf('"') === beforePosition.lastIndexOf('"');
   const singleQuoteBefore =
      beforePosition.includes("'") &&
      beforePosition.indexOf("'") === beforePosition.lastIndexOf("'");
   const isInString = doubleQuoteBefore || singleQuoteBefore;
   if (isInComment || isInString) {
      return null;
   }
   let newContext = {
      triggerKind: CompletionTriggerKind.TriggerCharacter,
      triggerCharacter: context.triggerCharacter,
   };
   const whitespaceTrigger = context.triggerCharacter === " ";
   beforePosition = beforePosition.trimEnd();
   if (beforePosition.endsWith(".")) {
      newContext = {
         ...newContext,
         triggerCharacter: ".",
      };
   }
   // Customized completion for use imports.
   const beforeIsUse =
      beforePosition.endsWith(" use") || beforePosition === "use";
   if (beforeIsUse && whitespaceTrigger) {
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
   } else if (beforePosition.endsWith("module") && whitespaceTrigger) {
      newContext = {
         ...newContext,
         triggerCharacter: "module ",
      };
   }
   return next(document, position, newContext, token);
};
