# Module Resolution

For every import "use A.B" in a module C:

-  if A == C, return a self reference error.
-  if A == "Package":
   -  if C is the entry module, return an error.
   -  else, A = entry module.
-  else if A == "Super":
   -  let X be the name of the containing directory.
   -  if there is no module X in the directory, then return an error.
   -  else A = module X.
-  else:
   -  check directory for modules named A.
   -  if there are multiple matches, the import is ambiguous, return an error.
   -  if there are no matches:
      -  check the directory for a directory named A.
      -  if there is no such directory, return an error.
      -  else if there is no module A in directory A, return an error.
