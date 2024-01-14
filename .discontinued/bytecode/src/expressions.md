# Generating Bytecode from Expressions.

## Binary Expressions.

-  Given the expression a + b.
   -  Load a into a register of its type. Start with r1. If r1 is occupied, use r2. If both registers is occupied, pop the value on the operand stack, empty both r1 and r2 to the accumulator and use r1.
   -  Load b into a register of its type, same as a.
   -  Push the operand onto the operand queue.

## Assignment Expressions.

-  Transform the lhs from an expression to a stack address.
-  Evaluate the RHS.
-  If the two intermediate registers are not empty, flush their two intermediate registers with the last operand on the operand stack.
-  Empty whatever value is in the mapped accumulator register to the stack address.
-  If the value in the stack address is a heap pointer,
   -  If the pointer has only one reference, update it in place.
   -  If it has multiple references, clone it and update its clone.
