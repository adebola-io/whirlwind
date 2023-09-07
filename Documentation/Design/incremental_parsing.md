# Incremental Parsing Pseudocode.

1. Pinpoint changed statement `S`, as definitely as possible.
2. Create a new tokenizer `T`.
3. Set `T` to the start of the statement span.
4. Pinpoint nodes existing in the changed range and mark them.
5. Regenerate the entire statement as text, while replacing the parts of the marked nodes with the new text.
6. Use `T` to generate tokens for the text.
7. Use a parser `P` over `T` to generate a new Statement node `S_n`.
8. For every declaration in the new
9. Replace `S` with `S_n`.
10.   Delete every child scope using `swap_index` and update the scope addresses for the corresponding nodes.
11.
