# fsharp toy-parser
# F# Toy Parser

## BNF Grammar

```text
program      → stmt_list $$

stmt         → id assign
             | read_stmt
             | write_stmt
             | if_stmt
             | for_stmt
             | while_stmt
             | fun_def

stmt_list    → stmt stmt_list | ε

stmt_block   → do stmt_list od

read_stmt    → read id

write_stmt   → write expr

if_stmt      → if cond then stmt_list else_stmt

else_stmt    → else stmt_list endif
             | endif

for_stmt     → for id in for_tail

for_tail     → array stmt_block
             | id stmt_block

fun_def      → def id ( id_list ) stmt_block

while_stmt   → while cond stmt_block

assign       → <- fun_call
             | = assign_tail

assign_tail  → expr
             | array

fun_call     → id ( param_list )

array        → [ id_list ]

param_list   → expr param_tail
             | ε

param_tail   → , expr param_tail
             | ε

id_list      → id id_tail

id_tail      → , id_list
             | ε

expr         → id expr_tail
             | ( expr )

expr_tail    → arith_op expr
             | ε

cond         → expr rel_oper expr

arith_op     → -
             | +
             | *
             | /

rel_oper     → >
             | <
             | ==

id           → any lexeme/token (numbers or letters)
               not already expressed as a terminal above
```

## Notes

- `ε` represents an empty production.
- `$$` represents the end of the input and ensures no unprocessed tokens remain.
- Expressions do not implement operator precedence. All arithmetic operators are treated equally for simplicity.
