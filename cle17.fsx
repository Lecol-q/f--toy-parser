// Keywords: read, write, if, then, else, endif, for, in, do, od, def, whike
// Operators: <-, =, -, +, *, /, >, <, ==
// Punctuation: (, ), [, ], ',' (comma)
// id

// token set
type Token =
    // keywords
    | READ
    | WRITE
    | IF
    | THEN
    | ELSE
    | ENDIF
    | FOR
    | IN
    | DO
    | OD
    | DEF
    | WHILE
    // operators
    | ASSIGN_ARROW // <-
    | ASSIGNMENT
    | MINUS
    | ADD
    | FSLASH // division
    | MULTIPLY
    | GREATER_THAN
    | LESS_THAN
    | COMPARISON
    // punctuation
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACKET
    | RIGHT_BRACKET
    | COMMA
    | ID of string

    // Member Function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
        // keywords mapping
        | "read" -> READ
        | "write" -> WRITE
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "endif" -> ENDIF
        | "for" -> FOR
        | "in" -> IN
        | "do" -> DO
        | "od" -> OD
        | "def" -> DEF
        | "while" -> WHILE
        // operators
        | "<-" -> ASSIGN_ARROW
        | "=" -> ASSIGNMENT
        | "-" -> MINUS
        | "+" -> ADD
        | "*" -> MULTIPLY
        | "/" -> FSLASH
        | ">" -> GREATER_THAN
        | "<" -> LESS_THAN
        | "==" -> COMPARISON
        // punctuation
        | "(" -> LEFT_PAREN
        | ")" -> RIGHT_PAREN
        | "[" -> LEFT_BRACKET
        | "]" -> RIGHT_BRACKET
        | "," -> COMMA
        | _ -> ID str

// start
let rec start xs =
    try
        match xs |> program with 
        | [] -> printfn "SUCCESS!!!" |> ignore 
        | _  -> printfn "Parse Failed: Unprocessed tokens remain!" |> ignore
    with Failure msg 
        -> printfn $"Parsing error... %s{msg}" |> ignore
    
    xs // Reurn the list of tokens for debugging purposes.

// program -> stmt_list $$
and program xs =
    xs |> stmt_list |> ``$$``

and ``$$`` =
    function
    | [] -> [] : Token list  // Explicit Type is necessary because the type of an empty list cannot be inferred!
    | reminingElements -> failwithf $"Unprocessed Tokens: {reminingElements}"

// stmt -> id assign | read_stmt | write_stmt | if_stmt | for_stmt | while_stmt | fun_def
and stmt lst =
    printfn $"DEBUG stmt rule: The token list  = %A{lst}"

    match lst with
    | ID _ :: xs -> xs |> assign
    | READ :: _ -> lst |> read_stmt
    | WRITE :: _ -> lst |> write_stmt
    | IF :: _ -> lst |> if_stmt
    | FOR :: _ -> lst |> for_stmt
    | WHILE :: _ -> lst |> while_stmt
    | DEF :: _ -> lst |> fun_def
    | _ -> failwithf $"Not a valid statement: %A{lst}"

// stmt_list -> stmt stmt_list | ε
and stmt_list lst =
    match lst with
    | (ID _ | READ | WRITE | IF | FOR | WHILE | DEF) ::_ -> lst |> stmt |> stmt_list
    | x -> x // ε


// stmt_block -> do stmt_list od
and stmt_block lst =
    // the helper function to determine if OD comes at the end
    let match_OD_token =
        function
        | OD :: xs -> xs
        | _ -> failwith "Expected OD"

    match lst with
    | DO :: xs -> xs |> stmt_list |> match_OD_token
    | _ -> failwith "Expected DO"

// read_stmt -> read id
and read_stmt lst =
    match lst with
    | READ :: ID _ :: xs -> xs
    | _ -> failwith "..."

and write_stmt lst =
    match lst with
    | WRITE :: xs -> xs |> expr
    | _ -> failwith "Expected WRITE"

// if_stmt -> if cond then stmt_list else_stmt
and if_stmt lst =
    let match_THEN_token =
        function
        | THEN :: xs -> xs
        | _ -> failwith "Expected THEN"

    match lst with
    | IF :: xs -> xs |> cond |> match_THEN_token |> stmt_list |> else_stmt
    | _ -> failwith "Expected IF"

// else_stmt -> else stmt_list endif | endif
and else_stmt lst =
    let match_ENDIF_token =
        function
        | ENDIF :: xs -> xs
        | _ -> failwith "Expected ENDIF"

    match lst with
    | ELSE :: xs -> xs |> stmt_list |> match_ENDIF_token
    | ENDIF :: xs -> xs
    | _ -> failwith "Expected ELSE or ENDIF"

// for_stmt -> for id in for_tail
and for_stmt lst =
    match lst with
    | FOR :: ID _ :: IN :: xs -> xs |> for_tail
    | _ -> failwith "Expected FOR ID IN"

// for_tail -> array stmt_block | id stmt_block
and for_tail lst =
    match lst with 
    | LEFT_BRACKET :: _ -> lst |> arrayRule |> stmt_block 
    | ID _ :: xs -> xs |> stmt_block
    | _ -> failwith "Expected array or ID"

// fun_def -> def id (id_list) stmt_block
and fun_def lst =
    let match_RIGHT_PAREN_token =
        function
        | RIGHT_PAREN :: xs -> xs
        | _ -> failwith "Expected ')' (RIGHT_PAREN)"

    match lst with
    | DEF :: ID _ :: LEFT_PAREN :: xs -> xs |> id_list |> match_RIGHT_PAREN_token |> stmt_block
    | _ -> failwith "Expected DEF ID ("

// while_stmt -> while cond stmt_block
and while_stmt lst =
    match lst with
    | WHILE :: xs -> xs |> cond |> stmt_block
    | _ -> failwith "Expected WHILE"


// assign -> <- fun_call | = assign_tail
and assign lst =
    match lst with
    | ASSIGN_ARROW :: xs -> xs |> fun_call
    | ASSIGNMENT :: xs -> xs |> assign_tail
    | _ -> failwith "Expected assignment operator"

// assign_tail -> expr | array
and assign_tail lst =
    match lst with
    | ID _ :: _ | LEFT_PAREN :: _ -> lst |> expr
    | LEFT_BRACKET :: _ -> lst |> arrayRule
    | _ -> failwith "Expected expression or array"

// fun_call -> id ( param_list )
and fun_call lst =
    let match_RIGHT_PAREN_token =
        function
        | RIGHT_PAREN :: xs -> xs
        | _ -> failwith "Expected ')' (RIGHT_PAREN)"
    
    match lst with
    | ID _ :: LEFT_PAREN :: xs -> xs |> param_list |> match_RIGHT_PAREN_token
    | _ -> failwith "Expected ID or '(' (LEFT_PAREN)"

// array -> [ id_list ]
and arrayRule lst = 
    let match_RIGHT_BRACKET_token =
        function
        | RIGHT_BRACKET :: xs -> xs
        | _ -> failwith "Expected ']' (RIGHT_BRACKET)"

    match lst with
    | LEFT_BRACKET :: xs -> xs |> id_list |> match_RIGHT_BRACKET_token
    | _ -> failwith "Expected '[' (LEFT_BRACKET)"


// param_list -> expr param_tail | ε
and param_list lst =
    match lst with
    | ID _ :: _ | LEFT_PAREN :: _ -> lst |> expr |> param_tail
    | x -> x // ε

// param_tail , expr param_tail | ε
and param_tail lst =
    match lst with
    | COMMA :: xs -> xs |> expr |> param_tail
    | x -> x // ε

// id_list -> id id_tail | ε
and id_list lst = 
    match lst with
    | ID _ :: xs -> xs |> id_tail
    | _ -> failwith "Expected ID"

// id_tail -> , id_list | ε
and id_tail lst =
    match lst with
    | COMMA :: xs -> xs |> id_list
    | x -> x // ε

// expr -> id expr_tail | ( expr )
and expr lst =
    let match_RIGHT_PAREN_token =
        function
        | RIGHT_PAREN :: xs -> xs
        | _ -> failwith "Expected ')' (RIGHT_PAREN)"

    match lst with
    | ID _ :: xs -> xs |> expr_tail
    | LEFT_PAREN :: xs -> xs |> expr |> match_RIGHT_PAREN_token
    | _ -> failwith "Expected ID or '(' (LEFT_PAREN)"

// expr_tail -> arith_op expr | ε
and expr_tail lst =
    match lst with
    | (MINUS | ADD | MULTIPLY | FSLASH) :: _ -> lst |> arith_op |> expr
    | x -> x // ε

// arith_op -> - | + | * | /
and arith_op lst =
    match lst with
    | MINUS :: xs -> xs | ADD :: xs -> xs | MULTIPLY :: xs -> xs | FSLASH :: xs -> xs
    | _ -> failwith "Expected arithmetic operator"

// cond -> expr rel_oper expr
and cond lst =
// Nothing to check, so just follow and implement the rule as is.
    lst |> expr |> rel_oper |> expr

// rel_oper -> > | < | ==
and rel_oper lst =
    match lst with
    | GREATER_THAN :: xs -> xs | LESS_THAN :: xs -> xs | COMPARISON :: xs -> xs
    | _ -> failwith "Expected relational operator"    

(* ************************************************************************************************ *
 * !!!!!!!!!!!!!!!!!!!!!!!!! DO NOT MODIFY ANY CODE BELOW THIS POINT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
 * ************************************************************************************************ *)

// Import the regex module
open System.Text.RegularExpressions

// NOTE: To make the 'let' assignment be a function that accepts 0 parameters,
// an "empty tuple" must be accepted in ML/SML/OCaml/F#. (In the ML family, there 
// is no such thing as a function which does not accept a parameter.)
let main () =
    // A simple 'helper' to help debug the mapping process.
    let displayMapping str = printfn $"Mapping \"%s{str}\" -> %O{Token.tokenFromLexeme str}"; str

    // Convert a list of stings to Tokens:
    // This is very ".NET" specific. Regex.Matches is part of the (Microsoft).NET API.        
    let getTokenList str = 
        let tokenRegex = 
            @"(?x)" +  // "@" marks a regex string, (?x) is "PCRE style" Extended mode (ignore whitespace) 
            " >= | <= | => | =< | <> | == | != | >> | << | \|= | &= | \^= | := |" +
            " \+= | -= | \*= | /= | %= | && | \|\| | \+\+ | -- | <- | -> |" + 
            " \d+ | \w+ | [^\s]"
            
        // The regex matches the following tokens:
        Regex.Matches (str, tokenRegex) |> 
        (* Seq.cast<Match> |>  // This line is only necessary when FSI.EXE is running on ".NET Framework", not on ".NET Core" *)
        Seq.toList |>
        List.map string |>
        List.map _.ToLower() |>  // Yes, a violation of not using "methods", but much simpler in this case.
        List.map (displayMapping >> Token.tokenFromLexeme)


    // Begin the Parsing Process
    let startParsing str =
        // Display our list of tokens...
        printfn $"\nInitial String: %s{str}"
   
        let tokenList = getTokenList str
        printfn $"\nToken List BEFORE Parsing: %A{tokenList}"

        let parsedList = start tokenList
        printfn $"Tokens List AFTER Parsing: %O{parsedList}"


    // Get the user input and start parsing
    let getUserInput () =
        printf "Input String\n=> "    
        System.Console.ReadLine()  // Note that console I/O uses the .NET library in F#.  
                                   // For OCaml, See: https://ocaml.org/manual/5.2/api/Stdlib.html#VALread_line

    // Get the user input and start parsing
    in getUserInput () |> startParsing |> ignore  // Just ignore the result, as we are just printing results above.


// Uncomment the following lines to run the program in continuous a loop.
// This will allow you to enter multiple strings without restarting the program.

// *** YOU MAYUNCOMMENT THIS FOR TESTING, BUTCHANGE IT BACK BEFORE SUBMITTING!!! ***
// let rec run () : unit = main (); run () 
// run ()  

// Run the program once
main ()
