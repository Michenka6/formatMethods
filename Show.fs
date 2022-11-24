module book.Show

open AST

open System.IO

let rec showQ =
    function
    | InitNode -> "q▷"
    | FinalNode -> "q◀"
    | Node x -> "q" + string x

let rec showA =
    function
    | N x -> string x
    | Var x -> x
    | Arr (x, y) -> x + "[" + showA y + "]"
    | Add (x, y) -> showA x + "+" + showA y
    | Sub (x, y) -> showA x + "-" + showA y
    | Times (x, y) -> showA x + "*" + showA y
    | Div (x, y) -> showA x + "/" + showA y
    | Minus x -> "-" + showA x
    | Power (x, y) -> showA x + "^" + showA y
    | ParanA x -> "(" + showA x + ")"

let rec showB =
    function
    | B x -> string x
    | BitAnd (x, y) -> showB x + "&" + showB y
    | BitOr (x, y) -> showB x + "|" + showB y
    | And (x, y) -> showB x + "&&" + showB y
    | Or (x, y) -> showB x + "||" + showB y
    | Not x -> "!(" + showB x + ")"
    | Eq (x, y) -> showA x + "=" + showA y
    | NEq (x, y) -> showA x + "!=" + showA y
    | Ls (x, y) -> showA x + "<" + showA y
    | LsEq (x, y) -> showA x + "<=" + showA y
    | Gr (x, y) -> showA x + ">" + showA y
    | GrEq (x, y) -> showA x + ">=" + showA y
    | ParanB x -> "(" + showB x + ")"

let rec showAct =
    function
    | Ass (x, a) -> x + ":=" + showA a
    | ArrAss (x, a, b) -> x + "[" + showA a + "]:=" + showA b
    | Pred x -> showB x
    | DoNothing -> "skip"

let rec showC command =
    match command with
    | Assign (var, a) -> var + " := " + showA a
    | ArrayAssign (var, index, a) -> var + "[ " + showA index + " ]:=" + showA a
    | Chain (c1, c2) -> showC c1 + "; " + showC c2
    | If gc -> "if " + showGC gc + " fi"
    | Do gc -> "do " + showGC gc + " od"
    | Skip -> "skip"
    | Break -> "break"
    | Continue -> "continue"

and showGC (gc: GuardCommand) =
    match gc with
    | Then (b, c) -> showB b + " -> " + showC c
    | Else (gc1, gc2) -> showGC gc1 + " [] " + showGC gc2



let showEdge (Edge (q1, act, q2)) =
    showQ q1 + " -> " + showQ q2 + " [label = \"" + showAct act + "\"];"

let showPG = List.map showEdge >> String.concat "\n"

let showMemory =
    function
    | Undefined -> "UNDEFINED MEMORY"
    | Mem mp ->
        ("", mp)
        ||> Map.fold (fun acc key value -> acc + " | " + key + " : " + string value)

let rec showConfig (Config (q, mem)) =
    "<" + showQ q + " ; " + showMemory mem + ">"

let rec showStep (Step (c1, act, c2)) =
    showConfig c1 + " == " + showAct act + " => " + showConfig c2 + "\n"

let rec showExecResult =
    function
    | Stuck -> "\nPROGRAM STUCK!!!\n"
    | Deter ls -> "\nDETERMINISTIC\n" + (List.map showStep >> String.concat "\n") ls
    | NonDeter ls ->
        let steps = List.map (List.map showStep >> String.concat "\n") ls

        "\nNON-DETERMINISTIC\n"
        + String.concat "----------------------------------------------\n" steps

let rec makePG program =
    let graphTemplate =
        "digraph program_graph {rankdir=LR;\nnode [shape = circle]; q▷;\nnode [shape = circle]; q◀;\nnode [shape = circle]\n"

    File.WriteAllLines("graph.gv", [| graphTemplate + showPG program + "}" |])
