module book.Eval

open AST

open Utils

let getVar s =
    function
    | Mem x when Map.containsKey s x -> Map.find s x
    | _ -> 0

let setVar s x =
    function
    | Undefined -> Undefined
    | Mem memory -> Mem(Map.add s x memory)

let rec evalA (aExpr: Aexpr) (memory: Memory) =
    match aExpr with
    | N x -> x
    | Var x -> getVar x memory
    | Arr (x, y) -> getVar (x + "[" + string (evalA y memory) + "]") memory
    | Add (x, y) -> evalA x memory + evalA y memory
    | Sub (x, y) -> evalA x memory - evalA y memory
    | Times (x, y) -> evalA x memory * evalA y memory
    | Div (x, y) -> evalA x memory / evalA y memory
    | Minus x -> -evalA x memory
    | Power (x, y) -> pown (evalA x memory) (evalA y memory)
    | ParanA x -> evalA x memory

let rec evalB (bExpr: Bexpr) (memory: Memory) =
    match bExpr with
    | B x -> x
    | BitAnd (x, y) -> evalB x memory &&& evalB y memory
    | BitOr (x, y) -> evalB x memory ||| evalB y memory
    | And (x, y) -> evalB x memory && evalB y memory
    | Or (x, y) -> evalB x memory || evalB y memory
    | Not x -> not (evalB x memory)
    | Eq (x, y) -> evalA x memory = evalA y memory
    | NEq (x, y) -> evalA x memory <> evalA y memory
    | Ls (x, y) -> evalA x memory < evalA y memory
    | LsEq (x, y) -> evalA x memory <= evalA y memory
    | Gr (x, y) -> evalA x memory > evalA y memory
    | GrEq (x, y) -> evalA x memory >= evalA y memory
    | ParanB x -> evalB x memory

let rec evalS (act: Act) (memory: Memory) =
    match act with
    | Ass (var, a) -> setVar var (evalA a memory) memory
    | ArrAss (var, index, a) -> setVar (var + "[" + string (evalA index memory) + "]") (evalA a memory) memory
    | DoNothing -> memory
    | Pred b when evalB b memory -> memory
    | Pred _ -> Undefined

let rec evalC (command: Command) (memory: Memory) =
    match command with
    | Assign (var, a) -> setVar var (evalA a memory) memory
    | ArrayAssign (var, index, a) -> setVar (var + "[" + string (evalA index memory) + "]") (evalA a memory) memory
    | Chain (c1, c2) -> evalC c2 (evalC c1 memory)
    | Skip -> memory
    | Break -> memory
    | Continue -> memory
    | If gc -> evalGC gc memory
    | Do gc ->
        match evalGC gc memory with
        | Undefined -> memory
        | newMemory -> evalC (Do gc) newMemory

and evalGC (gc: GuardCommand) (memory: Memory) =
    match gc with
    | Then (b, c) when evalB b memory -> evalC c memory
    | Then _ -> Undefined
    | Else (gc1, gc2) -> (evalGC gc1 memory) <|> (evalGC gc2 memory)
