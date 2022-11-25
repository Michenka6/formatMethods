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
    | Predicate b when evalB b memory -> memory
    | Predicate _ -> Undefined

let rec evalC (command: Command) (memory: Memory) =
    match command with
    | Assign (var, a) -> setVar var (evalA a memory) memory
    | ArrayAssign (var, index, a) -> setVar (var + "[" + string (evalA index memory) + "]") (evalA a memory) memory
    | Chain (c1, c2) -> evalC c2 (evalC c1 memory)
    | Skip -> memory
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

let rec evalP (f: Pred) (memory: Memory) (vMemory: VirtualMemory) =
    match f with
    | F b -> b
    | Conjuntion (f1, f2) -> evalP f1 memory vMemory && evalP f2 memory vMemory
    | Disjunction (f1, f2) -> evalP f1 memory vMemory || evalP f2 memory vMemory
    | Neg f1 -> not (evalP f1 memory vMemory)
    | Constraint (f1, f2) -> evalP f1 memory vMemory && evalP f2 memory vMemory
    | Exists (x, f1) ->
        match Map.tryFind x vMemory with
        | None -> false
        | Some _ -> evalP f1 memory vMemory
    | Forall (x, f1) ->
        match Map.tryFind x vMemory with
        | None -> false
        | Some _ -> evalP f1 memory vMemory
    | Equality (e1, e2) -> evalE e1 memory vMemory = evalE e2 memory vMemory
    | PredMap (p, es) -> p es

and evalE (e: Expr) (memory: Memory) (vMemory: VirtualMemory) =
    match e with
    | Concrete x -> getVar x memory
    | Virtual x -> Map.find x vMemory
    | AddExpr (e1, e2) -> evalE e1 memory vMemory + evalE e2 memory vMemory
    | SubExpr (e1, e2) -> evalE e1 memory vMemory - evalE e2 memory vMemory
    | TimesExpr (e1, e2) -> evalE e1 memory vMemory * evalE e2 memory vMemory
    | DivExpr (e1, e2) -> evalE e1 memory vMemory / evalE e2 memory vMemory
    | Map (f, es) -> f es

let getPred (node: Q) : Pred = failwith ""

let rec isCorrectPred (p: Pred) (Edge (node1, act, node2)) (memory: Memory) (vMemory: VirtualMemory) =
    let memory' = evalS act memory
    evalP (getPred node1) memory vMemory && evalP (getPred node2) memory' vMemory
