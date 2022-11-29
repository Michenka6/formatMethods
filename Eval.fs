module book.Eval

open AST

open Utils

let getVar s (memory: Memory) : int option = Map.tryFind s memory

let setVar s x (memory: Memory) : Memory option =
    match Map.tryFind s memory with
    | None -> None
    | Some _ -> Some(Map.add s x memory)

let rec evalA (aExpr: Aexpr) (memory: Memory) : int option =
    match aExpr with
    | N x -> Some x
    | Var x -> getVar x memory
    | Arr (x, y) -> getVar (x + "[" + string (evalA y memory) + "]") memory
    | Add (x, y) -> (+) <&> (evalA x memory) <*> (evalA y memory)
    | Sub (x, y) -> (-) <&> (evalA x memory) <*> (evalA y memory)
    | Times (x, y) -> (*) <&> (evalA x memory) <*> (evalA y memory)
    | Div (x, y) -> (/) <&> (evalA x memory) <*> (evalA y memory)
    | Minus x -> (-) 0 <&> (evalA x memory)
    | Power (x, y) -> pown <&> (evalA x memory) <*> (evalA y memory)
    | ParanA x -> evalA x memory

let rec evalB (bExpr: Bexpr) (memory: Memory) : bool option =
    match bExpr with
    | B x -> Some x
    | BitAnd (x, y) -> (&&&) <&> (evalB x memory) <*> (evalB y memory)
    | BitOr (x, y) -> (|||) <&> (evalB x memory) <*> (evalB y memory)
    | And (x, y) -> (&&) <&> (evalB x memory) <*> (evalB y memory)
    | Or (x, y) -> (||) <&> (evalB x memory) <*> (evalB y memory)
    | Not x -> not <&> (evalB x memory)
    | Eq (x, y) -> (=) <&> (evalA x memory) <*> (evalA y memory)
    | NEq (x, y) -> (<>) <&> (evalA x memory) <*> (evalA y memory)
    | Ls (x, y) -> (<) <&> (evalA x memory) <*> (evalA y memory)
    | LsEq (x, y) -> (<=) <&> (evalA x memory) <*> (evalA y memory)
    | Gr (x, y) -> (>) <&> (evalA x memory) <*> (evalA y memory)
    | GrEq (x, y) -> (>=) <&> (evalA x memory) <*> (evalA y memory)
    | ParanB x -> evalB x memory

let rec evalS (act: Act) (memory: Memory) : Memory option =
    match act with
    | Skip -> Some memory
    | Predicate b -> evalB b memory >>= (fun b -> if b then Some memory else None)
    | Ass (var, a) -> evalA a memory >>= (fun value -> setVar var value memory)
    | ArrAss (var, index, a) ->
        evalA index memory
        >>= (fun index ->
            evalA a memory
            >>= (fun value -> setVar ((var + "[" + string index + "]")) value memory))

let transition (program: PG) (config: Config) : (Act * Config) list =
    match config with
    | (FinalNode, _) -> []
    | (node, memory) ->
        let edges = List.filter (fun (q0, _, _) -> q0 = node) program
        let actsMemories = List.map (fun (_, act, _) -> (act, evalS act memory)) edges

        (edges, actsMemories)
        ||> filter2b (fun (_, y) -> isSome y)
        |> List.map (fun ((_, _, q), (act, memory)) -> (act, (q, fromSome memory)))

let rec iterate (program: PG) (config: Config) : Step list =
    match config with
    | (FinalNode, memory) -> []
    | (node, memory) ->
        match transition program config with
        | [] -> []
        | (act, nextConfig) :: _ -> (config, act, nextConfig) :: iterate program nextConfig

let runProgram (program: PG) (memory: Memory) : ExecResult =
    match iterate program (InitNode, memory) with
    | [] -> Stuck []
    | ls ->
        let (_, _, (q, _)) = List.last ls

        match q with
        | FinalNode -> Executed ls
        | _ -> Stuck ls















let rec doneB =
    function
    | Then (b, c) -> Not b
    | Else (gc1, gc2) -> And(doneB gc1, doneB gc2)

// Non-deterministic  Program Graph
let rec edges (node1: Q) (node2: Q) index (command: Command) : PG =
    match command with
    | Assign (var, a) -> [ (node1, Ass(var, a), node2) ]
    | ArrayAssign (var, index, a) -> [ (node1, ArrAss(var, index, a), node2) ]
    | SkipGC -> [ (node1, Skip, node2) ]
    | Chain (c1, c2) ->
        (edges node1 (Node index) (2 * index) c1)
        @ (edges (Node index) node2 (2 * index + 1) c2)
    | If gc -> edgesG node1 node2 index gc
    | Do gc -> (node1, Predicate(doneB gc), node2) :: (edgesG node1 node1 index gc)

and edgesG (node1: Q) (node2: Q) index (gc: GuardCommand) : PG =
    match gc with
    | Then (b, c) -> (node1, Predicate b, Node index) :: (edges (Node index) node2 (index * 2) c)
    | Else (gc1, gc2) -> (edgesG node1 node2 (2 * index) gc1) @ (edgesG node1 node2 (2 * index + 1) gc2)

let generate (c: Command) = edges InitNode FinalNode 1 c


let rec edgesD (node1: Q) (node2: Q) index (command: Command) : PG =
    match command with
    | Assign (var, a) -> [ (node1, Ass(var, a), node2) ]
    | ArrayAssign (var, index, a) -> [ (node1, ArrAss(var, index, a), node2) ]
    | SkipGC -> [ (node1, Skip, node2) ]
    | Chain (c1, c2) ->
        (edgesD node1 (Node index) (2 * index) c1)
        @ (edgesD (Node index) node2 (2 * index + 1) c2)
    | If gc -> let (e, d) = edgesGD node1 node2 index gc (B false) in e
    | Do gc -> let (e, d) = edgesGD node1 node1 index gc (B false) in (node1, Predicate(Not d), node2) :: e

and edgesGD (node1: Q) (node2: Q) index (gc: GuardCommand) (d: Bexpr) : (PG * Bexpr) =

    match gc with
    | Then (b, c) ->
        ((node1, Predicate(And(b, Not d)), Node index)
         :: (edgesD (Node index) node2 (2 * index) c),
         Or(b, d))
    | Else (gc1, gc2) ->
        let (e1, d1) = edgesGD node1 node2 (2 * index) gc1 d
        let (e2, d2) = edgesGD node1 node2 (2 * index + 1) gc2 d1
        (e1 @ e2, d2)

let generateDeterministic (c: Command) = edgesD InitNode FinalNode 1 c
