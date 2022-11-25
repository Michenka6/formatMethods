module book.Chapter_2

open AST

open Utils

open Show

// Extract the Bexpr from a GuardCommand
let rec doneB =
    function
    | Then (b, c) -> Not b
    | Else (gc1, gc2) -> And(doneB gc1, doneB gc2)

// Non-deterministic  Program Graph
let rec edges (node1: Q) (node2: Q) index (command: Command) =
    match command with
    | Assign (var, a) -> [ Edge(node1, Ass(var, a), node2) ]
    | ArrayAssign (var, index, a) -> [ Edge(node1, ArrAss(var, index, a), node2) ]
    | Skip -> [ Edge(node1, DoNothing, node2) ]
    | Chain (c1, c2) ->
        (edges node1 (Node index) (2 * index) c1)
        @ (edges (Node index) node2 (2 * index + 1) c2)
    | If gc -> let b = doneB gc in Edge(node1, Predicate b, node2) :: edgesG node1 node2 index gc
    | Do gc -> Edge(node1, Predicate(doneB gc), node2) :: (edgesG node1 node1 index gc)

and edgesG (node1: Q) (node2: Q) index (gc: GuardCommand) : PG =

    match gc with
    | Then (b, c) -> Edge(node1, Predicate b, Node index) :: (edges (Node index) node2 (index * 2) c)
    | Else (gc1, gc2) -> (edgesG node1 node2 (2 * index) gc1) @ (edgesG node1 node2 (2 * index + 1) gc2)

let commandtoPG (c: Command) = makePG (edges InitNode FinalNode 1 c)

// ---------------------------------------------------------- //

// Deterministic Program graph
let rec edgesD (node1: Q) (node2: Q) index (command: Command) =
    match command with
    | Assign (var, a) -> [ Edge(node1, Ass(var, a), node2) ]
    | ArrayAssign (var, index, a) -> [ Edge(node1, ArrAss(var, index, a), node2) ]
    | Skip -> [ Edge(node1, DoNothing, node2) ]
    | Chain (c1, c2) ->
        (edgesD node1 (Node index) (2 * index) c1)
        @ (edgesD (Node index) node2 (2 * index + 1) c2)
    | If gc -> let (e, d) = edgesGD node1 node2 index gc (B false) in e
    | Do gc -> let (e, d) = edgesGD node1 node1 index gc (B false) in Edge(node1, Predicate(Not d), node2) :: e

and edgesGD (node1: Q) (node2: Q) index (gc: GuardCommand) (d: Bexpr) : (PG * Bexpr) =

    match gc with
    | Then (b, c) ->
        (Edge(node1, Predicate(And(b, Not d)), Node index)
         :: (edgesD (Node index) node2 (2 * index) c),
         Or(b, d))
    | Else (gc1, gc2) ->
        let (e1, d1) = edgesGD node1 node2 (2 * index) gc1 d
        let (e2, d2) = edgesGD node1 node2 (2 * index + 1) gc2 d1
        (e1 @ e2, d2)

let commandtoDeterPG (c: Command) = makePG (edgesD InitNode FinalNode 1 c)
