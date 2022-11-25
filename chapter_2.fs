module book.Chapter_2

open AST

open Utils

open Show

let maxCommand: Command =
    If(Else(Then(GrEq(Var "x", Var "y"), Assign("z", Var "x")), Then(Gr(Var "y", Var "x"), Assign("z", Var "y"))))

let factorialCommand: Command =
    Chain(
        Assign("y", N 1),
        Do(Then(Gr(Var "x", N 0), Chain(Assign("y", Times(Var "x", Var "y")), Assign("x", Sub(Var "x", N 1)))))
    )

let nthPowerof2Command: Command =
    Chain(
        Assign("y", N 1),
        Do(Then(Gr(Var "x", N 0), Chain(Assign("y", Times(Var "y", N 2)), Assign("x", Sub(Var "x", N 1)))))
    )

let modulusCommand: Command =
    Do(Then(GrEq(Var "x", Var "y"), Assign("x", Sub(Var "x", Var "y"))))

let gcdCommand: Command =
    Do(
        Then(
            NEq(Var "x", Var "y"),
            If(
                Else(
                    Then(Ls(Var "x", Var "y"), Assign("y", Sub(Var "y", Var "x"))),
                    Then(Gr(Var "x", Var "y"), Assign("x", Sub(Var "x", Var "y")))
                )
            )
        )
    )

let nthFibCommand =
    Chain(
        Assign("y", N 1),
        If(
            Else(
                Then(LsEq(Var "n", N 2), Skip),
                Then(
                    Gr(Var "n", N 2),
                    Chain(
                        Assign("x", N 1),
                        Chain(
                            Assign("n", Sub(Var "n", N 2)),
                            Do(
                                Then(
                                    Gr(Var "n", N 0),
                                    Chain(
                                        Assign("y", Add(Var "x", Var "y")),
                                        Chain(Assign("x", Sub(Var "y", Var "x")), Assign("n", Sub(Var "n", N 1)))
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

let traverseArrayCommand: Command =
    Chain(
        Assign("i", N 0),
        Chain(
            Assign("x", N 0),
            Chain(
                Assign("y", N 0),
                Do(
                    Then(
                        Ls(Var "i", N 10),
                        Chain(
                            If(
                                Else(
                                    Then(
                                        GrEq(Arr("A", Var "i"), N 0),
                                        Chain(
                                            Assign("x", Add(Var "x", Arr("A", Var "i"))),
                                            Assign("i", Add(Var "i", N 1))
                                        )
                                    ),
                                    Then(Ls(Arr("A", Var "i"), N 0), Chain(Assign("i", Add(Var "i", N 1)), Continue))
                                )
                            ),
                            Assign("y", Add(Var "y", N 1))
                        )
                    )
                )
            )
        )
    )

let traverseArrayMemory: Memory =
    Mem(
        Map.ofList
            [ ("A[0]", 0)
              ("A[1]", 1)
              ("A[2]", 2)
              ("A[3]", 3)
              ("A[4]", 4)
              ("A[5]", 5)
              ("A[6]", 6)
              ("A[7]", 7)
              ("A[8]", 8)
              ("A[9]", 09) ]
    )

// Extract the Bexpr from a GuardCommand
let rec doneB =
    function
    | Then (b, c) -> Not b
    | Else (gc1, gc2) -> And(doneB gc1, doneB gc2)

// Non-deterministic  Program Graph
let rec edges (node1: Q) (node2: Q) index (command: Command) (nodeB: Q) (nodeC: Q) =
    match command with
    | Assign (var, a) -> [ Edge(node1, Ass(var, a), node2) ]
    | ArrayAssign (var, index, a) -> [ Edge(node1, ArrAss(var, index, a), node2) ]
    | Skip -> [ Edge(node1, DoNothing, node2) ]
    | Break -> [ Edge(node1, DoNothing, nodeC) ]
    | Continue -> [ Edge(node1, DoNothing, nodeB) ]
    | Chain (c1, c2) ->
        (edges node1 (Node index) (2 * index) c1 nodeB nodeC)
        @ (edges (Node index) node2 (2 * index + 1) c2 nodeB nodeC)
    | If gc -> let b = doneB gc in Edge(node1, Pred b, node2) :: edgesG node1 node2 index gc nodeB nodeC
    | Do gc -> Edge(node1, Pred(doneB gc), node2) :: (edgesG node1 node1 index gc node1 node2)

and edgesG (node1: Q) (node2: Q) index (gc: GuardCommand) (nodeB: Q) (nodeC: Q) : PG =

    match gc with
    | Then (b, c) ->
        Edge(node1, Pred b, Node index)
        :: (edges (Node index) node2 (index * 2) c nodeB nodeC)
    | Else (gc1, gc2) ->
        (edgesG node1 node2 (2 * index) gc1 nodeB nodeC)
        @ (edgesG node1 node2 (2 * index + 1) gc2 nodeB nodeC)

let commandtoPG (c: Command) =
    makePG (edges InitNode FinalNode 1 c InitNode FinalNode)

// ---------------------------------------------------------- //

// Deterministic Program graph
let rec edgesD (node1: Q) (node2: Q) index (command: Command) (nodeB: Q) (nodeC: Q) =
    match command with
    | Assign (var, a) -> [ Edge(node1, Ass(var, a), node2) ]
    | ArrayAssign (var, index, a) -> [ Edge(node1, ArrAss(var, index, a), node2) ]
    | Skip -> [ Edge(node1, DoNothing, node2) ]
    | Break -> [ Edge(node1, DoNothing, nodeC) ]
    | Continue -> [ Edge(node1, DoNothing, nodeB) ]
    | Chain (c1, c2) ->
        (edgesD node1 (Node index) (2 * index) c1 nodeB nodeC)
        @ (edgesD (Node index) node2 (2 * index + 1) c2 nodeB nodeC)
    | If gc -> let (e, d) = edgesGD node1 node2 index gc (B false) nodeB nodeC in e
    | Do gc -> let (e, d) = edgesGD node1 node1 index gc (B false) node1 node2 in Edge(node1, Pred(Not d), node2) :: e

and edgesGD (node1: Q) (node2: Q) index (gc: GuardCommand) (d: Bexpr) (nodeB: Q) (nodeC: Q) : (PG * Bexpr) =

    match gc with
    | Then (b, c) ->
        (Edge(node1, Pred(And(b, Not d)), Node index)
         :: (edgesD (Node index) node2 (2 * index) c nodeB nodeC),
         Or(b, d))
    | Else (gc1, gc2) ->
        let (e1, d1) = edgesGD node1 node2 (2 * index) gc1 d nodeB nodeC
        let (e2, d2) = edgesGD node1 node2 (2 * index + 1) gc2 d1 nodeB nodeC
        (e1 @ e2, d2)

let commandtoDeterPG (c: Command) =
    makePG (edgesD InitNode FinalNode 1 c InitNode FinalNode)
