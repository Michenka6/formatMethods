module book.Functions

open AST

let factorial: PG =
    [ Edge(InitNode, Ass("y", N 1), Node 1)
      Edge(Node 1, Pred(Gr(Var "x", N 0)), Node 2)
      Edge(Node 1, Pred(LsEq(Var "x", N 0)), FinalNode)
      Edge(Node 2, Ass("y", Times(Var "x", Var "y")), Node 3)
      Edge(Node 3, Ass("x", Sub(Var "x", N 1)), Node 1) ]

let factorialMemory: Memory = Mem(Map.ofList [ ("x", 5) ])

let nthPowerof2: PG =
    [ Edge(InitNode, Ass("y", N 1), Node 1)
      Edge(Node 1, Pred(Gr(Var "x", N 0)), Node 2)
      Edge(Node 1, Pred(LsEq(Var "x", N 0)), FinalNode)
      Edge(Node 2, Ass("y", Times(N 2, Var "y")), Node 3)
      Edge(Node 3, Ass("x", Sub(Var "x", N 1)), Node 1) ]

let nthPowerof2Memory: Memory = Mem(Map.ofList [ ("x", 5) ])

let gcd: PG =
    [ Edge(InitNode, Pred(Eq(Var "x", Var "y")), FinalNode)
      Edge(InitNode, Pred(Gr(Var "x", Var "y")), Node 1)
      Edge(InitNode, Pred(Ls(Var "x", Var "y")), Node 2)
      Edge(Node 1, Ass("x", Sub(Var "x", Var "y")), InitNode)
      Edge(Node 2, Ass("y", Sub(Var "y", Var "x")), InitNode) ]

let gcdMemory = Mem(Map.ofList [ ("x", 5); ("y", 3) ])

let modulus: PG =
    [ Edge(InitNode, Pred(GrEq(Var "x", Var "y")), Node 1)
      Edge(Node 1, Ass("x", Sub(Var "x", Var "y")), InitNode)
      Edge(InitNode, Pred(Ls(Var "x", Var "y")), FinalNode) ]

let modulusMemory: Memory = Mem(Map.ofList [ ("x", 5); ("y", 3) ])

let nthFib: PG =
    [ Edge(InitNode, Ass("y", N 1), Node 1)
      Edge(Node 1, Pred(Or(Eq(Var "n", N 2), Eq(Var "n", N 1))), Node 8)
      Edge(Node 8, DoNothing, FinalNode)
      Edge(Node 1, Pred(Gr(Var "n", N 2)), Node 2)
      Edge(Node 2, Ass("x", N 1), Node 3)
      Edge(Node 3, Ass("n", Sub(Var "n", N 2)), Node 4)
      Edge(Node 4, Pred(LsEq(Var "n", N 0)), FinalNode)
      Edge(Node 4, Pred(Gr(Var "n", N 0)), Node 5)
      Edge(Node 5, Ass("y", Add(Var "x", Var "y")), Node 6)
      Edge(Node 6, Ass("x", Sub(Var "y", Var "x")), Node 7)
      Edge(Node 7, Ass("n", Sub(Var "n", N 1)), Node 4) ]

let nthFibMemory: Memory = Mem(Map.ofList [ ("n", 3) ])

let nonDeter: PG =
    [ Edge(InitNode, Pred(LsEq(Var "x", N 0)), Node 1)
      Edge(InitNode, Pred(GrEq(Var "x", N 0)), Node 2)
      Edge(Node 1, Ass("y", N(-1)), FinalNode)
      Edge(Node 2, Ass("y", N 1), FinalNode) ]

let nonDeterMemory: Memory = Mem(Map.ofList [ ("x", 0) ])

let stuckProg: PG =
    [ Edge(InitNode, Pred(Ls(Var "x", N 0)), Node 1)
      Edge(InitNode, Pred(Gr(Var "x", N 0)), Node 2)
      Edge(Node 1, Ass("y", N(-1)), FinalNode)
      Edge(Node 2, Ass("y", N 1), FinalNode) ]

let stuckProgMemory: Memory = Mem(Map.ofList [ ("x", 0) ])

let insertionSort: PG =

    let b = And(Gr(Var "j", N 0), Gr(Arr("A", Sub(Var "j", N 1)), Arr("A", Var "j")))

    [ Edge(InitNode, Ass("i", N 1), Node 1)
      Edge(Node 1, Pred(Ls(Var "i", Var "n")), Node 2)
      Edge(Node 1, Pred(GrEq(Var "i", Var "n")), FinalNode)
      Edge(Node 2, Ass("j", Var "i"), Node 3)
      Edge(Node 3, Pred b, Node 4)
      Edge(Node 4, ArrAss("A", Var "j", Add(Arr("A", Sub(Var "j", N 1)), Arr("A", Var "j"))), Node 5)
      Edge(Node 5, ArrAss("A", Sub(Var "j", N 1), Sub(Arr("A", Var "j"), Arr("A", Sub(Var "j", N 1)))), Node 6)
      Edge(Node 6, ArrAss("A", Var "j", Sub(Arr("A", Var "j"), Arr("A", Sub(Var "j", N 1)))), Node 7)
      Edge(Node 7, Ass("j", Sub(Var "j", N 1)), Node 3)
      Edge(Node 3, Pred(Not b), Node 8)
      Edge(Node 8, Ass("i", Add(Var "i", N 1)), Node 1) ]

let insertionSort2: PG =
    let b = And(Gr(Var "j", N 0), Gr(Arr("A", Sub(Var "j", N 1)), Arr("A", Var "j")))

    [ Edge(InitNode, Ass("i", N 1), Node 1)
      Edge(Node 1, Pred(Ls(Var "i", Var "n")), Node 2)
      Edge(Node 1, Pred(GrEq(Var "i", Var "n")), FinalNode)
      Edge(Node 2, Ass("j", Var "i"), Node 3)
      Edge(Node 3, Pred b, Node 4)
      Edge(Node 4, Ass("t", Arr("A", Var "j")), Node 5)
      Edge(Node 5, ArrAss("A", Var "j", Arr("A", Sub(Var "j", N 1))), Node 6)
      Edge(Node 6, ArrAss("A", Sub(Var "j", N 1), Var "t"), Node 3)
      Edge(Node 3, Pred(Not b), Node 7)
      Edge(Node 7, Ass("i", Add(Var "i", N 1)), Node 1) ]

let insertionSortMemory: Memory =
    Mem(
        Map.ofList
            [ ("n", 4)
              ("i", 1)
              ("j", 2)
              ("A[0]", 4)
              ("A[1]", 2)
              ("A[2]", 17)
              ("A[3]", 9) ]
    )

let bubbleSort =
    let b = And(Gr(Var "j", N 0), Gr(Arr("A", Sub(Var "j", N 1)), Arr("A", Var "j")))

    [ Edge(InitNode, Ass("i", N 1), Node 1)
      Edge(Node 1, Pred(Ls(Var "i", Var "n")), Node 2)
      Edge(Node 1, Pred(GrEq(Var "i", Var "n")), FinalNode)
      Edge(Node 2, Ass("j", N 1), Node 3)
      Edge(Node 3, Pred b, Node 4)
      Edge(Node 3, Pred(Not b), Node 7)
      Edge(Node 7, DoNothing, Node 8)
      Edge(Node 4, ArrAss("A", Var "j", Add(Arr("A", Sub(Var "j", N 1)), Arr("A", Var "j"))), Node 5)
      Edge(Node 5, ArrAss("A", Sub(Var "j", N 1), Sub(Arr("A", Var "j"), Arr("A", Sub(Var "j", N 1)))), Node 6)
      Edge(Node 6, ArrAss("A", Var "j", Sub(Arr("A", Var "j"), Arr("A", Sub(Var "j", N 1)))), Node 8)
      Edge(Node 8, Ass("j", Add(Var "j", N 1)), Node 9)
      Edge(Node 9, Pred(Ls(Var "j", Var "n")), Node 3)
      Edge(Node 9, Pred(GrEq(Var "j", Var "n")), Node 10)
      Edge(Node 10, Ass("i", Add(Var "i", N 1)), Node 1) ]

let bubbleSortMemory: Memory =
    Mem(
        Map.ofList
            [ ("n", 4)
              ("i", 1)
              ("j", 2)
              ("A[0]", 4)
              ("A[1]", 2)
              ("A[2]", 17)
              ("A[3]", 9) ]
    )

let innerProduct: PG =
    [ Edge(InitNode, Ass("i", N 0), Node 1)
      Edge(Node 1, Ass("j", N 0), Node 2)
      Edge(Node 2, Pred(GrEq(Var "i", Var "n")), FinalNode)
      Edge(Node 2, Pred(Ls(Var "i", Var "n")), Node 3)
      Edge(Node 3, Pred(GrEq(Var "j", Var "m")), Node 4)
      Edge(Node 3, Pred(Ls(Var "j", Var "m")), Node 6)
      Edge(Node 4, Ass("j", N 0), Node 5)
      Edge(Node 5, Ass("i", Add(Var "i", N 1)), Node 2)
      Edge(Node 6, Ass("y", Add(Var "y", Times(Arr("A", Var "i"), Arr("B", Var "j")))), Node 7)
      Edge(Node 7, Ass("j", Add(Var "j", N 1)), Node 2) ]

let innerProductMemory: Memory =
    Mem(
        Map.ofList
            [ ("n", 3)
              ("m", 2)
              ("A[0]", 0)
              ("A[1]", 1)
              ("A[2]", 2)
              ("B[0]", 0)
              ("B[1]", 1) ]
    )

let outerProduct: PG =
    [ Edge(InitNode, Ass("i", N 0), Node 1)
      Edge(Node 1, Ass("j", N 0), Node 2)
      Edge(Node 2, Pred(GrEq(Var "i", Var "n")), FinalNode)
      Edge(Node 2, Pred(Ls(Var "i", Var "n")), Node 3)
      Edge(Node 3, Pred(GrEq(Var "j", Var "m")), Node 4)
      Edge(Node 3, Pred(Ls(Var "j", Var "m")), Node 5)
      Edge(Node 4, Ass("i", Add(Var "i", N 1)), Node 7)
      Edge(
          Node 5,
          ArrAss("C", Add(Times(Var "i", Var "m"), Var "j"), Times(Arr("A", Var "i"), Arr("B", Var "j"))),
          Node 6
      )
      Edge(Node 6, Ass("j", Add(Var "j", N 1)), Node 3)
      Edge(Node 7, Ass("j", N 0), Node 2) ]

let outerProductMemory: Memory =
    Mem(
        Map.ofList
            [ ("n", 3)
              ("m", 2)
              ("A[0]", 1)
              ("A[1]", 2)
              ("A[2]", 3)
              ("B[0]", 1)
              ("B[1]", 2) ]
    )

let deter: PG =
    [ Edge(InitNode, Pred(Eq(Var "x", N 0)), Node 1)
      Edge(Node 1, Ass("y", N 0), FinalNode)
      Edge(InitNode, Pred(Not(Eq(Var "x", N 0))), Node 2)
      Edge(Node 2, Pred(LsEq(Var "x", N 0)), Node 3)
      Edge(Node 2, Pred(GrEq(Var "x", N 0)), Node 4)
      Edge(Node 3, Ass("y", N 1), FinalNode)
      Edge(Node 4, Ass("y", N 2), FinalNode) ]

let bitLevel: PG =
    [ Edge(InitNode, Ass("u", N 1), Node 1)
      Edge(Node 1, Pred(LsEq(Var "x", N 0)), FinalNode)
      Edge(Node 1, Pred(Gr(Var "x", N 0)), Node 2)
      Edge(Node 2, Pred(Gr(Var "y", N 0)), Node 3)
      Edge(Node 3, Ass("z", Add(Var "x", Var "y")), Node 4)
      Edge(Node 4, Pred(LsEq(Var "z", N 0)), Node 5)
      Edge(Node 5, Ass("u", N 0), FinalNode)
      Edge(Node 2, Pred(LsEq(Var "y", N 0)), FinalNode)
      Edge(Node 4, Pred(Gr(Var "z", N 0)), FinalNode) ]

let bitLevelMemory = Mem(Map.ofList [ ("x", 3); ("y", 2) ])
