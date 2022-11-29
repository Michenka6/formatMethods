module book.AST

type Q =
    | InitNode
    | FinalNode
    | Node of int

type Aexpr =
    | N of int
    | Var of string
    | Arr of string * Aexpr
    | Add of Aexpr * Aexpr
    | Sub of Aexpr * Aexpr
    | Times of Aexpr * Aexpr
    | Div of Aexpr * Aexpr
    | Minus of Aexpr
    | Power of Aexpr * Aexpr
    | ParanA of Aexpr

type Bexpr =
    | B of bool
    | BitAnd of Bexpr * Bexpr
    | BitOr of Bexpr * Bexpr
    | And of Bexpr * Bexpr
    | Or of Bexpr * Bexpr
    | Not of Bexpr
    | Eq of Aexpr * Aexpr
    | NEq of Aexpr * Aexpr
    | Ls of Aexpr * Aexpr
    | LsEq of Aexpr * Aexpr
    | Gr of Aexpr * Aexpr
    | GrEq of Aexpr * Aexpr
    | ParanB of Bexpr

type Act =
    | Ass of string * Aexpr
    | ArrAss of string * Aexpr * Aexpr
    | Predicate of Bexpr
    | Skip

type Edge = Q * Act * Q

type PG = Edge list

type Memory = Map<string, int>

type Sem = Act -> Memory -> Memory option
// evalS : S

type A = Aexpr -> Memory -> int option
// evalA : A

type B = Bexpr -> Memory -> bool option
// evalB : B

type Config = Q * Memory

type Step = Config * Act * Config

type GuardCommand =
    | Then of Bexpr * Command
    | Else of GuardCommand * GuardCommand

and Command =
    | Assign of string * Aexpr
    | ArrayAssign of string * Aexpr * Aexpr
    | Chain of Command * Command
    | If of GuardCommand
    | Do of GuardCommand
    | SkipGC

type ExecResult =
    | Stuck of Step list
    | Executed of Step list
