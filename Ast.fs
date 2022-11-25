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
    | DoNothing

type E = Edge of Q * Act * Q

type PG = E list

type Memory =
    | Undefined
    | Mem of Map<string, int>

type S = Act -> Memory -> Memory
// evalS : S

type A = Aexpr -> Memory -> int
// evalA : A

type B = Bexpr -> Memory -> bool
// evalB : B

type Config = Config of Q * Memory

type Step = Step of Config * Act * Config

type ExecResult =
    | Stuck
    | Deter of Step list
    | NonDeter of Step list list

type GuardCommand =
    | Then of Bexpr * Command
    | Else of GuardCommand * GuardCommand

and Command =
    | Assign of string * Aexpr
    | ArrayAssign of string * Aexpr * Aexpr
    | Chain of Command * Command
    | If of GuardCommand
    | Do of GuardCommand
    | Skip

type Pred =
    | F of bool
    | Conjuntion of Pred * Pred
    | Disjunction of Pred * Pred
    | Neg of Pred
    | Constraint of Pred * Pred
    | Exists of string * Pred
    | Forall of string * Pred
    | Equality of Expr * Expr
    | PredMap of (Expr list -> bool) * Expr list

and Expr =
    | Concrete of string
    | Virtual of string
    | AddExpr of Expr * Expr
    | SubExpr of Expr * Expr
    | TimesExpr of Expr * Expr
    | DivExpr of Expr * Expr
    | Map of (Expr list -> int) * Expr list

type VirtualMemory = Map<string, int>

type Define = Memory -> VirtualMemory -> Pred -> bool

type P = Q -> Pred
