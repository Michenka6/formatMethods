module book.Utils

open AST

let rec (&&&) (b1: bool) (b2: bool) =
    match (b1, b2) with
    | (true, true) -> true
    | (true, false) -> false
    | (false, true) -> false
    | (false, false) -> false

let rec (|||) (b1: bool) (b2: bool) =
    match (b1, b2) with
    | (true, true) -> true
    | (true, false) -> true
    | (false, true) -> true
    | (false, false) -> false

let curry f a b = f (a, b)

let uncurry f (a, b) = f a b

let append x xs = x :: xs

// List.zip implementation for lists of unequal length
let rec zip (xs: 'a list) (ys: 'b list) : ('a * 'b) list =
    match xs, ys with
    | [], [] -> []
    | [], y -> []
    | x, [] -> []
    | x :: t1, y :: t2 -> (x, y) :: zip t1 t2

// Zip two lists with applying a function on the tuple, a very good way to make a new list out of two
let zipWith (f: ('a -> 'b -> 'c)) (xs: 'a list) (ys: 'b list) =
    (xs, ys) ||> zip |> List.map (uncurry f)

// Filter two lists by a predicate onto the first one
let filter2 (p: ('a -> bool)) (xs: 'a list) (ys: 'b list) : ('a * 'b) list =
    (xs, ys) ||> zip |> List.filter (fst >> p)

// Filter a list by a predicate and then map the new list
let mapFilter (f: ('a -> 'b)) (p: ('a -> bool)) (ls: 'a list) : 'b list = ls |> List.filter p |> List.map f

// FIlter the two lists by a predicate on the first one and then map it
let mapFilter2 (p: ('a -> bool)) (f: ('a -> 'b -> 'c)) (xs: 'a list) (ys: 'b list) : 'c list =
    (xs, ys) ||> filter2 p |> List.map (uncurry f)

// Check if all elements are true given predicaste
let all (p: ('a -> bool)) (ls: 'a list) : bool =
    (true, ls) ||> List.fold (fun acc x -> p x && acc)

// Check if there is at least one true given a predicate in a list
let any (p: ('a -> bool)) (ls: 'a list) : bool =
    (false, ls) ||> List.fold (fun acc x -> p x || acc)

// Group a list into sublists by a key maker function, and them map each sublist
let mapGroup (p: ('a -> 'k)) (f: ('a list -> 'b)) (ls: 'a list) : 'b list =
    ls |> List.groupBy p |> List.map (fun (_, ls) -> f ls)

// Check if each element of a list is true given a predicate against the rest of the list
let checkForEach (p: ('a -> 'a list -> bool)) (ls: 'a list) =
    List.forall (fun x -> (x, List.filter ((<>) x) ls) ||> p) ls

// Implementation of Alternative for Memory
let inline (<|>) (mem1: Memory) (mem2: Memory) =
    match mem1, mem2 with
    | Undefined, Undefined -> Undefined
    | Undefined, (Mem x) -> Mem x
    | (Mem x), Undefined -> Mem x
    | (Mem x), (Mem y) -> Mem x
