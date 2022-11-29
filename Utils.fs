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

let prepend x xs = x :: xs

// List.zip implementation for lists of unequal length
let rec zip (xs: 'a list) (ys: 'b list) : ('a * 'b) list =
    match xs, ys with
    | [], [] -> []
    | [], y -> []
    | x, [] -> []
    | x :: t1, y :: t2 -> (x, y) :: zip t1 t2

// Zip two lists with applying a function on the tuple, a very good way to make a new list out of two
let zipWith (f: ('a -> 'b -> 'c)) (xs: 'a list) (ys: 'b list) =
    (xs, ys) ||> zip |> List.map (fun (x, y) -> f x y)

// Filter two lists by a predicate onto the first one
let filter2a (p: ('a -> bool)) (xs: 'a list) (ys: 'b list) : ('a * 'b) list =
    (xs, ys) ||> zip |> List.filter (fst >> p)

let filter2b (p: ('b -> bool)) (xs: 'a list) (ys: 'b list) : ('a * 'b) list =
    (xs, ys) ||> zip |> List.filter (snd >> p)

// Filter a list by a predicate and then map the new list
let mapFilter (f: ('a -> 'b)) (p: ('a -> bool)) (ls: 'a list) : 'b list = ls |> List.filter p |> List.map f

// FIlter the two lists by a predicate on the first one and then map it
let mapFilter2 (p: ('a -> bool)) (f: ('a -> 'b -> 'c)) (xs: 'a list) (ys: 'b list) : 'c list =
    (xs, ys) ||> filter2a p |> List.map (fun (x, y) -> f x y)

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

let inline (<&>) (f: ('a -> 'b)) (a: 'a option) = Option.map f a

let inline (<*>) (f: ('a -> 'b) option) (a: 'a option) : 'b option =
    match f with
    | None -> None
    | Some f' -> f' <&> a

let inline (>>=) (a: 'a option) (f: ('a -> 'b option)) =
    match a with
    | None -> None
    | Some x -> f x

let inline (>=>) (f1: ('a -> 'b option)) (f2: ('b -> 'c option)) : ('a -> 'c option) = fun x -> f1 x >>= f2

let join = Option.flatten

let inline (<|>) a b =
    match a, b with
    | None, None -> None
    | Some x, _ -> Some x
    | None, Some x -> Some x

let rec sequence (ls: ('a option) list) : ('a list) option =
    match ls with
    | [] -> Some []
    | None :: tail -> sequence tail
    | Some (x) :: tail -> prepend x <&> sequence tail

let some x =
    function
    | None -> x
    | Some a -> a

let isSome =
    function
    | None -> false
    | _ -> true

let isNone =
    function
    | None -> true
    | _ -> false

let fromSome =
    function
    | None -> failwith "Can't access a None"
    | Some a -> a
