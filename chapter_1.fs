module book.Chapter_1

open AST
open Eval
open Utils

let rec stepsList (config: Config) (program: PG) =
    match config with
    | Config (_, Undefined) -> []
    | Config (FinalNode, _) -> [ [] ]
    | Config (q, memory) ->
        let edges = List.filter (fun (Edge (q0, _, _)) -> q0 = q) program
        let memories = List.map (fun (Edge (_, act, _)) -> evalS act memory) edges

        (memories, edges)
        ||> mapFilter2 ((<>) Undefined) (fun memory (Edge (_, act, q1)) -> (act, Config(q1, memory)))
        |> List.collect (fun (act, c) -> List.map (append (Step(config, act, c))) (stepsList c program))

let executionSeq (program: PG) (memory: Memory) =
    let steps = stepsList (Config(InitNode, memory)) program

    match steps with
    | [] -> Stuck
    | [ x ] -> Deter x
    | _ -> NonDeter steps

let deterministicS x xs =
    let case1 = List.forall ((<>) x) xs
    case1

let isDeterministic (program: PG) =
    program
    |> mapGroup (fun (Edge (q, _, _)) -> q) (List.map (fun (Edge (_, a, q)) -> (a, q)))
    |> checkForEach deterministicS

let isEvolvingSystem (program: PG) (memory: Memory) =
    match executionSeq program memory with
    | Stuck -> false
    | _ -> true
