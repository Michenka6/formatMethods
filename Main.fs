module book.Main

open FSharp.Text.Lexing

open System

open AST
open Eval
open Show
open Utils
open Lexer
open Parser
open Chapter_1
open Chapter_2
open Input

let inputtoCommand (input: string) : Command =
    Parser.start Lexer.tokenize (LexBuffer<char>.FromString input)

[<EntryPoint>]
let main args =
    let command = inputtoCommand stuckProgramInput
    let memory = Mem(Map.ofList [ ("x", 5); ("y", 3) ])

    if args = [||] then
        makePG (commandtoPG command)

    for arg in args do
        match arg with
        | "-d" -> makePG (commandtoDeterPG command)
        | "--swe" ->
            let pg = commandtoPG command
            let execRes = showExecResult (executionSeq pg memory)
            printfn "%s" execRes
        | _ -> ()



    0
