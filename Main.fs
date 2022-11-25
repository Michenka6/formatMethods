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
    printfn "%s" (showC (inputtoCommand gcdInput))




    // printfn "%A" (Undefined <|> (Mem Map.empty))

    // commandtoDeterPG traverseArrayCommand
    // makePG bubbleSort

    // printfn "%s" (showExecResult (executionSeq bitLevel bitLevelMemory))
    // printfn "%s" (string (isDeterministic nonDeter nonDeterMemory))
    0
