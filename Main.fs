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
    let command = inputtoCommand gcdInput

    if args = [||] then
        commandtoPG command

    for arg in args do
        match arg with
        | "-d" -> commandtoDeterPG command
        | _ -> ()



    0
