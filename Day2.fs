module Day2

open System
open System.IO
open Utils
open FSharpx.Prelude

let diff a =
    Seq.windowed 2 a |> Seq.map (fun a -> a.[1] - a.[0])

let valid a =
    Seq.forall (inrange 1 3) a || Seq.forall (inrange -3 -1) a

let valid2 (a: int array) =
    seq { 0 .. (Array.length a) } |> Seq.exists ((flip skipi) a >> diff >> valid)

let solve (file: string) : int option * int option =
    let input =
        File.ReadAllLines file
        |> Array.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

    input |> Array.map diff |> countp valid |> Some, None // input |> countp valid2 |> Some
