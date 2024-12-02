module Day2

open System
open System.IO

open FSharpx.Prelude

let inrange l u v = l <= v && v <= u
let complement f = fun x -> not (f x)

let countp p = Seq.filter p >> Seq.length

let diff a =
    Seq.windowed 2 a |> Seq.map (fun a -> a.[1] - a.[0])

let valid a =
    Seq.forall (inrange 1 3) a || Seq.forall (inrange -3 -1) a

let skipi idx (sequence: seq<'a>) =
    sequence
    |> Seq.mapi (fun i x -> if i = idx then None else Some x)
    |> Seq.choose id

let valid2 (a: int array) =
    // correctness by accident: seq { .. } is inclusive, so we end up including the index past the last element, therefore also testing the whole array
    seq { 0 .. (Array.length a) } |> Seq.exists ((flip skipi) a >> diff >> valid)

let solve () : int option * int option =
    let input =
        File.ReadAllLines("input/day2.txt")
        |> Array.map (fun line -> line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

    input |> Array.map diff |> countp valid |> Some, None // input |> countp valid2 |> Some
