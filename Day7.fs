module Day7

open System.IO
open System.Text.Json

type operation =
    | Add
    | Mul

// <target>: <operand>[ <operand>]*
type row = { target: int64; operands: int64 list }

let parse (line: string) =
    let parts = line.Split(": ")
    let target = int64 parts.[0]
    let operands = parts.[1].Split(" ") |> Array.map int64 |> Array.toList
    { target = target; operands = operands }

let solve (fname: string) =
    let input = File.ReadAllLines fname |> Array.map parse

    let rec valid target tail acc =
        match acc > target, tail with
        | true, _ -> false
        | _, [] -> acc = target
        | _, h :: t -> (valid target t (acc + h)) || (valid target t (acc * h))

    printfn $"%A{input}"

    // debugging: ground truth via alternative implementation in Day7-valid.json (array of target values for valid rows)
    let groundTruth =
        File.ReadAllText "./Day7-valid.json"
        |> JsonSerializer.Deserialize
        |> Set.ofArray

    let part1 =
        input
        |> Array.filter (fun r -> valid r.target (List.tail r.operands) (List.head r.operands))
    
    let p1Set = part1 |> Array.map _.target |> Set.ofArray
    let falsePositives = p1Set - groundTruth
    let falseNegatives = groundTruth - p1Set
    
    printfn $"False positives: %A{falsePositives}"
    printfn $"False negatives: %A{falseNegatives}"
    
    Some(int (part1 |> Array.sumBy _.target)), None
