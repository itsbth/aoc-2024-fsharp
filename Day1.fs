module Day1

open System.IO

let solve =
    let a1, a2 =
        File.ReadAllLines("input/day1.txt")
        |> Array.map (fun line ->
            match line.Split() with
            | [| a1; a2 |] -> int a1, int a2
            | _ -> failwith "invalid input")
        |> Array.unzip

    let a1, a2 = a1 |> Array.sort, a2 |> Array.sort

    printf "part 1: %d\n" (Array.zip a1 a2 |> Array.sumBy Utils.absoluteDifference)

    let counts = a2 |> Array.countBy id |> Map.ofArray

    printf
        "part 2: %d\n"
        (a1
         |> Array.sumBy (fun v1 ->
             match counts |> Map.tryFind v1 with
             | Some v2 -> v1 * v2
             | None -> 0))
