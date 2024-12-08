open FSharpPlus

type solver = string -> int option * int option

let solvers =
    Map
        [ (1, Day1.solve)
          (2, Day2.solve)
          (3, Day3.solve)
          (4, Day4.solve)
          (6, Day6.solve)
          (7, Day7.solve)
          (8, Day8.solve) ]

[<EntryPoint>]
let main argv =
    let day, inputFile =
        match argv with
        | [| d; file |] ->
            match tryParse d with
            | Some n when solvers.ContainsKey(n) -> n, file
            | _ -> solvers.Keys |> Seq.max, "./input/day" + (solvers.Keys |> Seq.max).ToString() + ".txt"
        | [| d |] ->
            match tryParse d with
            | Some n when solvers.ContainsKey(n) ->
                let file = "./input/day" + n.ToString() + ".txt"
                n, file
            | _ ->
                let n = solvers.Keys |> Seq.max
                n, "./input/day" + n.ToString() + ".txt"
        | _ ->
            let n = solvers.Keys |> Seq.max
            n, "./input/day" + n.ToString() + ".txt"

    match Map.tryFind day solvers with
    | Some solve ->
        printfn $"Running solver for day %d{day} with input %s{inputFile}"
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let part1, part2 = solve inputFile
        sw.Stop()

        match part1, part2 with
        | Some p1, Some p2 -> printfn $"Part 1: %d{p1}\nPart 2: %d{p2}"
        | Some p1, None -> printfn $"Part 1: %d{p1}"
        | None, Some p2 -> printfn $"Part 2: %d{p2}"
        | None, None -> printfn "No solution found"

        printfn $"Elapsed: {sw.Elapsed}"

        0
    | None ->
        printfn $"No solver found for day %d{day}"
        1
