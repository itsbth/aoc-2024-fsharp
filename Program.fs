open FSharpPlus

type solver = unit -> int option * int option

let solvers = Map [
    (1, Day1.solve)
    (2, Day2.solve)
]

[<EntryPoint>]
let main argv =
    let day = 
        match argv with
        | [|d|] -> 
            match tryParse d with
            | Some n when solvers.ContainsKey(n) -> n
            | _ -> solvers.Keys |> Seq.max
        | _ -> solvers.Keys |> Seq.max

    match Map.tryFind day solvers with
    | Some solve -> 
        printfn $"Running solver for day %d{day}"
        let part1, part2 = solve()
        match part1, part2 with
        | Some p1, Some p2 -> printfn $"Part 1: %d{p1}\nPart 2: %d{p2}"
        | Some p1, None -> printfn $"Part 1: %d{p1}"
        | None, Some p2 -> printfn $"Part 2: %d{p2}"
        | None, None -> printfn "No solution found"
        0
    | None ->
        printfn $"No solver found for day %d{day}"
        1