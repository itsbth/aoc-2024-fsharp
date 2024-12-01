module Day1

open System.IO

let solve =
    let (a1, a2) =
        File.ReadAllLines("input/day1.txt")
        |> Array.map (fun l -> l.Split "   " |> (fun a -> (int a.[0], int a.[1])))
        |> Array.unzip

    let (a1, a2) = (a1 |> Array.sort, a2 |> Array.sort)

    printf "part 1: %d\n" (Array.zip a1 a2 |> Array.map (fun (a, b) -> abs (a - b)) |> Array.sum)

    let counts =
        (a2
         |> Array.fold (fun m v -> Map.change v (Option.defaultValue 0 >> fun c -> Some(c + 1)) m) Map.empty)

    printf
        "part 2: %d\n"
        (a1
         |> Array.map (fun v1 -> v1 * (counts |> Map.tryFind v1 |> Option.defaultValue 0))
         |> Array.sum)
