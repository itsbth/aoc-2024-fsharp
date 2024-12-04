module Day4

open System.IO

open FSharpPlus


let directions =
    [ (-1, 0); (0, 1); (1, 0); (0, -1); (-1, 1); (1, 1); (1, -1); (-1, -1) ]

let target = "XMAS"

let solve (file: string) =
    let input = File.ReadAllLines file |> Array.map _.ToCharArray()
    // find row and col of all X chars
    let potentialStarts =
        [ for i in 0 .. input.Length - 1 do
              for j in 0 .. input.[i].Length - 1 do
                  if input.[i].[j] = 'X' then
                      yield (i, j) ]

    let getChar i j =
        if i >= 0 && i < input.Length && j >= 0 && j < input.[0].Length then
            Some input.[i].[j]
        else
            None

    let getSpan i j di dj len =
        [ for k in 0 .. len - 1 do
              yield (i + k * di, j + k * dj) ]

    let count =
        potentialStarts
        |> List.map (fun (i, j) ->
            directions
            |> List.map (fun (di, dj) ->
                let span = getSpan i j di dj target.Length |> List.map (fun (i, j) -> getChar i j)

                match sequence span with
                | Some [ 'X'; 'M'; 'A'; 'S' ] -> 1
                | _ -> 0)
            |> List.sum)
        |> List.sum

    let potentialStartsA =
        [ for i in 0 .. input.Length - 1 do
              for j in 0 .. input.[i].Length - 1 do
                  if input.[i].[j] = 'A' then
                      yield (i, j) ]

    // diagonals
    let deltas = [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]

    let counts2 =
        potentialStartsA
        |> List.map (fun (i, j) ->
            match (deltas |> List.map (fun (di, dj) -> getChar (i + di) (j + dj)) |> sequence) with
            | Some [ 'M'; 'M'; 'S'; 'S' ] -> 1
            | Some [ 'S'; 'S'; 'M'; 'M' ] -> 1
            | Some [ 'M'; 'S'; 'M'; 'S' ] -> 1
            | Some [ 'S'; 'M'; 'S'; 'M' ] -> 1
            | _ -> 0)
        |> List.sum

    Some count, Some counts2
