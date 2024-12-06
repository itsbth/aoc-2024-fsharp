module Day6

open System.IO

type tile =
    | Empty
    | Obstacle
    | Start

type direction =
    | Up
    | Down
    | Left
    | Right

let turnRight =
    function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let next x y dir =
    match dir with
    | Up -> x - 1, y
    | Down -> x + 1, y
    | Left -> x, y - 1
    | Right -> x, y + 1

let charToTile =
    function
    | '.' -> Empty
    | '#' -> Obstacle
    | '^' -> Start
    | _ -> failwith "Invalid tile"

let solve (fpath: string) =
    let input =
        File.ReadAllLines fpath
        |> Array.map _.ToCharArray()
        |> Array.map (Array.map charToTile)

    let getAt x y =
        if x >= 0 && x < input.Length && y >= 0 && y < input.[0].Length then
            Some input.[x].[y]
        else
            None

    let startPos =
        [ for i in 0 .. input.Length - 1 do
              for j in 0 .. input.[i].Length - 1 do
                  if input.[i].[j] = Start then
                      yield (i, j) ]
            .Head

    let rec walk x y dir seen =
        let seen = Set.add (x, y) seen
        let nx, ny = next x y dir

        match getAt nx ny with
        | Some Empty
        | Some Start -> walk nx ny dir seen
        | Some Obstacle -> walk x y (turnRight dir) seen
        | _ -> seen

    let part1 = walk (fst startPos) (snd startPos) Up Set.empty
    // print out the map. Use X for visited tiles (in the seen/part1 set), otherwise use the input format
    let printTile =
        function
        | Empty -> '.'
        | Obstacle -> '#'
        | Start -> '^'

    input
    |> Array.mapi (fun i row ->
        row
        |> Array.mapi (fun j tile -> if Set.contains (i, j) part1 then 'X' else printTile tile))
    |> Array.iter (fun row -> printfn "%s" (System.String(row)))

    let rec terminates x y dir seen obsx obsy =
        let seen2 = Set.add (x, y, dir) seen
        let nx, ny = next x y dir

        let getAtWithObs x y =
            if x = obsx && y = obsy then Some Obstacle else getAt x y

        match Set.contains (x, y, dir) seen, getAtWithObs nx ny with
        | true, _ -> false
        | _, Some(Empty | Start) -> terminates nx ny dir seen2 obsx obsy
        | _, Some Obstacle -> terminates x y (turnRight dir) seen2 obsx obsy
        | _ -> true

    // sanity check
    // assert not (terminates (fst startPos) (snd startPos) Up Set.empty (fst startPos) (snd startPos - 1))

    // brute force for the win?
    // count the number of tiles that don't terminate
    let part2 =
        part1
        |> Set.toArray
        //|> Array.Parallel.map (fun (x, y) -> terminates (fst startPos) (snd startPos) Up Set.empty x y)
        |> Array.map (fun (x, y) -> terminates (fst startPos) (snd startPos) Up Set.empty x y)
        |> Array.sumBy (fun x -> if x then 0 else 1)

    part1 |> Set.count |> Some, Some part2
