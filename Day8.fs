module Day8

open System
open System.IO
open System.Diagnostics

type antenna = { x: int; y: int; channel: char }

type bounds =
    { maxX: int32
      maxY: int32
      minX: int32
      minY: int32 }

// 2d map: . empty space; any other char: antenna
let parse (lines: string array) =
    let points =
        lines
        |> Array.mapi (fun i line -> line.ToCharArray() |> Array.mapi (fun j c -> { x = i; y = j; channel = c }))
        |> Array.concat

    let bounds =
        points
        |> Array.fold
            (fun (minX, minY, maxX, maxY) p -> (min minX p.x, min minY p.y, max maxX p.x, max maxY p.y))
            (Int32.MaxValue, Int32.MaxValue, Int32.MinValue, Int32.MinValue)

    let (minX, minY, maxX, maxY) = bounds

    let boundRecord =
        { minX = minX
          minY = minY
          maxX = maxX
          maxY = maxY }

    let ant = points |> Array.filter (fun p -> p.channel <> '.')

    (boundRecord, ant)

let solve (fname: string) =
    let rsw = Stopwatch.StartNew()
    let content = File.ReadAllLines fname
    rsw.Stop()
    printfn $"Read file in {rsw.Elapsed}"
    rsw.Restart()
    let bounds, input = content |> parse
    rsw.Stop()
    printfn $"Parsed input in {rsw.Elapsed}"

    rsw.Restart()
    let byChannel = input |> Array.groupBy _.channel |> Map.ofArray
    rsw.Stop()
    printfn $"Common: {rsw.Elapsed}"

    rsw.Restart()
    let antinodes (ant: antenna array) =
        // antinodes are placed on locations that are in line with both, and exactly twice as far away from one as the other
        Array.allPairs ant ant
        |> Array.filter (fun (a, b) -> a.x <> b.x || a.y <> b.y)
        |> Array.collect (fun (a, b) ->
            let dx = a.x - b.x
            let dy = a.y - b.y
            // now that we have displacement vector, we can subtract it from a and add it to b to get both antinodes
            // we don't care about channel, use ! to indicate antinode
            [| { x = a.x + dx
                 y = a.y + dy
                 channel = '!' }
               { x = b.x - dx
                 y = b.y - dy
                 channel = '!' } |])

    let inBounds (p: antenna) =
        p.x >= bounds.minX
        && p.x <= bounds.maxX
        && p.y >= bounds.minY
        && p.y <= bounds.maxY

    let allAntiNodes =
        byChannel
        |> Map.values
        |> Seq.collect antinodes
        |> Set.ofSeq
        |> Set.filter inBounds
    
    rsw.Stop()
    printfn $"Part 1: {rsw.Elapsed}"

    rsw.Restart()

    let rec gcd a b = if b = 0 then a else gcd b (a % b)

    let slope (a: antenna) (b: antenna) =
        let dx = a.x - b.x
        let dy = a.y - b.y
        // make sure all lines point in the same direction
        let dx, dy =
            if dx < 0 then (-dx, -dy)
            elif dx = 0 then (0, abs dy)
            else (dx, dy)

        let g = gcd (abs dx) (abs dy)
        (dx / g, dy / g)

    // find the first point in the line in bounds (x & y >= 0)
    let start (a: antenna) (dx: int, dy: int) =
        if dx = 0 then
            let x = a.x

            if dy > 0 then (x, bounds.minY) else (x, bounds.maxY)
        elif dy = 0 then
            let y = a.y

            if dx > 0 then
                (bounds.minX, y)
            else
                // strictly speaking won't happen as we normalize slopes to dx >= 0
                (bounds.maxX, y)
        else
            let nCandidates =
                [ if dx > 0 then
                      (a.x - bounds.minX) / dx
                  else
                      //ditto
                      (bounds.maxX - a.x) / (-dx)
                  if dy > 0 then
                      (a.y - bounds.minY) / dy
                  else
                      (bounds.maxY - a.y) / (-dy) ]

            let n = List.min nCandidates |> max 0

            let startX = a.x - n * dx
            let startY = a.y - n * dy

            (startX, startY)

    let antinodes2 (ant: antenna array) =
        // all points that are line of any pair of antennas
        let allPoints =
            Array.allPairs ant ant
            |> Array.filter (fun (a, b) -> a.x <> b.x || a.y <> b.y)
            |> Seq.collect (fun (a, b) ->
                let (dx, dy) = slope a b
                let (x, y) = start a (dx, dy)

                let rec loop x y =
                    if x < bounds.minX || x > bounds.maxX || y < bounds.minY || y > bounds.maxY then
                        []
                    else
                        { x = x; y = y; channel = '!' } :: loop (x + dx) (y + dy)

                loop x y)

        allPoints

    let allAntiNodes2 = byChannel |> Map.values |> Seq.collect antinodes2 |> Set.ofSeq
    rsw.Stop()
    printfn $"Part 2: {rsw.Elapsed}"

    Some(allAntiNodes |> Set.count), Some(allAntiNodes2 |> Set.count)
