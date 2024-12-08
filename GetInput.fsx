// Fetch https://adventofcode.com/2024/day/<d>/input and save to ./input/day<d>.txt
// Read session token from .session

open System
open System.IO
open System.Net.Http

let session = File.ReadAllText ".session"

let httpClient = new HttpClient()

type status =
    | Success
    | Exists
    | Error of string

let fetchInput day force =
    task {
        let url = $"https://adventofcode.com/2024/day/{day}/input"
        let path = $"./input/day{day}.txt"
        let exists = File.Exists path

        if not exists || force then
            use req = new HttpRequestMessage(HttpMethod.Get, url)
            req.Headers.Add("Cookie", $"session={session}")
            req.Headers.Add("User-Agent", "Itsbth-AoC/0.1 (github.com/itsbth/aoc-2024-fsharp)")
            use! resp = httpClient.SendAsync(req, HttpCompletionOption.ResponseHeadersRead)

            if not resp.IsSuccessStatusCode then
                return Error $"Failed to fetch input: {resp.StatusCode}"
            else
                use! stream = resp.Content.ReadAsStreamAsync()
                use fileStream = new FileStream(path, FileMode.Create, FileAccess.Write)
                do! stream.CopyToAsync(fileStream)
                return Success
        else
            return Exists
    }

type args = { day: int; force: bool; help: bool }

let parseArgs cli =
    let rec loop args =
        function
        | [] -> args
        | "-d" :: day :: rest -> loop { args with day = int day } rest
        | "-f" :: rest -> loop { args with force = true } rest
        | "-h" :: _ -> { args with help = true }
        | _ :: rest -> loop args rest

    loop
        { day = DateTime.Today.Day
          force = false; help = false }
        cli

let args = parseArgs <| List.ofArray fsi.CommandLineArgs

printfn $"Fetching input for day %d{args.day}"
if args.help then
    printfn $"Usage: GetInput [-d <day>] [-f] [-h]"
    printfn $"-d <day>  Day to fetch input for (default: %d{DateTime.Today.Day})"
    printfn $"-f        Force overwrite of existing input"
    printfn $"-h        Display this help message"
else
    match fetchInput args.day args.force |> Async.AwaitTask |> Async.RunSynchronously with
    | Success -> printfn "Done"
    | Exists -> printfn "Input already exists"
    | Error e -> printfn $"Error: {e}"
