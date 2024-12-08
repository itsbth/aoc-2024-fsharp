// Fetch https://adventofcode.com/2024/day/<d>/input and save to ./input/day<d>.txt
// Read session token from .session

open System
open System.IO
open System.Net.Http

let session = File.ReadAllText ".session"

let httpClient = new HttpClient()

let fetchInput day =
    task {
        let url = $"https://adventofcode.com/2024/day/{day}/input"
        let path = $"./input/day{day}.txt"
        use req = new HttpRequestMessage(HttpMethod.Get, url)
        req.Headers.Add("Cookie", $"session={session}")
        use! resp = httpClient.SendAsync(req, HttpCompletionOption.ResponseHeadersRead)

        if not resp.IsSuccessStatusCode then
            failwith $"Failed to fetch input for day {day}"

        use! stream = resp.Content.ReadAsStreamAsync()
        use fileStream = new FileStream(path, FileMode.Create, FileAccess.Write)
        do! stream.CopyToAsync(fileStream)
    }

// assume december :)
let today = DateTime.Today.Day

let day =
    match fsi.CommandLineArgs with
    // first is script path
    | [| _; d |] -> int d
    | _ -> today

printfn $"Fetching input for day %d{day}"
fetchInput day |> Async.AwaitTask
printfn "Done"