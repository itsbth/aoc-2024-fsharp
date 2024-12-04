module Day3

open System.IO

type token =
    | Ident of string
    | Num of int
    | RParen
    | LParen
    | Comma
    | Invalid
    | EOF

let (|Punctuation|_|) (a: char) =
    match a with
    | '(' -> Some LParen
    | ')' -> Some RParen
    | ',' -> Some Comma
    | _ -> None

let createMatcher (pred: char -> bool) =
    let matcher (a: char list) : (char list * char list) option =
        let rec loop (str: char list) (rest: char list) =
            match rest with
            | a :: rest when pred a -> loop (a :: str) rest
            | _ -> (str |> List.rev), rest

        match loop [] a with
        | [], _ -> None
        | a, rest -> Some(a, rest)

    matcher

let (|Numeric|_|) = createMatcher (fun a -> a >= '0' && a <= '9')
// Domain: a..z, ' (to handle "don't"; ugly but that's the spec)
let (|Alpha|_|) = createMatcher (fun a -> a >= 'a' && a <= 'z' || a = '\'')

let charListToString (str: char list) = new string (str |> Array.ofList)

let tokenizeOne (str: char list) =
    match str with
    | Punctuation(a) :: rest -> a, rest
    | Alpha(ident, rest) -> Ident(charListToString ident), rest
    | Numeric(num, rest) -> Num(num |> charListToString |> int), rest
    | _ :: rest -> Invalid, rest
    | [] -> EOF, []

let tokenize (str: char list) =
    let rec loop (acc: token list) (str: char list) =
        match str with
        | [] -> List.rev acc
        | _ ->
            let tok, rest = tokenizeOne str
            loop (tok :: acc) rest

    loop List.empty str

/// We used greedy matching for idents, but e.g. xmul should be mul (strictly speaking Invalid :: Ident("mul"), but we don't actually care about invalid tokens)
/// EndsWith is strictly speaking not correct, but as the syntax only cares about identifiers immediately succeeded by a paren, it's good enough
let fixupTokens =
    List.map (fun (t: token) ->
        match t with
        | Ident(n) when n.EndsWith("mul") -> Ident("mul")
        | Ident(n) when n.EndsWith("do") -> Ident("do")
        | Ident(n) when n.EndsWith("don't") -> Ident("don't")
        | _ -> t)

type node =
    | Mul of int * int
    | Do
    | Dont

let parse (tokens: token list) =
    let rec loop (acc: node list) (tokens: token list) =
        match tokens with
        | [] -> List.rev acc
        | Ident("mul") :: LParen :: Num(a) :: Comma :: Num(b) :: RParen :: rest -> loop (Mul(a, b) :: acc) rest
        | Ident("do") :: LParen :: RParen :: rest -> loop (Do :: acc) rest
        | Ident("don't") :: LParen :: RParen :: rest -> loop (Dont :: acc) rest
        | _ :: rest -> loop acc rest

    loop [] tokens

type s =
    { Part1: int
      Part2: int
      Enabled: bool }

let solve (file: string) =
    let input =
        File.ReadAllText(file) |> Seq.toList |> tokenize |> fixupTokens

    let parsed = parse input

    let sum =
        List.fold
            (fun (acc: s) node ->
                match node with
                | Mul(a, b) ->
                    { acc with
                        Part1 = acc.Part1 + a * b
                        Part2 = if acc.Enabled then acc.Part2 + a * b else acc.Part2 }
                | Do -> { acc with Enabled = true }
                | Dont -> { acc with Enabled = false })
            { Part1 = 0; Part2 = 0; Enabled = true }
            parsed

    (Some sum.Part1, Some sum.Part2)
