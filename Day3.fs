module Day3

open System.IO

let (|Alpha|_|) (a: char) =
    // hack: single quote is allowed
    if a >= 'a' && a <= 'z' || a = '\'' then Some a else None

let (|Numeric|_|) (a: char) =
    if a >= '0' && a <= '9' then Some a else None

type token =
    | Ident of string
    | Num of int
    | RParen of unit
    | LParen of unit
    | Comma of unit
    | Invalid of unit

let tokenizeIdent (str: string) =
    let rec loop (acc: string) (str: string) : token * string =
        match str[0] with
        | Alpha(a) -> loop (acc + string a) str[1..]
        | _ -> Ident acc, str

    loop "" str

let tokenizeNum (str: string) =
    let rec loop (acc: string) (str: string) =
        match str[0] with
        | Numeric(a) -> loop (acc + string a) str[1..]
        | _ -> Num(int acc), str

    loop "" str

let tokenizeOne (str: string) =
    match str[0] with
    | '(' -> LParen(), str[1..]
    | ')' -> RParen(), str[1..]
    | ',' -> Comma(), str[1..]
    | Alpha(_) -> tokenizeIdent str
    | Numeric(_) -> tokenizeNum str
    | _ -> Invalid(), str[1..]

let tokenize (str: string) =
    let rec loop (acc: token list) (str: string) =
        match str with
        | "" -> acc
        | _ ->
            let tok, rest = tokenizeOne str
            loop (acc @ [ tok ]) rest

    loop [] str

/// We used greedy matching for idents, but e.g. xmul should be mul (strictly speaking Invalid::Ident("mul"), but we don't actually care about invalid tokens)
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
    | Do of unit
    | Dont of unit

let parse (tokens: token list) =
    let rec loop (acc: node list) (tokens: token list) =
        match tokens with
        | [] -> acc
        | Ident("mul") :: LParen() :: Num(a) :: Comma() :: Num(b) :: RParen() :: rest -> loop (acc @ [ Mul(a, b) ]) rest
        | Ident("do") :: LParen() :: RParen() :: rest -> loop (acc @ [ Do() ]) rest
        | Ident("don't") :: LParen() :: RParen() :: rest -> loop (acc @ [ Dont() ]) rest
        | _ :: rest -> loop acc rest

    loop [] tokens

type s =
    { Part1: int
      Part2: int
      Enabled: bool }

let solve () =
    let input = File.ReadAllText("input/day3.txt") |> tokenize |> fixupTokens

    let parsed = parse input

    let sum =
        List.fold
            (fun (acc: s) node ->
                match node with
                | Mul(a, b) ->
                    { acc with
                        Part1 = acc.Part1 + a * b
                        Part2 = if acc.Enabled then acc.Part2 + a * b else acc.Part2 }
                | Do() -> { acc with Enabled = true }
                | Dont() -> { acc with Enabled = false })
            { Part1 = 0; Part2 = 0; Enabled = true }
            parsed

    (Some sum.Part1, Some sum.Part2)
