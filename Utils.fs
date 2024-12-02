module Utils

/// Calculate the absolute difference between two numbers.
let absoluteDifference (a, b) = abs (a - b)

/// Check if a value is within an inclusive range
let inrange l u v = l <= v && v <= u

/// Create complement of a predicate function
let complement f = fun x -> not (f x)

/// Count elements in a sequence that satisfy a predicate
let countp p = Seq.filter p >> Seq.length

/// Skip an element at specified index in a sequence
let skipi idx (sequence: seq<'a>) =
    sequence
    |> Seq.mapi (fun i x -> if i = idx then None else Some x)
    |> Seq.choose id
