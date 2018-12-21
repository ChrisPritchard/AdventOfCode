open System
open System.IO
open FParsec.CharParsers
open FParsec

type Input = 
    | Example of instruction: int list * before: int list * after: int list
    | Input of int list

let parseInput input = 
    let pRegState = sepBy pint32 (pstring ", ") .>> pstring "]"
    let pBefore = pstring "Before: [" >>. pRegState
    let pAfter = pstring "After:  [" >>. pRegState
    let pInstruction = parray 4 (pint32 .>> opt (pchar ' ')) .>> newline |>> Array.toList
    let pExample = (pBefore .>> newline) .>>. pInstruction .>>. pAfter |>> fun ((b, i), a) -> Example (i, b, a)
    let pInput = many ((pExample .>> spaces) <|> (pInstruction |>> Input))

    let parsed = 
        match run pInput input with
        | Success (r, _, _) -> r
        | Failure (e, _, _) -> failwith e

    parsed 
    |> List.fold (fun (es, is) -> 
        function 
        | Example (i, b, a) -> (i, b, a)::es, is 
        | Input list -> es, list::is) ([], [])
    |> fun (e, i) -> e, List.rev i

let withC c registers cval = 
    registers |> List.mapi (fun i v -> if i = c then cval else v)

let addr a b c (registers: int list) =
    registers.[a] + registers.[b] |> withC c registers

let addi a b c (registers: int list) =
    registers.[a] + b |> withC c registers

let mulr a b c (registers: int list) =
    registers.[a] * registers.[b] |> withC c registers

let muli a b c (registers: int list) =
    registers.[a] * b |> withC c registers

let banr a b c (registers: int list) =
    registers.[a] &&& registers.[b] |> withC c registers

let bani a b c (registers: int list) =
    registers.[a] &&& b |> withC c registers

let borr a b c (registers: int list) =
    registers.[a] ||| registers.[b] |> withC c registers

let bori a b c (registers: int list) =
    registers.[a] ||| b |> withC c registers

let setr a _ c (registers: int list) =
    registers.[a] |> withC c registers

let seti a _ c (registers: int list) =
    a |> withC c registers

let gtir a b c (registers: int list) =
    (if a > registers.[b] then 1 else 0) |> withC c registers

let gtri a b c (registers: int list) =
    (if registers.[a] > b then 1 else 0) |> withC c registers

let gtrr a b c (registers: int list) =
    (if registers.[a] > registers.[b] then 1 else 0) |> withC c registers

let eqir a b c (registers: int list) =
    (if a = registers.[b] then 1 else 0) |> withC c registers

let eqri a b c (registers: int list) =
    (if registers.[a] = b then 1 else 0) |> withC c registers

let eqrr a b c (registers: int list) =
    (if registers.[a] = registers.[b] then 1 else 0) |> withC c registers

let ops = [
    addr; addi; mulr; muli
    banr; bani; borr; bori
    setr; seti
    gtir; gtri; gtrr
    eqir; eqri; eqrr
]

let applies op (i: int list, before, after) =
    let a, b, c = i.[1], i.[2], i.[3]
    op a b c before = after

let countApplies example = 
    let result = ops |> List.sumBy (fun o -> if applies o example then 1 else 0)
    result

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"
    let examples, _ = parseInput input
    
    let part1 = examples |> List.sumBy (fun ex -> if (countApplies ex) >= 3 then 1 else 0)
    printfn "part1: %i" part1

    0
