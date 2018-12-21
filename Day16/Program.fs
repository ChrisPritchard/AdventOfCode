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
    ops |> List.sumBy (fun o -> if applies o example then 1 else 0)

let opCodeOptions (i, before, after) =
    List.head i,
    ops 
    |> List.mapi (fun i o -> i, o) 
    |> List.filter (fun (_, o) -> applies o (i, before, after))
    |> List.map fst
    |> Set.ofList

let rec reduceOptions (codeMap : Map<int, Set<int>>) =
    let contents = codeMap |> Map.toList
    if contents |> List.forall (fun (_, s) -> Set.count s = 1) then
        contents 
        |> List.map (fun (code, s) -> 
            let index = Set.minElement s
            code, ops.[index]) |> Map.ofList
    else
        let singles = 
            contents 
            |> List.filter (fun (_, s) -> s.Count = 1) 
            |> List.map (fun (_, s) -> s.MinimumElement)
        let next =
            contents
            |> List.map (fun (c, s) -> 
                if s.Count = 1 then c, s
                else c, s |> Set.toList |> List.except singles |> Set.ofList)
            |> Map.ofList
        reduceOptions next

let part2map examples =
    let options = 
        [0..15] 
        |> List.map (fun oi -> oi, [0..15] |> Set.ofList) 
        |> Map.ofList
    let filtered =
        examples |> List.fold (fun options example ->
            let code, valid = opCodeOptions example
            let filtered = Map.find code options |> Set.intersect valid
            Map.add code filtered options) options
    reduceOptions filtered

let apply codeMap state instruction =
    let code = List.head instruction
    let op = Map.find code codeMap
    op instruction.[1] instruction.[2] instruction.[3] state

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"
    let examples, instructions = parseInput input
    
    let part1 = examples |> List.sumBy (fun ex -> if (countApplies ex) >= 3 then 1 else 0)
    printfn "part1: %i" part1

    let codeMap = part2map examples
    let part2 = instructions |> List.fold (apply codeMap) [0;0;0;0]
    printfn "part2: %i" part2.[0]

    0
