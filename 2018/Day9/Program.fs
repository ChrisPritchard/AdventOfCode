open System
open System.IO
open FParsec.CharParsers
open FParsec

type Place = {
    mutable previous: Place
    value: int64
    mutable next: Place
} with
    member __.Rotate =
        function
        | 0 -> __
        | n when n > 0 -> __.next.Rotate (n - 1)
        | n -> __.previous.Rotate (n + 1)
    member __.Append marble =
        let newPlace = {
            next = __.next
            value = marble
            previous = __
        }
        __.next.previous <- newPlace
        __.next <- newPlace
        newPlace
    member __.Remove =
        __.previous.next <- __.next
        __.next.previous <- __.previous
        __.previous, __.value
     
let addScore player score scores = 
    let existing = match Map.tryFind player scores with Some n -> n | _ -> 0L
    Map.add player (existing + score) scores


let maxScore players maxMarble = 
    let start = { next = Unchecked.defaultof<Place>; value = 0L; previous = Unchecked.defaultof<Place> }
    start.next <- start
    start.previous <- start

    let _, finalScores =
        [1L..maxMarble] |> List.fold (fun ((current : Place), scores) marble ->
            if marble % 23L = 0L then
                let player = (marble - 1L) % players
                let next, removed = (current.Rotate -7).Remove
                next.Rotate 1, addScore player (removed + marble) scores
            else
                let next = (current.Rotate 1).Append marble
                next, scores) (start, Map.empty)
    finalScores |> Map.toList |> List.map snd |> List.max

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"
    let players, maxMarble = 
        match run (pint64 .>> pstring " players; last marble is worth " .>>. pint64 .>> pstring " points") input with
        | Success (r, _, _) -> r
        | _ -> failwith "invalid input"

    printfn "part 1: %i" <| maxScore players maxMarble
    printfn "part 2: %i" <| maxScore players (maxMarble*100L)

    0