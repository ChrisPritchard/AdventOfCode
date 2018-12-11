open System
open System.IO
open FParsec.Primitives
open FParsec.CharParsers

type Event = 
    | NewGuard of int
    | WakesUp
    | FallsAsleep

let pDateElem : Parser<int32, unit> = pint32 .>> (anyOf ['-';' ';':'])
let pTimeElem = 
    pDateElem .>>. pDateElem .>>. pDateElem .>>. pDateElem .>>. pDateElem 
    |>> fun ((((y, m), d), h), M) -> new DateTime(y, m, d, h, M, 0)
let pTime = pchar '[' >>. pTimeElem .>> pchar ']'

let pNewGuard = pstring " Guard #" >>. pint32 .>> pstring " begins shift" |>> NewGuard
let pWakesUp = pstring " wakes up" |>> fun _ -> WakesUp
let pFallsAsleep = pstring " falls asleep" |>> fun _ -> FallsAsleep
let pEvent = pNewGuard <|> pWakesUp <|> pFallsAsleep

let pLine = pTime .>>. pEvent

let processLine (line : string) =
    match run pLine line with
    | Success (result, _, _) -> result
    | _ -> failwith "invalid line"

[<EntryPoint>]
let main _ =
    
    //let lines = File.ReadAllLines "input.txt"
    let lines = [|
        "[1518-11-01 00:00] Guard #10 begins shift"
        "[1518-11-01 00:05] falls asleep"
        "[1518-11-01 00:25] wakes up"
        "[1518-11-01 00:30] falls asleep"
        "[1518-11-01 00:55] wakes up"
        "[1518-11-01 23:58] Guard #99 begins shift"
        "[1518-11-02 00:40] falls asleep"
        "[1518-11-02 00:50] wakes up"
        "[1518-11-03 00:05] Guard #10 begins shift"
        "[1518-11-03 00:24] falls asleep"
        "[1518-11-03 00:29] wakes up"
        "[1518-11-04 00:02] Guard #99 begins shift"
        "[1518-11-04 00:36] falls asleep"
        "[1518-11-04 00:46] wakes up"
        "[1518-11-05 00:03] Guard #99 begins shift"
        "[1518-11-05 00:45] falls asleep"
        "[1518-11-05 00:55] wakes up"
    |]

    let processed = lines |> Seq.map processLine |> Seq.toList

    0