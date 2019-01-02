open System
open System.IO
open FParsec.CharParsers
open FParsec

[<EntryPoint>]
let main _ =

    let pnum = spaces >>.pint32
    let pcoord = (pnum .>> pstring ", ") .>>. pnum
    let pline = (pstring "position=<" >>. pcoord) .>>. (pstring "> velocity=<" >>. pcoord .>> pchar '>')

    let lines = 
        File.ReadAllLines "input.txt"
        |> Seq.map (run pline >> function Success (r, _, _) -> r | Failure (ex, _, _) -> failwith ex) 
        |> Seq.toList

    let tick = List.map (fun ((x, y), (dx, dy)) -> (x + dx, y + dy), (dx, dy))

    let bounds = 
        List.fold (fun (ox, oy, omx, omy) ((x, y), _) -> 
            (if x < ox then x else ox),
            (if y < oy then y else oy),
            (if x > omx then x else omx),
            (if y > omy then y else omy))
            (Int32.MaxValue, Int32.MaxValue, 0, 0)

    let rec advanceTime n stars = 
        let (mx, my, maxx, maxy) = bounds stars 
        if maxy - my > 10 then 
            advanceTime (n + 1L) (tick stars)
        else
            let map = stars |> Seq.map fst |> Set.ofSeq
            n, [my..maxy] |> List.collect (fun y ->
                "\r\n"::([mx..maxx] |> List.map (fun x -> if Set.contains (x, y) map then "#" else " ")))
                |> System.String.Concat

    let time, result = advanceTime 0L lines
    printfn "%i" time
    printfn "%s" result

    0