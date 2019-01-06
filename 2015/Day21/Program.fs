
type Fighter = { hitPoints: int; damage: int; armour: int }

let weapons = [
    8, 4
    10, 5
    25, 6
    40, 7
    74, 8
]

let armour = [
    0, 0
    13, 1
    31, 2
    53, 3
    75, 4
    102, 5
]

let rings = [
    0, 0, 0
    25, 1, 0
    50, 2, 0
    100, 3, 0
    20, 0, 1
    40, 0, 2
    80, 0, 3
]

[<EntryPoint>]
let main _ =

    let boss = { hitPoints = 104; damage = 8; armour = 1 }
    let player = { hitPoints = 100; damage = 0; armour = 0 }

    let playerOptions = 
        weapons |> Seq.collect (fun (c1, wd) -> 
        armour |> Seq.collect (fun (c2, aa) ->
        rings |> Seq.collect (fun (c3, r1d, r1a) ->
        rings |> Seq.filter (fun r -> r = (0, 0, 0) || r <> (c3, r1a, r1d))
        |> Seq.map (fun (c4, r2d, r2a) -> 
            c1 + c2 + c3 + c4, 
            { player with 
                damage = player.damage + wd + r1d + r2d
                armour = player.armour + aa + r1a + r2a } ))))
        |> Seq.sortBy fst

    let rec fight player boss =
        let nextBoss = 
            { boss with 
                hitPoints = boss.hitPoints - max 1 (player.damage - boss.armour) }
        if nextBoss.hitPoints <= 0 then true
        else
            let nextPlayer = 
                { player with 
                    hitPoints = player.hitPoints - max 1 (boss.damage - player.armour) }
            if nextPlayer.hitPoints <= 0 then false
            else
                fight nextPlayer nextBoss

    let part1 = playerOptions |> Seq.find (fun (_, player) -> fight player boss) |> fst
    printfn "part 1: %i" part1

    let part2 = playerOptions |> Seq.rev |> Seq.find (fun (_, player) -> not <| fight player boss) |> fst
    printfn "part 2: %i" part2

    0
