
type Spell = Recharge | Shield | Poison | MagicMissile | Drain
type Player = { hitPoints: int; armour: int; mana: int; effects: (Spell * int) list }
type Boss = { hitPoints: int; damage: int }

let allSpells = 
    [
        Shield,(113,6)
        Poison,(173,6)
        Recharge,(229,5)
        MagicMissile,(53,0)
        Drain,(73,0)
    ] |> Map.ofList

[<EntryPoint>]
let main _ =
    
    let boss = { hitPoints = 51; damage = 9 }
    let player = { hitPoints = 50; armour = 0; mana = 500; effects = [] }

    let spellActive spell player = 
        player.effects |> List.exists (fun (s, t) -> s = spell && t > 1)

    let turnSpells player =
        allSpells 
        |> Map.toList 
        |> List.filter (fun (spell, (c, _)) -> 
            c < player.mana && not <| spellActive spell player)
        |> List.map fst
    
    let castSpell player (boss: Boss) spell =
        match spell, allSpells.[spell] with
        | MagicMissile, (cost, _) -> 
            { player with mana = player.mana - cost },
            { boss with hitPoints = boss.hitPoints - 4 },
            cost
        | Drain, (cost, _) -> 
            { player with 
                mana = player.mana - cost
                hitPoints = max 50 (player.hitPoints + 2) },
            { boss with hitPoints = boss.hitPoints - 2 },
            cost
        | Shield, (cost, t) -> 
            { player with 
                mana = player.mana - cost
                effects = (Shield, t)::player.effects }, 
            boss,
            cost
        | Poison, (cost, t) -> 
            { player with 
                mana = player.mana - cost
                effects = (Poison, t)::player.effects }, 
            boss,
            cost
        | Recharge, (cost, t) -> 
            { player with 
                mana = player.mana - cost
                effects = (Recharge, t)::player.effects }, 
            boss,
            cost

    let applyEffects player boss = 
        let nextEffects = 
            player.effects 
            |> List.map (fun (s, t) -> s, t - 1) 
            |> List.filter (snd >> (<) 0)
        let player, boss = 
            player.effects
            |> List.fold (fun (p, (b: Boss)) (spell, _) ->
                match spell with
                | Shield -> { p with armour = 7 }, b
                | Poison -> p, { b with hitPoints = b.hitPoints - 3 }
                | Recharge -> { p with mana = p.mana + 101 }, b
                | _ -> p, b) (player, boss)
        { player with effects = nextEffects }, boss

    let rec fullTurn player boss spell = 
        let player = { player with armour = 0 }
        let player, boss = applyEffects player boss
        if boss.hitPoints <= 0 then Some 0
        else
            let player, boss, cost = castSpell player boss spell
            if boss.hitPoints <= 0 then 
                Some cost
            else
                let player, boss = applyEffects player boss
                if boss.hitPoints <= 0 then 
                    Some cost
                else
                    let player = 
                        { player with 
                            hitPoints = player.hitPoints - (boss.damage - player.armour) }
                    if player.hitPoints <= 0 then 
                        None
                    else
                        let nextSpells = turnSpells player
                        if List.isEmpty nextSpells then 
                            None
                        else
                            let spell = nextSpells |> List.sort |> List.head
                            fullTurn player boss spell

    let part1 = fullTurn player boss Shield
    printfn "part 1: %A" part1

    0