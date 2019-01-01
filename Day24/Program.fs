open System
open System.IO
open FParsec.CharParsers
open FParsec

type AttackType = Slashing | Bludgeoning | Cold | Radiation | Fire
type Group = {
    units: int
    hitpoints: int
    weaknesses: AttackType list
    immunities: AttackType list
    attackType: AttackType
    attackStrength: int
    initiative: int
}

type Defenses = WeakTo of AttackType list | ImmuneTo of AttackType list
let attackTypeFrom = 
    function
    | "slashing" -> Slashing
    | "bludgeoning" -> Bludgeoning
    | "cold" -> Cold
    | "radiation" -> Radiation
    | "fire" -> Fire
    | _ -> failwith "attack type not recognised"

let parseInput input = 
    let pheader s = pstring s .>>. newline
    let punitshp =
        (pint32 .>> pstring " units each with ")
        .>>.
        (pint32 .>> pstring " hit points ")
    let pattackType = pstring "slashing" <|> pstring "bludgeoning" <|> pstring "cold" <|> pstring "radiation" <|> pstring "fire"
    let pweakto = pstring "weak to " >>. sepBy1 pattackType (pstring ", ") |>> fun list -> WeakTo <| List.map attackTypeFrom list
    let pimmune = pstring "immune to " >>. sepBy1 pattackType (pstring ", ") |>> fun list -> ImmuneTo <| List.map attackTypeFrom list
    let pdefense = pchar '(' >>. sepBy1 (pweakto <|> pimmune) (pstring "; ") .>> pstring ") "
    let pattack = pstring "with an attack that does " >>. pint32 .>>. (pchar ' ' >>. pattackType) .>>. (pstring " damage at initiative " >>. pint32)
    let pgroup = 
        punitshp .>>. opt pdefense .>>. pattack .>> opt newline
        |>> fun (((units, hitpoints), defenses), ((attackStrength, attackType), initiative)) ->
            let weaknesses = match defenses |> Option.map (fun d -> List.tryPick (function | WeakTo list -> Some list | _ -> None) d) with Some (Some list) -> list | _ -> []
            let immunities = match defenses |> Option.map (fun d -> List.tryPick (function | ImmuneTo list -> Some list | _ -> None) d) with Some (Some list) -> list | _ -> []
            {
                units = units
                hitpoints = hitpoints
                weaknesses = weaknesses
                immunities = immunities
                attackType = attackTypeFrom attackType
                attackStrength = attackStrength
                initiative = initiative
            }

    let pall =( pheader "Immune System:" >>. many pgroup) .>>. (newline >>. pheader "Infection:" >>. many pgroup)
    match run pall input with
    | Success(r, _, _) -> r
    | Failure(ex, _, _) -> failwith ex

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"
    let immuneGroups, infectionGroups = parseInput input

    0
