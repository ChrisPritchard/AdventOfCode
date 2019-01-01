module Model

type GroupKind = ImmuneSystem | Infection

type AttackType = Slashing | Bludgeoning | Cold | Radiation | Fire

type Group = {
    kind: GroupKind
    mutable units: int
    hitpoints: int
    weaknesses: AttackType list
    immunities: AttackType list
    attackType: AttackType
    attackStrength: int
    initiative: int
}
with 
    member __.effectivePower () = __.units * __.attackStrength
    member __.effectiveDamageTo other =
        if List.contains __.attackType other.immunities then 0
        else if List.contains __.attackType other.weaknesses then __.effectivePower () * 2
        else __.effectivePower ()
    member __.findTarget targets =
        targets 
        |> List.filter (fun group -> group.kind <> __.kind && group.units > 0)
        |> List.sortByDescending (fun o -> __.effectiveDamageTo o, o.effectivePower (), o.initiative)
        |> List.tryHead
        |> Option.bind (fun other -> if __.effectiveDamageTo other = 0 then None else Some other)
