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