module Model

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