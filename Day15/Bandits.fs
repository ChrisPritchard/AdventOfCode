module Bandits

type GameResult = 
    | Victory of map: string array * turn: int * totalHealth: int
    | ElfDeath of turn: int

let runGame startMap elfAttack shouldFailOnElfDeath =
    Victory (startMap, 0, 0)