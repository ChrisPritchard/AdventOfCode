(*
--- Day 4: Security Through Obscurity ---

Finally, you come across an information kiosk with a list of rooms. Of course, the list is encrypted and full of decoy data, but the instructions to decode the list are barely hidden nearby. Better remove the decoy data first.

Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a checksum in square brackets.

A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with ties broken by alphabetization. For example:

    aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x, y, and z, which are listed alphabetically.
    a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are listed alphabetically.
    not-a-real-room-404[oarel] is a real room.
    totally-real-room-200[decoy] is not.

Of the real rooms from the list above, the sum of their sector IDs is 1514.

What is the sum of the sector IDs of the real rooms?
*)

(*
--- Part Two ---

With all the decoy data out of the way, it's time to decrypt this list and get moving.

The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master cryptographer like yourself.

To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.

For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.

What is the sector ID of the room where North Pole objects are stored?
*)

module Day4

open System
open System.IO

let input = 
    File.ReadAllLines "Day4-input.txt" 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) 

let decrypt s n =
    s 
    |> Seq.map (fun c -> 
        if Char.IsLetter c then 
            let v = int c - int 'a'
            let nv = (v + n) % 26
            char (nv + int 'a')
        else c)
    |> fun chars -> String (Seq.toArray chars)

let parseLine (line: string) =
        let segments = line.Split('-')
        let checksum = segments.[segments.Length - 1].Split([|'[';']'|], StringSplitOptions.RemoveEmptyEntries)
        let mostCommon = 
            String.concat "" segments.[0..segments.Length-2] 
            |> Seq.countBy id 
            |> Seq.sortByDescending (fun o -> snd o, - int (fst o)) 
            |> Seq.map fst 
            |> Seq.truncate 5
            |> fun chars -> String (Seq.toArray chars)
        if mostCommon = checksum.[1] 
        then Some (int checksum.[0], decrypt line (int checksum.[0]))
        else None

let part1 () =
    input 
    |> Array.map parseLine 
    |> Array.choose id 
    |> Array.sumBy fst

let part2 () =
    input 
    |> Array.map parseLine 
    |> Array.choose id 
    |> Array.find (fun (_, s) -> s.Contains "north") |> fst