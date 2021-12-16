module Day16

open Common
open System

let processed = readEmbedded "day16" 

let init () =
    processed |> Array.length |> ignore

let hexToBin (s: string) = 
    s.ToCharArray()
    |> Array.map (fun c -> 
        let s = string c
        Convert.ToString(Convert.ToInt32(s, 16), 2).PadLeft(4, '0'))
    |> String.concat ""

let asDec s = Convert.ToInt64(s, 2)

let version (s: string) = asDec s[0..2]

let typeID (s: string) = asDec s[3..5]

let literal (s: string) = 
    let rec reader acc (rem: string) = 
        let acc = acc + rem[1..4]
        if rem[0] = '0' then
            acc, rem[5..]
        else
            reader acc rem[5..]
    let res, rem = reader "" s
    asDec res, rem

let part1 () =
    let read = hexToBin processed[0]
    let rec parsePacket s =
        let v = version s
        let typeID = typeID s
        if typeID = 4L then
            let _, rem = literal s[6..]
            v, rem
        else
            if s[6] = '0' then
                let packetLength = asDec s[7..21]
                let packets = s[22..21 + int packetLength]
                let rec subParser acc rem = 
                    let (v, rem) = parsePacket rem
                    let acc = acc + v
                    if rem = "" then
                        acc
                    else
                        subParser acc rem
                let res = subParser v packets
                res, s[22 + int packetLength..]
            else
                let packets = asDec s[7..17]
                let rec subParser acc rem count = 
                    let (v, rem) = parsePacket rem
                    let acc = acc + v
                    let count = count - 1L
                    if count = 0L then
                        acc, rem
                    else
                        subParser acc rem count
                subParser v s[18..] packets
    parsePacket read |> fst

let part2 () =
    let read = hexToBin processed[0]
    let rec parsePacket s =
        let typeID = typeID s
        if typeID = 4L then
            literal s[6..]
        else
            let acc, rem =
                if s[6] = '0' then
                    let packetLength = asDec s[7..21]
                    let packets = s[22..21 + int packetLength]
                    let rec subParser acc rem = 
                        let (res, rem) = parsePacket rem
                        let acc = Array.append acc [|res|]
                        if rem = "" then
                            acc
                        else
                            subParser acc rem
                    let res = subParser Array.empty packets
                    res, s[22 + int packetLength..]
                else
                    let packets = asDec s[7..17]
                    let rec subParser acc rem count = 
                        let (res, rem) = parsePacket rem
                        let acc = Array.append acc [|res|]
                        let count = count - 1L
                        if count = 0L then
                            acc, rem
                        else
                            subParser acc rem count
                    subParser Array.empty s[18..] packets
            if typeID = 0L then
                Array.sum acc, rem
            else if typeID = 1L then
                Array.reduce (*) acc, rem
            else if typeID = 2L then
                Array.min acc, rem
            else if typeID = 3L then
                Array.max acc, rem
            else if typeID = 5L then
                (if acc[0] > acc[1] then 1L else 0L), rem
            else if typeID = 6L then
                (if acc[0] < acc[1] then 1L else 0L), rem
            else
                (if acc[0] = acc[1] then 1L else 0L), rem
    parsePacket read |> fst


