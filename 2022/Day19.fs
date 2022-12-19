module Day19

open Common

let part1 () =
    let minutes = 24

    let rec automate minutes 
        (oreBots, clayBots, obsidianBots, geodeBots) 
        (ore, clay, obsidians, geodes) 
        (oreBotOreCost, clayBotOreCost, obsidianBotOreCost, obsidianBotClayCost, geodeBotOreCost, geodeBotObsidianCost) =

        if minutes = 0 then geodes
        else
            automate (minutes - 1) 
                (oreBots, clayBots, obsidianBots, geodeBots) 
                (ore, clay, obsidians, geodes) 
                (oreBotOreCost, clayBotOreCost, obsidianBotOreCost, obsidianBotClayCost, geodeBotOreCost, geodeBotObsidianCost)

    readEmbeddedRaw "day19"
    |> Seq.map (fun line ->
        line |> split " :" |> (fun a -> 
            let id = int a[1]
            id * automate minutes (1, 0, 0, 0) (0, 0, 0, 0) (int a[6], int a[12], int a[18], int a[21], int a[27], int a[30])))

    // this could be manually processed, however there need to be decisions on when to build what
    // one way this could be done would be by calculating 'tails': 
    //  maximum number of bots is 24 = 1 + 23 (one built each minute except the last, when its pointless)
    //  the first ore bot will mine 24 ore over its lifetime...

let part2 () =
    0