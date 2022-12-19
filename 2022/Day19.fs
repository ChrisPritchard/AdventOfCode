module Day19

open Common

type costs = {
    oreBotOreCost: int
    clayBotOreCost: int
    obsidianBotOreCost: int
    obsidianBotClayCost: int
    geodeBotOreCost: int 
    geodeBotObsidianCost: int
}

let part1 () =
    let minutes = 24

    let rec automate minute
        (oreBots, clayBots, obsidianBots, geodeBots) 
        (ore, clay, obsidian, geodes) 
        (botCosts: costs) =

        if minute = 0 then 
            geodes
        else
            let res = 
                seq [
                    // dont build anything
                    yield automate (minute - 1) (oreBots, clayBots, obsidianBots, geodeBots) (ore + oreBots, clay + clayBots, obsidian + obsidianBots, geodes + geodeBots) botCosts
                    
                    if ore >= botCosts.oreBotOreCost then 
                        yield automate (minute - 1) (oreBots + 1, clayBots, obsidianBots, geodeBots) (ore + oreBots - botCosts.oreBotOreCost, clay + clayBots, obsidian + obsidianBots, geodes + geodeBots) botCosts

                    if ore >= botCosts.clayBotOreCost then 
                        yield automate (minute - 1) (oreBots, clayBots + 1, obsidianBots, geodeBots) (ore + oreBots - botCosts.clayBotOreCost, clay + clayBots, obsidian + obsidianBots, geodes + geodeBots) botCosts

                    if ore >= botCosts.obsidianBotOreCost && clay >= botCosts.obsidianBotClayCost then 
                        yield automate (minute - 1) (oreBots, clayBots, obsidianBots + 1, geodeBots) (ore + oreBots - botCosts.obsidianBotOreCost, clay + clayBots - botCosts.obsidianBotClayCost, obsidian + obsidianBots, geodes + geodeBots) botCosts

                    if ore >= botCosts.geodeBotOreCost && obsidian >= botCosts.geodeBotObsidianCost then 
                        yield automate (minute - 1) (oreBots, clayBots, obsidianBots, geodeBots + 1) (ore + oreBots - botCosts.geodeBotOreCost, clay + clayBots, obsidian + obsidianBots - botCosts.geodeBotObsidianCost, geodes + geodeBots) botCosts
                ] 
            res |> Seq.max

    readEmbeddedRaw "day19"
    |> Seq.map (fun line ->
        line |> split " :" |> (fun a -> 
            let id = int a[1]
            let botCosts = { 
                oreBotOreCost = int a[6]
                clayBotOreCost = int a[12]
                obsidianBotOreCost = int a[18]
                obsidianBotClayCost = int a[21]
                geodeBotOreCost = int a[27]
                geodeBotObsidianCost = int a[30] }
            id * automate minutes (1, 0, 0, 0) (0, 0, 0, 0) botCosts))

let part2 () =
    0