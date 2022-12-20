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

type resource = {
    ore: int
    clay: int
    obsidian: int
    geode: int
}

let add r1 r2 = { 
    ore = r1.ore + r2.ore
    clay = r1.clay + r2.clay
    obsidian = r1.obsidian + r2.obsidian
    geode = r1.geode + r2.geode }

let empty () = {
    ore = 0
    clay = 0
    obsidian = 0
    geode = 0 }

let rec automate minute potentials (botCosts: costs) =
    let maxGeodes = potentials |> Seq.map (fun (_, r) -> r.geode) |> Seq.max
    if minute = 0 then 
        maxGeodes
    else
        let next = 
            potentials 
            |> Seq.collect (fun (bots, currentResources) ->
                let newResources = add bots currentResources
                seq [
                    yield (bots, newResources) // dont build any bots
                    if currentResources.ore >= botCosts.oreBotOreCost then 
                        let newResources = { newResources with ore = newResources.ore - botCosts.oreBotOreCost }
                        let newBots = { bots with ore = bots.ore + 1 }
                        yield newBots, newResources
                    if currentResources.ore >= botCosts.clayBotOreCost then 
                        let newResources = { newResources with ore = newResources.ore - botCosts.clayBotOreCost }
                        let newBots = { bots with clay = bots.clay + 1 }
                        yield newBots, newResources
                    if currentResources.ore >= botCosts.obsidianBotOreCost && currentResources.clay >= botCosts.obsidianBotClayCost then 
                        let newResources = 
                            { newResources with 
                                ore = newResources.ore - botCosts.obsidianBotOreCost
                                clay = newResources.clay - botCosts.obsidianBotClayCost }
                        let newBots = { bots with obsidian = bots.obsidian + 1 }
                        yield newBots, newResources
                    if currentResources.ore >= botCosts.geodeBotOreCost && currentResources.obsidian >= botCosts.geodeBotObsidianCost then 
                        let newResources = 
                            { newResources with 
                                ore = newResources.ore - botCosts.geodeBotOreCost
                                obsidian = newResources.obsidian - botCosts.geodeBotObsidianCost }
                        let newBots = { bots with geode = bots.geode + 1 }
                        yield newBots, newResources
                ])
            |> Seq.filter (fun (_, r) -> r.geode >= maxGeodes) |> Seq.distinct |> Seq.toArray
        automate (minute - 1) next botCosts

let part1 () =
    let minutes = 24

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
            id * automate minutes [|{ empty () with ore = 1 }, (empty ())|] botCosts))
    |> Seq.reduce (+)

let part2 () =
    let minutes = 32

    readEmbeddedRaw "day19"
    |> Seq.take 3
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
            automate minutes [|{ empty () with ore = 1 }, (empty ())|] botCosts))
    |> Seq.reduce (*)