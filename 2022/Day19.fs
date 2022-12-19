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


let part1 () =
    let minutes = 24

    let rec automate minute (bots: resource) (resources: resource) (botCosts: costs) =

        if minute = 0 then 
            resources.geode
        else
            let res = 
                seq [
                    // dont build anything
                    yield automate (minute - 1) bots (add resources bots) botCosts
                    
                    // if resources.ore >= botCosts.oreBotOreCost then 
                    //     let newResources = add resources bots
                    //     let newResources = { newResources with ore = newResources.ore - botCosts.oreBotOreCost }
                    //     yield automate (minute - 1) { bots with ore = bots.ore + 1 } newResources botCosts

                    if resources.ore >= botCosts.clayBotOreCost then 
                        let newResources = add resources bots
                        let newResources = { newResources with ore = newResources.ore - botCosts.clayBotOreCost }
                        yield automate (minute - 1) { bots with clay = bots.clay + 1 } newResources botCosts

                    // if resources.ore >= botCosts.obsidianBotOreCost && resources.clay >= botCosts.obsidianBotClayCost then 
                    //     let newResources = add resources bots
                    //     let newResources = 
                    //         { newResources with 
                    //             ore = newResources.ore - botCosts.obsidianBotOreCost
                    //             clay = newResources.clay - botCosts.obsidianBotClayCost }
                    //     yield automate (minute - 1) { bots with obsidian = bots.obsidian + 1 } newResources botCosts

                    // if resources.ore >= botCosts.geodeBotOreCost && resources.obsidian >= botCosts.geodeBotObsidianCost then 
                    //     let newResources = add resources bots
                    //     let newResources = 
                    //         { newResources with 
                    //             ore = newResources.ore - botCosts.geodeBotOreCost
                    //             obsidian = newResources.obsidian - botCosts.geodeBotObsidianCost }
                    //     yield automate (minute - 1) { bots with geode = bots.geode + 1 } newResources botCosts
                ] 
            res |> Seq.max
    
    // i dont think this is working because the calculations per minute rapidly expand beyond a processible range

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
            id * automate minutes { empty () with ore = 1 } (empty ()) botCosts))

let part2 () =
    0