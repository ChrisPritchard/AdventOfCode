module Day07

open Common
open System

type File = {
    name : string
    size : int64
}

type Directory = {
    name : string
    files : File list
    dirs : Directory list
    totalSize : int64
}

type CmdLine =
    | Chdir of string
    | Listdir
    | DirName of string
    | FileName of string * int64

let asCmdLine line =
    let p = split " " line
    if p[0] = "dir" then
        DirName p[1]
    else if p[0] = "$" && p[1] = "cd" then
        Chdir p[2]
    else if p[0] = "$" then
        Listdir
    else
        let size = Int64.Parse p[0]
        FileName (p[1], size)

let rec createDirectory name dirs files (commands: CmdLine list) =
    let createStruct () =
        let totalSize =  List.sumBy (fun f -> f.size) files + List.sumBy (fun f -> f.totalSize) dirs
        { name = name; files = files; dirs = dirs; totalSize = totalSize }
    match commands with
    | (Chdir "..")::rem -> 
        createStruct (), rem
    | (Chdir n)::rem ->
        let subDir, rem = createDirectory n [] [] rem
        createDirectory name (subDir::dirs) files rem
    | (FileName (n, s))::rem ->
        createDirectory name dirs ({ name = n; size = s}::files) rem
    | _::rem ->
        createDirectory name dirs files rem
    | [] -> 
        createStruct (), []

let part1And2 () =

    let commandHistory = readEmbeddedRaw "day07" |> Seq.map asCmdLine |> List.ofSeq
    let rootDir = createDirectory "/" [] [] (List.skip 2 commandHistory) |> fst

    let rec under100k dir = 
        let sum = List.sumBy under100k dir.dirs
        if dir.totalSize < 100000L then sum + dir.totalSize else sum

    let totalSpace = 70000000L
    let minToDelete = totalSpace - rootDir.totalSize

    let rec minSuitable acc dir =
        if dir.totalSize < minToDelete then acc
        else if List.isEmpty dir.dirs then acc 
        else
            let acc = if dir.totalSize < acc then dir.totalSize else acc
            List.map (minSuitable acc) dir.dirs |> List.min

    under100k rootDir, minSuitable totalSpace rootDir
