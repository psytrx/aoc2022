module Day07

type Entry =
    { Name: string
      IsDir: bool
      mutable Size: int
      mutable Children: Entry list
      Parent: Entry option }

let rec findRoot dir =
    match dir.Parent with
    | None -> dir
    | Some parent -> findRoot parent

let parseTree lines =
    let rec aux dir lines =
        match lines with
        | [] -> findRoot dir
        | (line: string) :: rest ->
            if line.StartsWith("$ cd") then
                let name = Sscanf.sscanf "$ cd %s" line

                match name with
                | ".." ->
                    match dir.Parent with
                    | None -> failwith "parent directory does not exist"
                    | Some parent -> aux parent rest
                | "/" ->
                    let root = findRoot dir
                    aux root rest
                | name ->
                    let child =
                        dir.Children
                        |> List.find (fun dir -> dir.Name = name)

                    aux child rest
            else if line.StartsWith("dir ") then
                let name = Sscanf.sscanf "dir %s" line

                let child =
                    { Name = name
                      IsDir = true
                      Size = 0
                      Children = []
                      Parent = Some dir }

                dir.Children <- dir.Children @ [ child ]
                aux dir rest
            else if line = "$ ls" then
                aux dir rest
            else
                let size, name = Sscanf.sscanf "%i %s" line

                let entry =
                    { Name = name
                      IsDir = false
                      Size = size
                      Children = []
                      Parent = Some dir }

                let rec updateSize entry size =
                    if entry.IsDir then
                        entry.Size <- entry.Size + size

                    match entry.Parent with
                    | None -> ignore
                    | Some parent -> updateSize parent size

                dir.Children <- dir.Children @ [ entry ]
                updateSize dir size |> ignore
                aux dir rest

    let root =
        { Name = "/"
          IsDir = true
          Size = 1
          Children = []
          Parent = None }

    aux root lines


let solve1 filename =
    let tree =
        System.IO.File.ReadAllLines filename
        |> Array.toList
        |> parseTree

    let rec findDirectories acc entry =
        if entry.IsDir then
            let children =
                entry.Children
                |> List.collect (findDirectories acc)

            entry :: acc @ children
        else
            acc

    findDirectories [] tree
    |> List.filter (fun dir -> dir.Size <= 100000)
    |> List.sumBy (fun entry -> entry.Size)

let solve2 filename = -1
