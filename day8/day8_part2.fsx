let junctionBoxes =
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map (fun line ->
        let coordinates = line.Split ','
        int coordinates[0], int coordinates[1], int coordinates[2])

let inline distance (x1, y1, z1) (x2, y2, z2) =
    let dx = float (x2 - x1)
    let dy = float (y2 - y1)
    let dz = float (z2 - z1)
    sqrt (dx * dx + dy * dy + dz * dz)

// Pairs of junction boxes, represented by unique IDs starting from 0, with their distances
let junctionBoxPairs =
    [| for i in 0 .. junctionBoxes.Length - 1 do
           for j in i + 1 .. junctionBoxes.Length - 1 do
               yield i, j, distance junctionBoxes[i] junctionBoxes[j] |]
    |> Array.sortBy (fun (_, _, dist) -> dist)

// Union-Find will represent each circuit as sets
type UnionFind(nodeCount: int) =
    // Each index represents a junction box ID
    let parent = Array.init nodeCount id
    let rank = Array.zeroCreate nodeCount

    member _.Find(node: int) =
        let mutable current = node

        while parent[current] <> current do
            parent[current] <- parent[parent[current]]
            current <- parent[current]

        current

    member this.Union(nodeA: int, nodeB: int) =
        let rootA = this.Find nodeA
        let rootB = this.Find nodeB

        if rootA = rootB then
            false
        else
            if rank[rootA] < rank[rootB] then
                parent[rootA] <- rootB
            elif rank[rootA] > rank[rootB] then
                parent[rootB] <- rootA
            else
                parent[rootB] <- rootA
                rank[rootA] <- rank[rootA] + 1

            true


let unionFind = UnionFind junctionBoxes.Length

// Union everything until there is only one set left Krustal-style,
// and return the x coordinates of the last pair connected
let findLastConnectedPair () =
    let rec loop remainingCircuits currentPairIndex =
        if currentPairIndex >= junctionBoxPairs.Length then
            failwith "Unable to connect all junction boxes"

        let nodeA, nodeB, _ = junctionBoxPairs[currentPairIndex]

        // If union was successful, check if now there is only one set left
        // If they were already merged, continue
        if unionFind.Union(nodeA, nodeB) then
            let remaining = remainingCircuits - 1

            if remaining = 1 then
                let (xA, _, _) = junctionBoxes[nodeA]
                let (xB, _, _) = junctionBoxes[nodeB]
                xA, xB
            else
                loop remaining (currentPairIndex + 1)
        else
            loop remainingCircuits (currentPairIndex + 1)

    loop junctionBoxes.Length 0

let result =
    findLastConnectedPair ()
    |> fun (xA, xB) -> uint64 xA * uint64 xB

printfn "Result: %d" result
