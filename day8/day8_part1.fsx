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

// Connect the 1000 closest junction boxes with Union-Find into circuits
let JUNCTION_BOX_PAIRS_COUNT = 1000
let unionFind = UnionFind junctionBoxes.Length

for i in 0 .. JUNCTION_BOX_PAIRS_COUNT - 1 do
    let nodeA, nodeB, _ = junctionBoxPairs[i]
    unionFind.Union(nodeA, nodeB) |> ignore

// Count how many junction boxes are in each circuit (represented by the root node of each set),
// sort descending to be able to get the 3 biggest circuits
let circuitSizes =
    seq { 0 .. junctionBoxes.Length - 1 }
    |> Seq.fold
        (fun counts boxId ->
            let root = unionFind.Find boxId

            counts
            |> Map.change root (function
                | None -> Some 1
                | Some v -> Some(v + 1)))
        Map.empty
    |> Map.values
    |> Seq.sortDescending

let result = circuitSizes |> Seq.take 3 |> Seq.reduce (*)

printfn "Result: %d" result
