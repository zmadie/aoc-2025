let tiles =
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.map (fun line ->
        let coordinates = line.Split ',' |> Array.map int
        coordinates[0], coordinates[1])

let maxArea =
    tiles
    |> Seq.indexed
    |> Seq.collect (fun (i, (x1, y1)) ->
        tiles
        |> Seq.skip (i + 1)
        |> Seq.map (fun (x2, y2) ->
            let w = abs (x2 - x1) + 1
            let h = abs (y2 - y1) + 1
            int64 w * int64 h))
    |> Seq.max

printfn "Result: %d" maxArea
