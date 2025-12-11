type Tile = { X: int; Y: int }

type Segment = { Start: Tile; End: Tile }

type Rectangle =
    { Corner1: Tile
      Corner2: Tile }
    member this.XMin = min this.Corner1.X this.Corner2.X
    member this.XMax = max this.Corner1.X this.Corner2.X
    member this.YMin = min this.Corner1.Y this.Corner2.Y
    member this.YMax = max this.Corner1.Y this.Corner2.Y

let redTiles =
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map (fun line ->
        let coordinates = line.Split ',' |> Array.map int

        { X = coordinates[0]
          Y = coordinates[1] })

// The edges of the polygon formed by the red tiles, which are the green tiles
let greenTiles =
    [| { Start = redTiles[redTiles.Length - 1]
         End = redTiles[0] } |]
    |> Array.append (
        redTiles
        |> Array.pairwise
        |> Array.map (fun (start, end') -> { Start = start; End = end' })
    )

// Test whether a segment intersects the interior of a rectangle
// Note: the segments formed by the green tiles are axis-aligned
let segmentIntersectsRect (rect: Rectangle) (segment: Segment) =
    let x1, y1 = segment.Start.X, segment.Start.Y
    let x2, y2 = segment.End.X, segment.End.Y

    // If both x coordinates are the same, it's a vertical wall at x1
    // from min(y1, y2) to max(y1, y2)
    // Else, it's a horizontal wall at y1 from min(x1, x2) to max(x1, x2)
    if x1 = x2 then
        if x1 > rect.XMin && x1 < rect.XMax then
            let wallBottom = min y1 y2
            let wallTop = max y1 y2
            max rect.YMin wallBottom < min rect.YMax wallTop
        else
            false
    elif y1 > rect.YMin && y1 < rect.YMax then
        let wallLeft = min x1 x2
        let wallRight = max x1 x2
        max rect.XMin wallLeft < min rect.XMax wallRight
    else
        false

// Check if a rectangle is blocked by any green edge
let rectBlocked (rect: Rectangle) =
    greenTiles
    |> Array.exists (segmentIntersectsRect rect)

// Return the area of the rectangle if it is not blocked
let rectArea (rect: Rectangle) =
    if rectBlocked rect then
        None
    else
        Some(
            int64 (rect.XMax - rect.XMin + 1)
            * int64 (rect.YMax - rect.YMin + 1)
        )

let result =
    redTiles
    |> Array.indexed
    |> Array.collect (fun (i, corner1) ->
        redTiles
        |> Array.skip (i + 1)
        |> Array.choose (fun corner2 -> rectArea { Corner1 = corner1; Corner2 = corner2 }))
    |> Array.max

printfn "Result: %d" result
