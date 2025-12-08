// Simple Okasaki Queue implementation
type Queue<'a> = 'a list * 'a list // front, back
module Queue =
    let empty: Queue<'a> = [], []

    let enqueue value ((front, back): Queue<'a>) = front, value :: back

    let dequeue (front, back) =
        match front, back with
        | head :: tail, back -> Some(head, (tail, back))
        | [], back ->
            match List.rev back with
            | [] -> None
            | head :: tail -> Some(head, (tail, []))

let input = System.IO.File.ReadAllLines "input.txt"

let grid =
    input
    |> Array.map _.ToCharArray()

let rows = grid.Length
let cols = grid[0].Length

// Find starting position 'S'
let startRow, startCol =
    let rowIdx, colIdx, _ =
        grid
        |> Seq.mapi (fun rowIdx row ->
            row
            |> Seq.mapi (fun colIdx cell -> rowIdx, colIdx, cell))
        |> Seq.concat
        |> Seq.find (fun (_, _, cell) -> cell = 'S')

    rowIdx, colIdx

// A beam is defined by the position it starts from
type Beam = { Row: int; Col: int }

// Find the first splitter hit by a beam moving downward from its starting position
// until the bottom of the grid
let findSplitter beam =
    seq { beam.Row .. rows - 1 }
    |> Seq.tryFind (fun row -> grid[row][beam.Col] = '^')
    |> Option.map (fun row -> (row, beam.Col))

// Generate new beams to the left and right from a splitter, within bounds
let generateBeamsFromSplitter (splitterRow, splitterCol) =
    [ if splitterCol - 1 >= 0 then
          { Row = splitterRow
            Col = splitterCol - 1 }
      if splitterCol + 1 < cols then
          { Row = splitterRow
            Col = splitterCol + 1 } ]

// BFS to find how many splitters were hit
let calculateHitSplittersCount initialBeam =
    let rec loop queue processed splitters =
        match Queue.dequeue queue with
        | None -> splitters
        | Some (beam, queueRest) ->
            let key = beam.Row, beam.Col

            if Set.contains key processed then
                loop queueRest processed splitters
            else
                let newProcessed = Set.add key processed

                match findSplitter beam with
                | Some splitter ->
                    let newSplitters = Set.add splitter splitters

                    let nextQueue =
                        generateBeamsFromSplitter splitter
                        |> List.fold (fun queue beam -> Queue.enqueue beam queue) queueRest

                    loop nextQueue newProcessed newSplitters
                | None -> loop queueRest newProcessed splitters

    loop (Queue.enqueue initialBeam Queue.empty) Set.empty Set.empty
    |> Set.count

let initialBeam = { Row = startRow + 1; Col = startCol }
let result = calculateHitSplittersCount initialBeam

printfn "%d" result
