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

// Processes a beam with its timeline count:
// if splitter found -> propagate its timeline counts to child beams
// if no splitter (reached the bottom) -> add its timelines to the total
let private expandBeam beam timelineCount pendingCounts queue =
    match findSplitter beam with
    | None -> pendingCounts, queue, timelineCount
    | Some splitter ->
        let newQueue, newPendingCounts =
            generateBeamsFromSplitter splitter
            |> List.fold
                (fun (queue, counts) nextBeam ->
                    let existingTimelines =
                        Map.tryFind nextBeam counts
                        |> Option.defaultValue 0UL

                    let newCounts = Map.add nextBeam (existingTimelines + timelineCount) counts

                    let newQueue =
                        if existingTimelines = 0UL then
                            Queue.enqueue nextBeam queue
                        else
                            queue

                    newQueue, newCounts)
                (queue, pendingCounts)

        newPendingCounts, newQueue, 0UL

// BFS, using a map to DP-aggregate timeline counts
let countTimelines initialBeam =
    let rec loop queue beamCounts timelineCount =
        match Queue.dequeue queue with
        | None -> timelineCount
        | Some (beam, queueRest) ->
            match Map.tryFind beam beamCounts with
            | None -> loop queueRest beamCounts timelineCount // Beam already expanded
            | Some beamTimelineCount ->
                let newPendingCounts, newQueue, completedTimelines =
                    expandBeam beam beamTimelineCount (Map.remove beam beamCounts) queueRest

                loop newQueue newPendingCounts (timelineCount + completedTimelines)

    loop (Queue.enqueue initialBeam Queue.empty) (Map.ofList [ initialBeam, 1UL ]) 0UL

let initialBeam = { Row = startRow + 1; Col = startCol }
let result = countTimelines initialBeam

printfn "%d" result
