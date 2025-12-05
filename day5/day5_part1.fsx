let lines = System.IO.File.ReadAllLines "input.txt"

let blankLineIndex =
    lines
    |> Array.findIndex System.String.IsNullOrWhiteSpace

let ranges =
    lines[0 .. blankLineIndex - 1]
    |> Seq.map (fun line ->
        let parts = line.Split '-'
        (uint64 parts[0], uint64 parts[1]))

let inline isFresh id =
    ranges
    |> Seq.exists (fun (start, end_) -> id >= start && id <= end_)

let freshCount =
    lines[blankLineIndex + 1 ..]
    |> Seq.fold
        (fun count line ->
            if isFresh (uint64 line) then
                count + 1
            else
                count)
        0

printfn "%d" freshCount
