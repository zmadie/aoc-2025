let lines = System.IO.File.ReadAllLines "input.txt"

let blankLineIndex =
    lines
    |> Array.findIndex System.String.IsNullOrWhiteSpace

// Sort the ranges and merge overlapping/adjacent ranges before counting

let ranges =
    lines[0 .. blankLineIndex - 1]
    |> Seq.map (fun line ->
        let parts = line.Split '-'
        (uint64 parts[0], uint64 parts[1]))
    |> Seq.sortBy fst
    |> Seq.toList

let mergeRanges ranges =
    let rec loop acc ranges =
        match acc, ranges with
        // Usually the result is reversed in a tail recursive function, but counting of
        // the IDs in the ranges doesn't depend on the order, so a pass can be skipped here
        | _, [] -> acc
        | [], r :: rest -> loop [ r ] rest
        | (start1, end1) :: accTail, (start2, end2) :: rest ->
            // Merge if the next range overlaps/is adjacent with the current range,
            // else makes it a new range in the accumulator
            if start2 <= end1 + 1UL then
                loop ((start1, max end1 end2) :: accTail) rest
            else
                loop ((start2, end2) :: acc) rest

    loop [] ranges

let merged = mergeRanges ranges

let totalFreshIds =
    merged
    |> List.fold (fun count (start, end_) -> count + (end_ - start + 1UL)) 0UL

printfn "%d" totalFreshIds
