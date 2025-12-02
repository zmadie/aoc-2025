let ranges =
    System.IO.File.ReadAllText "input.txt"
    |> _.Split(',')
    |> Seq.map (fun range ->
        let parts = range.Split '-'
        parts[0], parts[1])

let invalidIds =
    ranges
    |> Seq.collect (fun (startStr, endStr) ->
        let startId = uint64 startStr
        let endId = uint64 endStr

        [ startId..endId ]
        |> Seq.filter (fun id ->
            let idStr = id.ToString()
            let len = idStr.Length

            // Instead of splitting in half and checking if it matches, check for all possible substring lengths that divide the total length
            [ 1 .. (len / 2) ]
            |> Seq.filter (fun subLen -> len % subLen = 0)
            |> Seq.exists (fun subLen ->
                let repeatCount = len / subLen
                let subStr = idStr.Substring(0, subLen)
                let reconstructed = String.replicate repeatCount subStr
                reconstructed = idStr)))

printfn "%d" (Seq.sum invalidIds)
