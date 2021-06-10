module MonoClient.Cmdline

open System

type DateRange = { start: DateTime ; length: int64 }
type Request = {
    key: string
    range: DateRange
}

let parse (line: string[]) = 
    if Array.length line <> 3 then
        None
    else
        let year = line.[1] |> int
        let month = line.[2] |> int
        let start = DateTime.local (year,  month,  1)
        let length = int64 (start.AddMonths 1 - start).TotalSeconds - 1L
        { 
            key = line.[0]; 
            range = { 
                start = start; 
                length =  length
                } 
            } |> Some

let usage () =
    """Usage:
CLIENT <api key> <year> <month>"""
