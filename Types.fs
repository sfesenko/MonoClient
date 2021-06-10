namespace MonoClient

open System

module DateTime =
    let start = DateTime(1970,1,1,0,0,0,0, DateTimeKind.Utc)

    let toLocalTime (epoch: uint64) = 
        (start.AddSeconds (float epoch)).ToLocalTime ()

    let toEpoch (date: DateTime) =
        (date - start).TotalSeconds |> int64
    
    let local (year, month, day) = 
        DateTime (year, month, day, 0, 0, 0, DateTimeKind.Local)
