module MonoClient.Main

open System

type Statement = {
    id: string
    date: DateTime
    description: string
    amount: int64
    comment: string option
}

module MonobankConverter = 
    
    let mapStatement (st: Monobank.Statement) = {
        id = st.id
        date = DateTime.toLocalTime st.time
        description = st.description
        amount = st.amount
        comment = st.comment
    }

let statementWriter (map: Map<string, string>) (dt: DateTime) (statements: Statement seq) = 
    let tr src = 
        Map.tryFind src map |> Option.defaultValue src
    let sb = Text.StringBuilder ()
    let source = tr "Assets:Monobank:Card"
    sb.Append $"{dt:``yyyy/MM/dd``} *\n  {source}\n" |> ignore
    statements
    |> Seq.map (fun s ->
        let amount = - (float) s.amount / 100.0
        let description = s.description.Replace ("\n"," .. ") |> tr
        let comment = 
            s.comment |> Option.map (sprintf " ; %s") |> Option.defaultValue ""
        $"  {description,-30} {amount,-30:``#,#.#0 UAH``}{comment}")
    |> String.concat "\n"
    |> sb.Append 
    |> string


let show (param: Cmdline.Request) = 
    use h = Monobank.Api.create param.key

    let fromTime = param.range.start.ToUniversalTime () |> DateTime.toEpoch
    let tillTime = fromTime + param.range.length

    let trMap = Map.empty |> Map.add "АТБ" "Expences:Food:Super:ATB"

    Monobank.Api.statements h 0 fromTime tillTime
    |> Async.RunSynchronously
    |> Array.map MonobankConverter.mapStatement
    |> Array.groupBy (fun s -> s.date.Date)
    |> Array.map (fun (d, a) -> statementWriter trMap d a)
    |> String.concat "\n;\n"
    |> printfn "%O"

[<EntryPoint>]
let main argv =
    match Cmdline.parse argv with
    | None -> Cmdline.usage()  |> printfn "%s"
    | Some args -> 
        try
            show args
        with 
        | ex -> 
            printfn "Err: %s" ex.Message
    0
