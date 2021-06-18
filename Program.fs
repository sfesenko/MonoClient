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

module Cmdline =
    
    type DateRange = { start: DateTime ; length: int64 }
    type Request = {
        key: string
        range: DateRange
    }

    let parse (line: string[]) = 
        if Array.length line <> 3 then
            None
        else
            let year = int line.[1]
            let month = int line.[2] 
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

module Monobank =

    type Account = {
        id: string
        currencyCode: uint32
        cashbackType: string
        balance: uint32
        creditLimit: uint32
        maskedPan: array<string>
        ``type``: string
        iban: string
    }

    type ClientInfo = { 
        clientId: string
        name: string
        webHookUrl: string
        permissions: string
        accounts: list<Account>
    }

    type Statement = {
        id: string
        time: uint64
        description: string
        mcc: uint32
        hold: bool
        amount: int64
        operationAmount: int64
        currencyCode: uint32
        commissionRate: int64
        cashbackAmount: int64
        balance: int64
        comment: option<string>
        receiptId: option<string>
        counterEdrpou: option<string>
        counterIban: option<string>
    }

    type ApiError = { errorDescription: string }

    open System.Text.Json
    open System.Text.Json.Serialization

    module Api =
        type t = private { c : Net.Http.HttpClient }
            with interface IDisposable with member this.Dispose() = this.c.Dispose ()

        let jso = JsonSerializerOptions()
        let xTokenHeader = "X-Token"
        let apiBase = "https://api.monobank.ua/"
        let apiClientInfo = apiBase + "/personal/client-info"

        do
            jso.Converters.Add (JsonFSharpConverter())

        let create key = 
            let client = new System.Net.Http.HttpClient ()
            client.DefaultRequestHeaders.Add (xTokenHeader, [key])
            { c = client }

        let deserialize (json: string) = JsonSerializer.Deserialize (json, jso)

        let httpGet (t: t) (url: string) = async {
            let! rsp = t.c.GetAsync url |> Async.AwaitTask
            let! json = rsp.Content.ReadAsStringAsync () |> Async.AwaitTask
            if rsp.IsSuccessStatusCode then
                return deserialize json
            else
                let error: ApiError = deserialize json
                return failwith error.errorDescription
        }

        let clientInfo (t: t) : Async<ClientInfo> = httpGet t apiClientInfo

        let statements (t: t) account fromTime toTime: Async<array<Statement>> =
            apiBase + $"/personal/statement/{account}/{fromTime}/{toTime}" 
            |> httpGet t


module Main =
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
