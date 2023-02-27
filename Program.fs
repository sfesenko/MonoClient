namespace MonoClient

open System
open System.Text

module DateTime =
    let start = DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc)

    let toLocalTime (epoch: uint64) = (start.AddSeconds(float epoch)).ToLocalTime()

    let toEpoch (date: DateTime) = (date - start).TotalSeconds |> int64

    let local (year, month, day) = DateTime(year, month, day, 0, 0, 0, DateTimeKind.Local)

module Cmdline =

    type DateRange = { start: DateTime; length: int64 }
    type Request = { key: string; range: DateRange }

    let parse (line: string[]) =
        if Array.length line <> 3 then
            None
        else
            let year = int line.[1]
            let month = int line.[2]
            let start = DateTime.local (year, month, 1)
            let length = int64 (start.AddMonths 1 - start).TotalSeconds - 1L

            { key = line.[0]; range = { start = start; length = length } } |> Some

    let usage () =
        """Usage:
    CLIENT <api key> <year> <month>"""

module Json =
    open System.Text.Json
    open System.Text.Json.Serialization

    let jso = JsonSerializerOptions()
    jso.Converters.Add(JsonFSharpConverter())

    let deserialize (json: string) = JsonSerializer.Deserialize(json, jso)

    let serialize t = JsonSerializer.Serialize(t, jso)

module Monobank =

    type Account =
        { id: string
          currencyCode: uint32
          cashbackType: string
          balance: uint32
          creditLimit: uint32
          maskedPan: array<string>
          ``type``: string
          iban: string }

    type ClientInfo = { clientId: string; name: string; webHookUrl: string; permissions: string; accounts: list<Account> }

    type Statement =
        { id: string
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
          counterIban: option<string> }

    type ApiError = { errorDescription: string }

    module Api =
        type t =
            private
                { c: Net.Http.HttpClient }

            interface IDisposable with
                member this.Dispose() = this.c.Dispose()

        let xTokenHeader = "X-Token"
        let apiBase = "https://api.monobank.ua/"
        let apiClientInfo = apiBase + "/personal/client-info"

        let create key =
            let client = new System.Net.Http.HttpClient()
            client.DefaultRequestHeaders.Add(xTokenHeader, [ key ])
            { c = client }

        let httpGet (t: t) (url: string) =
            async {
                let! rsp = t.c.GetAsync url |> Async.AwaitTask
                let! json = rsp.Content.ReadAsStringAsync() |> Async.AwaitTask

                if rsp.IsSuccessStatusCode then
                    return Json.deserialize json
                else
                    let error: ApiError = Json.deserialize json
                    return failwith error.errorDescription
            }

        let clientInfo (t: t) : Async<ClientInfo> = httpGet t apiClientInfo

        let statements (t: t) account fromTime toTime : Async<array<Statement>> =
            apiBase + $"/personal/statement/{account}/{fromTime}/{toTime}" |> httpGet t

module Mapping =
    open System.Text.RegularExpressions
    type t = { monoBankAccount: string; map: Map<string, string>; regexp: List<Regex * String> }

    let readMapping (fileName: string) =
        let json: {| MonoBankAccount: string; Mapping: Map<string, List<string>> |} =
            System.IO.File.ReadAllText fileName |> Json.deserialize

        let mappings =
            json.Mapping
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> v |> Seq.map (fun l -> l, k))
            |> Seq.reduce Seq.append
            |> Seq.fold
                (fun (regexs, map) (k, v) ->
                    if k.StartsWith '/' && k.EndsWith '/' then (Regex(k[1 .. k.Length - 2]), v) :: regexs, map else regexs, Map.add k v map)
                ([], Map.empty)

        { monoBankAccount = json.MonoBankAccount; map = snd mappings; regexp = fst mappings }

module Main =
    type Statement = { id: string; date: DateTime; description: string; amount: int64; comment: string option }

    module MonobankConverter =

        let mapStatement (st: Monobank.Statement) =
            { id = st.id
              date = DateTime.toLocalTime st.time
              description = st.description
              amount = st.amount
              comment = st.comment }

    let tr (map: Mapping.t) (src: string) : string =
        let findByRegex (key: string) : String option = map.regexp |> List.tryFind (fun (regex, _) -> regex.IsMatch key) |> Option.map snd
        Map.tryFind src map.map |> Option.orElseWith (fun () -> findByRegex src) |> Option.defaultValue src

    let statementWriter (map: Mapping.t) (dt: DateTime) (statements: Statement seq) =
        let tr (src: string) : string = tr map (src.Replace("\n", " .. "))

        let sb = Text.StringBuilder()
        let source = map.monoBankAccount
        sb.Append $"{dt:``yyyy/MM/dd``} *\n  {source}\n" |> ignore

        // combine multiple statements
        let combine (description, statements) : int64 * 'a * StringBuilder =
            let (amount, comment) =
                statements
                |> Seq.fold
                    (fun (amount, comments: StringBuilder) st ->
                        Option.iter
                            (fun c ->
                                if not <| String.IsNullOrWhiteSpace c then
                                    comments.Append $"; {c}" |> ignore)
                            st.comment

                        (amount + st.amount, comments))
                    (0L, StringBuilder())

            amount, description, comment

        statements
        |> Seq.groupBy (fun st -> tr st.description)
        |> Seq.map combine
        |> Seq.map (fun (amount, description, comment) ->
            let amount = -(float) amount / 100.0

            if comment.Length > 0 then
                comment.Insert(0, " ; ") |> ignore

            $"  {description, -40} {amount, -30:``#,#.#0 UAH``}{comment}".TrimEnd())
        |> String.concat "\n"
        |> sb.Append
        |> string

    let show (param: Cmdline.Request) =
        use h = Monobank.Api.create param.key


        let fromTime = param.range.start.ToUniversalTime() |> DateTime.toEpoch
        let tillTime = fromTime + param.range.length

        let trMap = Mapping.readMapping "mapping.json"
        // Map.empty |> Map.add "АТБ" "Expenses:Food:Super:ATB"

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
        | None -> Cmdline.usage () |> printfn "%s"
        | Some args ->
            try
                show args
            with ex ->
                printfn $"Err: %s{ex.Message}"

        0
