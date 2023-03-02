namespace MonoClient

open System
open System.Text

module DateTime =
    let start = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

    let toLocalTime (epoch: uint64) = start.AddSeconds(float epoch).ToLocalTime()

    let toEpoch (date: DateTime) = (date - start).TotalSeconds |> int64

    let local (year, month, day) = DateTime(year, month, day, 0, 0, 0, DateTimeKind.Local)

module Cmdline =

    type DateRange = { start: DateTime; length: int64 }
    type Request = { key: string; range: DateRange }

    let parse (line: string[]) : Request option =
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
    let deserialize (json: string) : 'a = JsonSerializer.Deserialize json
    let serialize (t: 'a) : string = JsonSerializer.Serialize t

module Main =
    type Statement = { id: string; date: DateTime; description: string; amount: int64; comment: string option }

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

    let mapStatement (st: Statement) : Main.Statement =
        { id = st.id
          date = DateTime.toLocalTime st.time
          description = st.description
          amount = st.amount
          comment = st.comment }

    module Api =
        type t =
            private
                { c: Net.Http.HttpClient }

            interface IDisposable with
                member this.Dispose() = this.c.Dispose()

        let xTokenHeader = "X-Token"
        let apiBase = "https://api.monobank.ua/"

        let create (key: string) : t =
            let client = new System.Net.Http.HttpClient()
            client.DefaultRequestHeaders.Add(xTokenHeader, [ key ])
            { c = client }

        let httpGet (api: t) (url: string) : Async<'a> =
            async {
                let! rsp = api.c.GetAsync url |> Async.AwaitTask
                let! json = rsp.Content.ReadAsStringAsync() |> Async.AwaitTask

                if rsp.IsSuccessStatusCode then
                    return Json.deserialize json
                else
                    let error: ApiError = Json.deserialize json
                    return failwith error.errorDescription
            }

        let clientInfo (api: t) : Async<ClientInfo> = $"{apiBase}/personal/client-info" |> httpGet api

        let statements (t: t) account fromTime toTime : Async<array<Statement>> =
            $"{apiBase}/personal/statement/{account}/{fromTime}/{toTime}" |> httpGet t

module Mapping =
    open System.Text.RegularExpressions
    type t = private { monoBankAccount: string; map: Map<string, string>; regexp: List<Regex * String> }

    let fromJson (json: string) : t =
        let json: {| MonoBankAccount: string; Mapping: Map<string, List<string>> |} = Json.deserialize json

        let mappings =
            json.Mapping
            |> Map.toSeq
            |> Seq.map (fun (liability, keys) -> keys |> Seq.map (fun key -> key, liability))
            |> Seq.fold Seq.append Seq.empty
            |> Seq.fold
                (fun (regex, direct) (k, v) ->
                    if k.StartsWith '/' && k.EndsWith '/' then
                        (Regex(k[1 .. k.Length - 2]), v) :: regex, direct
                    else
                        regex, Map.add k v direct)
                ([], Map.empty)

        { monoBankAccount = json.MonoBankAccount; map = snd mappings; regexp = fst mappings }

    let mapDescription (mapping: t) (description: string) : string =
        Map.tryFind description mapping.map
        |> Option.orElseWith (fun () -> mapping.regexp |> List.tryFind (fun (regex, _) -> regex.IsMatch description) |> Option.map snd)
        |> Option.defaultValue description

    let monobankAccount (t: t) : string = t.monoBankAccount

module Program =
    open Main

    let statementWriter (mapping: Mapping.t) (dt: DateTime) (statements: Statement seq) : string =
        // combine multiple statements with same description into single line
        let combine (description, statements) : int64 * 'a * string =
            let amount, comments =
                statements
                |> Seq.fold
                    (fun (amount, comments) st ->
                        let newComments =
                            match st.comment with
                            | Some c -> c :: comments
                            | None -> comments

                        (amount + st.amount, newComments))
                    (0L, [])

            let comment = Seq.rev comments |> String.concat " ; "
            amount, description, comment

        let format (amount, description, comments) =
            let amount = -float amount / 100.0

            if String.IsNullOrWhiteSpace comments then
                $"  {description, -40} {amount, 8:``#,#0.#0 UAH``}"
            else
                $"""  {description, -40} {amount, 8:``#,#0.#0 UAH``}{"; ", 15}{comments}"""

        statements
        |> Seq.groupBy (fun st -> st.description.Replace("\n", " .. ") |> Mapping.mapDescription mapping)
        |> Seq.map combine
        |> Seq.map format
        |> function
            | lines when Seq.isEmpty lines -> String.Empty
            | lines ->
                let account = Mapping.monobankAccount mapping
                let sb = String.concat "\n" lines
                $"{dt:``yyyy/MM/dd``} *\n  {account}\n{sb}"

    let show (param: Cmdline.Request) : unit =
        use api = Monobank.Api.create param.key

        let fromTime = param.range.start.ToUniversalTime() |> DateTime.toEpoch
        let tillTime = fromTime + param.range.length

        let mapping = "mapping.json" |> System.IO.File.ReadAllText |> Mapping.fromJson

        Monobank.Api.statements api 0 fromTime tillTime
        |> Async.RunSynchronously
        |> Array.map Monobank.mapStatement
        |> Array.groupBy (fun s -> s.date.Date)
        |> Array.map (fun (date, statements) -> statementWriter mapping date statements)
        |> String.concat "\n;\n"
        |> printfn "%O"

    [<EntryPoint>]
    let main argv =
        match Cmdline.parse argv with
        | None -> Cmdline.usage () |> printfn "%s"
        | Some args -> show args

        0
