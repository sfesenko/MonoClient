module MonoClient.Monobank

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

open System
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

    let statements (t: t) fromTime toTime: Async<array<Statement>> =
        apiBase + $"/personal/statement/0/{fromTime}/{toTime}" 
        |> httpGet t
