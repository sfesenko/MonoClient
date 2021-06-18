open System

let s = DateTime.Now
DateTime.UnixEpoch

let diff = s - s.ToUniversalTime ()


let start = DateTime(1970,1,1,0,0,0,0,System.DateTimeKind.Utc)
let ts2 = DateTime.Now - start

start.AddYears -1970




[1;3;5]
|> Seq.map string
|> String.concat "\n"


let b = 12.3
$"%0.2f{b}"

let dt = System.DateTime.Now
$"{dt:D}"
let am = 1234.11
$"I think {dt,-20:``yyyy/MM/dd``} {am,-30:``#,#.#0``} == "

let ss = ""
$".{ss,4}"

System.DateTime.Now.ToString("yyyy/MM/dd")

let plus x y = x + y

(1,2) ||> plus

let ab = fun s -> ss


let pp2 a b = $"[{a} ++ {b}]"

Map.empty