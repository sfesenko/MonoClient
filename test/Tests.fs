module Tests

open System.Text.RegularExpressions
open MonoClient
open Xunit
open FsUnit.Xunit

module MappingTest =
    [<Fact>]
    let read_mappings () =
        let actual =
            """
{
  "MonoBankAccount": "account label",
  "Mapping": {
    "cat": ["yellow", "green"],
    "bunny": ["/funny/"]
  }
}
"""
            |> Mapping.readMapping

        let expected: Mapping.t =
            { monoBankAccount = "account label"
              map = Map.ofList [ ("yellow", "cat"); ("green", "cat") ]
              regexp = [ Regex("funny"), "bunny" ] }

        // Regex is  not comparable, so use string
        string actual |> should equal <| string expected

module CmdlineTest =
    [<Fact>]
    let parse_cmdline () =
        let actual = Cmdline.parse [| "key"; "2023"; "2" |]

        let expected: option<Cmdline.Request> =
            Some { key = "key"; range = { start = System.DateTime(2023, 2, 1); length = 28L * 24L * 3600L - 1L } }

        actual |> should equal expected
