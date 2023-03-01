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

module MainTest =
    open Mapping
    open Main

    let statement = { id = "1"; date = System.DateTime(0); description = "yellow!"; comment = None; amount = 0 }

    let mapping = { monoBankAccount = "label"; map = Map.ofList [ ("yellow!", "green") ]; regexp = [] }
    let dateTime = System.DateTime(0L)

    [<Fact>]
    let statement_writer () =
        let statements =
            seq {
                { statement with amount = -123L; comment = Some "everything" }
                { statement with amount = -456L; comment = None }
                { statement with amount = -789L; comment = Some "anything" }
            }

        let actual = statementWriter mapping dateTime statements

        let expected =
            "\
0001/01/01 *
  label
  green                                    13.68 UAH                     ; everything ; anything"

        actual |> should equal expected

    [<Fact>]
    let statement_writer_no_comment () =
        let actual = statementWriter mapping dateTime [ statement ]

        let expected =
            "\
0001/01/01 *
  label
  green                                    -.00 UAH"

        actual |> should equal expected


    [<Fact>]
    let statement_writer_empty () = statementWriter mapping dateTime Seq.empty |> should be Empty

    [<Fact>]
    let statement_writer_regexp () =
        let mapping = { monoBankAccount = "a"; map = Map.ofList["cat", "animal"]; regexp = [ (Regex("red|white"), "color") ] }

        let statements =
            seq {
                { statement with amount = -123L; description = "red" }
                { statement with amount = -321L; description = "white" }
                { statement with amount = -111L; description = "blue" }
                { statement with amount = -222L; description = "cat" }
            }

        let actual = statementWriter mapping dateTime statements

        let expected =
            "\
0001/01/01 *
  a
  color                                    4.44 UAH
  blue                                     1.11 UAH
  animal                                   2.22 UAH"

        actual |> should equal expected
