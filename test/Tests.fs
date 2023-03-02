module Tests

open MonoClient
open Xunit
open FsUnit.Xunit

module Mapping =
    [<Fact>]
    let read_mappings () =
        let actual: Mapping.t =
            """
{
  "MonoBankAccount": "account label",
  "Mapping": {
    "cat": ["yellow", "green"],
    "bunny": ["/funny/"]
  }
}
"""
            |> Mapping.fromJson

        let expected = """{ monoBankAccount = "account label"
  map = map [("green", "cat"); ("yellow", "cat")]
  regexp = [(funny, "bunny")] }"""

        string actual |> should equal expected

module CmdlineTest =
    [<Fact>]
    let parse_cmdline () =
        let actual = Cmdline.parse [| "key"; "2023"; "2" |]

        let expected: option<Cmdline.Request> =
            Some { key = "key"; range = { start = System.DateTime(2023, 2, 1); length = 28L * 24L * 3600L - 1L } }

        actual |> should equal expected

module MainTest =
    // open Mapping
    open Main
    open Program

    let dateTime = System.DateTime(0L)

    [<Fact>]
    let statement_writer () =
        let statement = { id = "1"; date = System.DateTime(0); description = "green"; comment = None; amount = 0 }
        let statements =
            seq {
                { statement with amount = -123L; comment = Some "everything" }
                { statement with amount = -456L; comment = None }
                { statement with amount = -789L; comment = Some "anything" }
            }
        let mapping = """{ "MonoBankAccount": "label", "Mapping": { "yellow!": ["green"] } }""" |> Mapping.fromJson
        let actual = statementWriter mapping dateTime statements

        let expected =
            "\
0001/01/01 *
  label
  yellow!                                  13.68 UAH             ; everything ; anything"
        // Assert.Equal (expected, actual)
        actual |> should equal expected

    [<Fact>]
    let statement_writer_no_comment () =
        let statement = { id = "1"; date = System.DateTime(0); description = "green"; comment = None; amount = 0 }
        let mapping = """{ "MonoBankAccount": "label", "Mapping": { "yellow!": ["green"] } }""" |> Mapping.fromJson
        
        let actual = statementWriter mapping dateTime [ statement ]

        let expected =
            "\
0001/01/01 *
  label
  yellow!                                  -0.00 UAH"

        actual |> should equal expected


    [<Fact>]
    let statement_writer_empty () =
        let mapping = """{ "MonoBankAccount": "label", "Mapping": {}}""" |> Mapping.fromJson
        statementWriter mapping dateTime Seq.empty |> should be Empty

    [<Fact>]
    let statement_writer_regexp () =
        let mapping = """{ "MonoBankAccount": "a", "Mapping": { "animal": ["cat"], "color": [ "/red|white/" ] } }""" |> Mapping.fromJson
        let statement = { id = "1"; date = System.DateTime(0); description = "green"; comment = None; amount = 0 }
        let statements =
            seq {
                { statement with amount = -123L; description = "red" }
                { statement with amount = -321L; description = "white"; comment = Some "grey" }
                { statement with amount = -111L; description = "blue" }
                { statement with amount = -222L; description = "cat" }
            }

        let actual = statementWriter mapping dateTime statements

        let expected =
            "\
0001/01/01 *
  a
  color                                    4.44 UAH             ; grey
  blue                                     1.11 UAH
  animal                                   2.22 UAH"

        actual |> should equal expected
