open System
open Expecto
open Tests
open System.IO

[<EntryPoint>]
let main argv =
    let crtfc_key = File.ReadAllText("../.env")
    let test = tests crtfc_key
    runTestsWithCLIArgs [] argv test