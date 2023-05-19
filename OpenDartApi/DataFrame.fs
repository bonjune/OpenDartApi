module OpenDartApi.DataFrame

open System.Xml
open System.Xml.Linq
open System.Threading.Tasks
open Deedle

open ApiCaller

type DartDataFrame(certificateKey: string) =
    let api = ApiCaller.DartApi(certificateKey)

    do
        api.DownloadCordCode("./CORPCODE.zip", CorpCodeDownloadOptions.IfNotExist)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let corpCodeTable =
        let xdoc = XDocument.Load(DOWNLOAD_DIR + "/CORPCODE.xml")
        let result = xdoc.Element("result")

        readOnlyDict
        <| seq {
            for row in result.Elements("list") do
                let name = row.Element("corp_name").Value
                let code = row.Element("corp_code").Value
                let stockCode = row.Element("stock_code").Value
                let modifyDate = row.Element("modify_date").Value
                (name, (code, stockCode, modifyDate))
        }

    do printfn "%A" corpCodeTable.Keys

    member _.SingleCompanyMainAccount(name: string, year: string) =
        let lookupResult, (code, stockCode, modifyDate) = corpCodeTable.TryGetValue name

        if lookupResult then
            async {
                let! data = api.``단일회사 주요계정`` (code, year)

                match data with
                | Result.Ok accnt ->
                    match accnt.List with
                    | Some rows -> return Frame.ofRecords rows
                    | None -> return failwith "The response does not contain data"
                | Result.Error err -> return failwithf "The response is malformed: %s" err
            }
        else
            failwithf "There is no such company %s" name
