module OpenDartApi.Observations

open System.Xml
open System.Xml.Linq
open System.Threading.Tasks
open System.Diagnostics
open System.Collections.Generic

open ResponseTypes
open ApiCaller

type DartObservations(certificateKey: string) =
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

                (name,
                 {| CorpCode = code
                    StockCode = stockCode
                    ModifiedDate = modifyDate |})
        }

    do printfn "%A" corpCodeTable.Keys


    // TODO:
    // it only shows major account such as current asset, non current asset, ...
    // need to parse https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019020
    member _.SingleCompanyMajorAccount(name: string, year: string) =
        let lookupResult, value = corpCodeTable.TryGetValue name

        if lookupResult then
            async {
                let sw = Stopwatch()
                sw.Start()
                let! data = api.``단일회사 주요계정`` (value.CorpCode, year)
                sw.Stop()

                printfn "%A ms elapsed for a api call" sw.ElapsedMilliseconds

                match data with
                | Result.Ok accnt ->
                    match accnt.List with
                    | Some rows ->
                        sw.Restart()

                        let cfs, ofs =
                            Array.partition (fun row -> row.``개별/연결구분`` = Some(ConSep.Consolidated)) rows

                        let cfsBs, cfsIs = cfs |> Array.partition (fun row -> row.재무제표구분 = FinStat.BS)
                        let ofsBs, ofsIs = ofs |> Array.partition (fun row -> row.재무제표구분 = FinStat.BS)

                        let collectAccountRows (rows: 계정 array) =
                            seq {
                                for row in rows do
                                    yield
                                        row.계정명,
                                        [ if row.당기일자.IsSome then
                                              yield (row.당기일자.Value, row.당기금액.Value)

                                          if row.전기일자.IsSome then
                                              yield (row.전기일자.Value, row.전기금액.Value)

                                          if row.전전기일자.IsSome then
                                              yield (row.전전기일자.Value, row.전전기금액.Value) ]

                            }
                            |> dict

                        let cfsBsByAccounts = collectAccountRows cfsBs
                        let cfsIsByAccounts = collectAccountRows cfsIs
                        let ofsBsByAccounts = collectAccountRows ofsBs
                        let ofsIsByAccounts = collectAccountRows ofsIs
                        sw.Stop()

                        printfn "%A ms elapsed for collecting data" sw.ElapsedMilliseconds

                        return
                            {| CompanyName = name
                               StockCode = value.StockCode
                               Consolidate =
                                {| BS = cfsBsByAccounts
                                   IS = cfsIsByAccounts |}
                               Separate =
                                {| BS = ofsBsByAccounts
                                   IS = ofsIsByAccounts |} |}
                    | None -> return failwith "The response does not contain data"
                | Result.Error err -> return failwithf "The response is malformed: %s" err
            }
        else
            failwithf "There is no such company: %s" name
