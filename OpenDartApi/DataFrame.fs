module OpenDartApi.DataFrame

open System.Xml
open System.Xml.Linq
open System.Threading.Tasks
open Deedle

open ResponseTypes
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


    // TODO:
    // it only shows major account such as current asset, non current asset, ...
    // need to parse https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019020
    member _.SingleCompanyMajorAccount(name: string, year: string) =
        let lookupResult, (code, stockCode, modifyDate) = corpCodeTable.TryGetValue name

        let cols =
            [ "개별/연결구분"
              "재무제표구분"
              "계정명"
              "당기명"
              "당기일자"
              "당기금액"
              "전기명"
              "전기일자"
              "전기금액"
              "전전기명"
              "전전기일자"
              "전전기금액" ]

        if lookupResult then
            async {
                let! data = api.``단일회사 주요계정`` (code, year)

                match data with
                | Result.Ok accnt ->
                    match accnt.List with
                    | Some rows ->
                        let frame = Frame.ofRecords rows |> Frame.sliceCols cols

                        let cfs = Frame.filterRowsBy "개별/연결구분" (Some ConSep.Consolidated) frame
                        let ofs = Frame.filterRowsBy "개별/연결구분" (Some ConSep.Separate) frame

                        let cfsBs = Frame.filterRowsBy "재무제표구분" FinStat.BS cfs
                        let ofsBs = Frame.filterRowsBy "재무제표구분" FinStat.BS ofs
                        let cfsIs = Frame.filterRowsBy "재무제표구분" FinStat.IS cfs
                        let ofsIs = Frame.filterRowsBy "재무제표구분" FinStat.IS ofs

                        let pivotThisTerm =
                            Frame.pivotTable
                                (fun k r -> r.GetAs<AccountDate option>("당기일자"))
                                (fun k r -> r.GetAs<AccountName>("계정명"))
                                (fun frame ->
                                    frame.GetColumn<decimal option>("당기금액")
                                    |> Series.mapValues (fun d -> d.Value)
                                    |> Series.firstValue)

                        let pivotFormerTerm =
                            Frame.pivotTable
                                (fun k r -> r.GetAs<AccountDate option>("전기일자"))
                                (fun k r -> r.GetAs<AccountName>("계정명"))
                                (fun frame ->
                                    frame.GetColumn<decimal option>("전기금액")
                                    |> Series.mapValues (fun d -> d.Value)
                                    |> Series.firstValue)

                        let pivotBeforeFormerTerm =
                            Frame.pivotTable
                                (fun k r -> r.GetAs<AccountDate option>("전전기일자"))
                                (fun k r -> r.GetAs<AccountName>("계정명"))
                                (fun frame ->
                                    frame.GetColumn<decimal option>("전전기금액")
                                    |> Series.mapValues (fun d -> d.Value)
                                    |> Series.firstValue)

                        let pivotTerms table =
                            Frame.mergeAll [ pivotThisTerm table; pivotFormerTerm table; pivotBeforeFormerTerm table ]

                        return
                            {| CompanyName = name
                               StockCode = code
                               Orig = frame
                               Consolidate =
                                {| BS = pivotTerms cfsBs
                                   IS = pivotTerms cfsIs |}
                               Separate =
                                {| BS = pivotTerms ofsBs
                                   IS = pivotTerms ofsIs |} |}
                    | None -> return failwith "The response does not contain data"
                | Result.Error err -> return failwithf "The response is malformed: %s" err
            }
        else
            failwithf "There is no such company: %s" name
