module OpenDartApi.DartApi

open System
open System.IO
open Thoth.Json.Net
open FsHttp

open ResponseTypes

type ReportCode =
    | Qrt1
    | Qrt2
    | Qrt3
    | Annual

    member this.ToCode =
        match this with
        | Qrt1 -> "11013"
        | Qrt2 -> "11012"
        | Qrt3 -> "11014"
        | Annual -> "11011"

type FinStmt =
    | CFS // 연결재무제표
    | OFS // 재무제표

    member this.ToCode =
        match this with
        | CFS -> "CFS"
        | OFS -> "OFS"

type CorpClass =
    | KOSPI
    | KOSDAQ
    | KONEX
    | Other

    member this.ToCode =
        match this with
        | KOSPI -> "Y"
        | KOSDAQ -> "K"
        | KONEX -> "N"
        | Other -> "E"

type DartListSortBy =
    | Date
    | Corp
    | Report

    member this.ToCode =
        match this with
        | Date -> "date"
        | Corp -> "crp"
        | Report -> "rpt"


type DartPublicNotification =
    | 정기공시
    | 주요사항보고
    | 발행공시
    | 지분공시
    | 기타공시
    | 외부감사관련
    | 펀드공시
    | 자산유동화
    | 거래소공시
    | 공정위공시

    member this.ToCode =
        match this with
        | 정기공시 -> "A"
        | 주요사항보고 -> "B"
        | 발행공시 -> "C"
        | 지분공시 -> "D"
        | 기타공시 -> "E"
        | 외부감사관련 -> "F"
        | 펀드공시 -> "G"
        | 자산유동화 -> "H"
        | 거래소공시 -> "I"
        | 공정위공시 -> "J"


[<Literal>]
let HOST = "opendart.fss.or.kr"


[<Literal>]
let API_ENDPOINT = "https://opendart.fss.or.kr/api"

type DartApi(crtfc_key: string) =
    do
        if crtfc_key.Length <> 40 then
            invalidArg "crtfc_key" "crtfc_key should be of length 40"

    member _.RequestAsync(path: string, ?queryParams) =
        let url = $"{API_ENDPOINT}/{path}"

        let queryParams = [ "crtfc_key", box crtfc_key; yield! defaultArg queryParams [] ]

        http {
            GET url
            query queryParams
            Host "opendart.fss.or.kr"
            Accept "*/*"
            UserAgent "open-dart-api-fsharp/0.0.1"
        }
        |> Request.toAsync


    /// Get response of cord codes metadata
    /// written in xml format.
    /// The response binary will be zip archive
    member this.CorpCodeZipBytes() =
        async {
            let! resp = this.RequestAsync("corpCode.xml")
            let! bytes = Response.toBytesAsync resp
            return bytes
        }

    member this.DownloadCordCode(path: string) =
        task {
            let! bytes = this.CorpCodeZipBytes()
            do! File.WriteAllBytesAsync(path, bytes)
            do Compression.ZipFile.ExtractToDirectory(path, ".")
        }


    /// 공시정보 목록 - 1. 공시정보 검색
    member this.공시정보
        (
            ?corpCode: string,
            ?startDate: DateOnly,
            ?endDate: DateOnly,
            ?onlyLastReport: bool,
            ?publicNotification: DartPublicNotification,
            ?pblntf_detail_ty,
            ?corpClass: CorpClass,
            ?sort: DartListSortBy,
            ?orderAsc: bool,
            ?pageNumber,
            ?countPerPage
        ) =

        let queryParams =
            [ "corp_code", corpCode
              "bgn_de", startDate |> Option.map (fun date -> date.ToString("yyyyMMdd"))
              "end_de", endDate |> Option.map (fun date -> date.ToString("yyyyMMdd"))
              "last_reprt_at", onlyLastReport |> Option.map (fun flag -> if flag then "Y" else "N")
              "pblntf_ty", publicNotification |> Option.map (fun typ -> typ.ToCode)
              "pblntf_detail_ty", pblntf_detail_ty
              "corp_cls", corpClass |> Option.map (fun c -> c.ToCode)
              "sort", sort |> Option.map (fun sortBy -> sortBy.ToCode)
              "sort_mth", orderAsc |> Option.map (fun asc -> if asc then "asc" else "desc")
              "page_no", pageNumber
              "page_count", countPerPage ]
            |> List.fold
                (fun acc p ->
                    match p with
                    | name, Some x -> (name, box x) :: acc
                    | _ -> acc)
                []

        async {
            let! resp = this.RequestAsync("list.json", queryParams)
            let! body = Response.toStringAsync None resp
            return Decode.fromString ``공시정보 Response``.Decoder body
        }

    member private this.GetAndDecode(path: string, queryParams, decoder) =
        async {
            let! resp = this.RequestAsync(path, queryParams)
            let! text = Response.toStringAsync None resp
            return Decode.fromString decoder text
        }

    /// 공시정보 목록 - 2. 기업개황
    member this.기업개황(corpCode: string) =
        this.GetAndDecode("company.json", [ "corp_code", corpCode ], ``기업개황 Response``.Decoder)

    // 사업보고서 주요정보

    /// 증자(감자) 현황
    member this.증자감자현황(corpCode: string, businessYear, reprt_code: ReportCode) =
        this.GetAndDecode(
            "irdsSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reprt_code.ToCode ],
            ``증자감자현황 Response``.Decoder
        )


    /// 배당에 관한 사항
    member this.배당관련사항(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "alotMatter.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``배당관련사항 Response``.Decoder
        )


    /// 자기주식 취득 및 처분 현황
    member this.``자기주식 취득 및 처분 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "tesstkAcqsDspsSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``자기주식 취득 및 처분 현황 Response``.Decoder
        )


    /// 최대주주 현황
    member this.``최대주주 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "hyslrSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``최대주주 현황 Response``.Decoder
        )

    /// 최대주주 변동 현황
    member this.``최대주주 변동 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "hyslrChgSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``최대주주 변동 현황 Response``.Decoder
        )

    /// 소액주주 현황
    member this.``소액주주 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "mrhlSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``소액주주 현황 Response``.Decoder
        )

    member this.``임원 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "exctvSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``임원현황 Response``.Decoder
        )

    member this.``직원 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "empSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``직원현황 Response``.Decoder
        )

    /// 2.9 이사, 감사의 개인별 보수 현황
    member this.``이사감사 개인별 보수 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "hmvAuditIndvdlBySttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``이사감사 개인별 보수 현황 Response``.Decoder
        )

    /// 2.10 - 이사, 감사 전체의 보수 현황
    member this.``이사감사 전체 보수 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "hmvAuditAllSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``이사감사 전체의 보수 현황 Response``.Decoder
        )

    /// 2.11 - 개인별 보수지급 금액(5억이상 상위5인)
    member this.``개인별 보수지급 금액``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "indvdlByPay.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``이사감사 개인별 보수 현황 Response``.Decoder
        )

    /// 2.12 - 타법인 출자현황
    member this.``타법인 출자 현황``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "otrCprInvstmntSttus.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``타법인 출자현황 Response``.Decoder
        )

    // 상장기업 재무정보

    /// 단일회사 주요계정
    /// Reference: https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019016
    member this.``단일회사 주요계정``(corpCode: string, businessYear, reportCode: ReportCode) =
        this.GetAndDecode(
            "fnlttSinglAcnt.json",
            [ "corp_code", corpCode
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``상장기업 주요계정 Response``.Decoder
        )


    /// 다중회사 주요계정
    /// https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019017
    member this.``다중회사 주요계정``(corpCodes, businessYear, reportCode: ReportCode) =
        let corpCodes = corpCodes |> List.reduce (fun x y -> x + "," + y)

        this.GetAndDecode(
            "fnlttSinglAcnt.json",
            [ "corp_code", corpCodes
              "bsns_year", businessYear
              "reprt_code", reportCode.ToCode ],
            ``상장기업 주요계정 Response``.Decoder
        )

    /// 단일회사 전체 재무제표
    /// https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019020
    member this.``단일회사 전체 재무제표``(corpCode: string, bsnsYear, reprtCode: ReportCode, fsDiv: FinStmt) =
        this.GetAndDecode(
            "fnlttSinglAcntAll.json",
            [ "corp_code", corpCode
              "bsns_year", bsnsYear
              "reprt_code", reprtCode.ToCode
              "fs_div", fsDiv.ToCode ],
            ``상장기업 주요계정 Response``.Decoder
        )

    /// 재무제표 원본파일 (Zip file)
    /// https://opendart.fss.or.kr/guide/detail.do?apiGrpCd=DS003&apiId=2019019
    member this.``재무제표 원본파일``(rceptNo, reprtCode: ReportCode) =
        async {
            let! resp = this.RequestAsync("fnlttXbrl.xml", [ "rcept_no", rceptNo; "reprt_code", reprtCode.ToCode ])
            return! Response.toBytesAsync resp
        }
