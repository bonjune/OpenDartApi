module OpenDartApi.DartApi

open FSharp.Data
open FSharp.Data.HttpRequestHeaders
open Thoth.Json.Net
open ResponseTypes

type ReportCode =
    | Qrt1 = 11013
    | Qrt2 = 11012
    | Qrt3 = 11014
    | Annual = 11011

type DartApi(crtfc_key : string) =
    let defaultHeaders = [
        Host "opendart.fss.or.kr"
        Accept "*/*"
        AcceptEncoding "gzip, deflate"
        UserAgent "fsharp-data"
    ]

    member _.Request(url, ?queryParams) =
        Http.Request(
            url,
            query = [
                "crtfc_key", crtfc_key
                yield! queryParams |> Option.defaultValue []
            ],
            headers = defaultHeaders,
            responseEncodingOverride = "UTF-8")

    member this.CorpCodeZipBytes() =
        let url = "https://opendart.fss.or.kr/api/corpCode.xml"
        let res = this.Request(url)
        match res.Body with
        | Binary bytes -> bytes
        | _ -> failwith "No text response is expected"
    
    /// 공시정보 목록 - 1. 공시정보 검색
    member this.Disclosure
        (?corp_code, ?bgn_de, ?end_de, ?last_reprt_at, ?pblntf_ty, ?pblntf_detail_ty, ?corp_cls, ?sort, ?sort_mth, ?page_no, ?page_count) =

        let url = "https://opendart.fss.or.kr/api/list.json"
        let ps = [
            nameof corp_code, corp_code
            nameof bgn_de, bgn_de
            nameof end_de, end_de
            nameof last_reprt_at, last_reprt_at
            nameof pblntf_ty, pblntf_ty
            nameof pblntf_detail_ty, pblntf_detail_ty
            nameof corp_cls, corp_cls
            nameof sort, sort
            nameof sort_mth, sort_mth
            nameof page_no, page_no
            nameof page_count, page_count
        ]
        let ps =
            ps
            |> List.fold
                (fun acc p ->
                    match p with
                    | name, Some x -> (name, x) :: acc
                    | _ -> acc) []
        let res = this.Request(url, ps)
        match res.Body with
        | Text t -> t |> Decode.fromString ``공시정보 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``공시정보`` = this.Disclosure
    
    /// 공시정보 목록 - 2. 기업개황
    member this.Overview(corp_code) =
        let url = "https://opendart.fss.or.kr/api/company.json"
        let res = this.Request(url, [
            nameof corp_code, corp_code
        ])

        match res.Body with
        | Text t ->
            t
            |> Decode.fromString ``기업개황 Response``.Decoder
        | _ -> failwith "no binary response expected"
    
    member this.``기업개황`` = this.Overview
    
    // 사업보고서 주요정보
    
    /// 증자(감자) 현황
    member this.CapitalIncDec(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/irdsSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t ->
            t
            |> Decode.fromString ``증자감자현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``증자감자현황`` = this.CapitalIncDec
    
    /// 배당에 관한 사항
    member this.DividendMatter(corp_code,  bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/alotMatter.json"
        let res =
            this.Request(url, [
                nameof  corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t ->
            t
            |> Decode.fromString ``배당관련사항 Response``.Decoder
        | _ -> failwith "no binary response expected"
    
    member this.``배당관련사항`` = this.DividendMatter
    
    /// 자기주식 취득 및 처분 현황
    member this.StockAcqDsps(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/tesstkAcqsDspsSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        match res.Body with
        | Text t ->
            t
            |> Decode.fromString ``자기주식 취득 및 처분 현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``자기주식 취득 및 처분 현황`` = this.StockAcqDsps
    
    /// 최대주주 현황
    member this.MainShareholder(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/hyslrSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code,corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t ->
            t |> Decode.fromString ``최대주주 현황 Response``.Decoder
        | _ -> failwith "no binary response expected"
    
    member this.``최대주주 현황`` = this.MainShareholder
    
    /// 최대주주 변동 현황
    member this.MainShareholderChange(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/hyslrChgSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code,corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t ->
            t |> Decode.fromString ``최대주주 변동 현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``최대주주 변동 현황`` = this.MainShareholderChange
    
    /// 소액주주 현황
    member this.RetailShareholder(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/mrhlSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t ->
            t |> Decode.fromString ``소액주주 현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member  this.``소액주주 현황`` = this.RetailShareholder
    
    /// 임원현황
    member this.Executives(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/exctvSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t ->
            t |> Decode.fromString ``임원현황 Response``.Decoder
        | _ -> failwith "no binary response expected"
        
    member this.``임원 현황`` = this.Executives
        
    /// 직원현황
    member this.Employees(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/empSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t -> t |> Decode.fromString ``직원현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``직원 현황`` = this.Employees
    
    /// 2.9 이사, 감사의 개인별 보수 현황
    member this.DirectorAuditorIndivCompensation(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/hmvAuditIndvdlBySttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t -> t |> Decode.fromString ``이사감사 개인별 보수 현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``이사감사 개인별 보수 현황`` = this.DirectorAuditorIndivCompensation
    
    /// 2.10 - 이사, 감사 전체의 보수 현황
    member  this.DirectorAuditorTotalCompensation(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/hmvAuditAllSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t -> t |> Decode.fromString ``이사감사 전체의 보수 현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``이사감사 전체 보수 현황`` = this.DirectorAuditorTotalCompensation
    
    /// 2.11 - 개인별 보수지급 금액(5억이상 상위5인)   
    member this.IndividualCompensation(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/indvdlByPay.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t -> t |> Decode.fromString ``이사감사 개인별 보수 현황 Response``.Decoder
        | _ -> failwith "no binary response expected"

    member this.``개인별 보수지급 금액`` = this.IndividualCompensation
    
    /// 2.12 - 타법인 출자현황
    member this.OuterInvestmentStatus(corp_code, bsns_year, reprt_code : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/otrCprInvstmntSttus.json"
        let res =
            this.Request(url, [
                nameof corp_code, corp_code
                nameof bsns_year, bsns_year
                nameof reprt_code, reprt_code.ToString()
            ])
        
        match res.Body with
        | Text t -> t |> Decode.fromString ``타법인 출자현황 Response``.Decoder
        | _ -> failwith "no binary response expected"
    
    member this.``타법인 출자 현황`` = this.OuterInvestmentStatus
    
    // 상장기업 재무정보
    
    /// 단일회사 주요계정
    member this.FinState(corpCode, bsnsYear, reprtCode : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/fnlttSinglAcnt.json"
        let res =  
            this.Request(url, [
                "corp_code", corpCode
                "bsns_year", bsnsYear
                "reprt_code", reprtCode.ToString()
            ])
        match res.Body with
        | Text t -> t |> Decode.fromString ``상장기업 주요계정 Response``.Decoder
        | _ -> failwith "no binary response expected"
        
    member this.``단일회사 주요계정`` = this.FinState
        
    /// 다중회사 주요계정
    member this.FinStateMany(corpCodes, bsnsYear, reprtCode : ReportCode) =
        let corpCodes = corpCodes |> List.reduce (fun x y -> x + "," + y)
        let url = "https://opendart.fss.or.kr/api/fnlttMultiAcnt.json"
        let res =  
            this.Request(url, [
                "corp_code", corpCodes
                "bsns_year", bsnsYear
                "reprt_code", reprtCode.ToString()
            ])
        match res.Body with
        | Binary b -> failwith "no binary response expected"
        | Text t -> t
        |> Decode.fromString ``상장기업 주요계정 Response``.Decoder
    
    member this.``다중회사 주요계정`` = this.FinStateMany
    
    /// 단일회사 전체 재무제표
    member this.FinStateAll(corpCode, bsnsYear, reprtCode : ReportCode, fsDiv) =
        let url = "https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json"
        let res =
            this.Request(url, [
                "corp_code", corpCode
                "bsns_year", bsnsYear
                "reprt_code", reprtCode.ToString()
                "fs_div", fsDiv
            ])
        
        match res.Body with
        | Text data -> data |> Decode.fromString ``상장기업 주요계정 Response``.Decoder
        | _ -> failwith "no binary reponse expected"

    member this.``단일회사 전체 재무제표`` = this.FinStateAll
    
    /// 재무제표 원본파일
    member this.FinStateXbrl(rceptNo, reprtCode : ReportCode) =
        let url = "https://opendart.fss.or.kr/api/fnlttXbrl.xml"
        let res = 
            this.Request(url, [
                "rcept_no", rceptNo
                "reprt_code", reprtCode.ToString()
            ])
        
        match res.Body with
        | Binary data -> data
        | Text t -> failwith "no text response is expected"
    
    member this.``재무제표 원본파일`` = this.FinStateXbrl