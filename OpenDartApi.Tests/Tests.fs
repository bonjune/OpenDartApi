module Tests

open Expecto
open OpenDartApi.DartApi

let notImplemented implInfo =
    test implInfo {
        failtest "not implemented"
    }

let tests crtfc_key =
    let dart = DartApi(crtfc_key)
    
    testList "api calls" [
        testList "disclosure information section" [
            test "disclosure information api call" {
                let disc = dart.공시정보()
                Expect.isOk disc "getting disclosure api response and parsing failed"
            }
            
            test "overview api call" {
                let res = dart.기업개황("00126380")
                Expect.isOk res "getting overview api response and parsing failed"
            }
            notImplemented "disclosure file api call"
            test "corp code api call" {
                let res = dart.CorpCodeZipBytes()
                Expect.isNonEmpty res "getting corp codes failed"
            }
        ]
        testList "business report major information section" [
            test "증자(감자) 현황" {
                let res = dart.증자감자현황("00126380", "2020", ReportCode.Annual)
                Expect.isOk res "getting capital inc/dec api response and parsing failed"
            }
            test "배당에 관한 사항" {
                let res = dart.배당관련사항("00126380", "2020", ReportCode.Annual)
                Expect.isOk res "배당관련사항 테스트 실패"
            }
            test "자기주식 취득" {
                let res = dart.``자기주식 취득 및 처분 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res "자기주식 취득 및 처분 현황 테스트 실패"
            }
            test "최대주주 현황" {
                let res = dart.``최대주주 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "최대주주 변동" {
                let res = dart.``최대주주 변동 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "소액주주현황" {
                let res =  dart.``소액주주 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "임원현황" {
                let res = dart.``임원 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "직원현황" {
                let res = dart.``직원 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "이사ㆍ감사의 개인별 보수 현황" {
                let res = dart.``이사감사 개인별 보수 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "이사ㆍ감사 전체의 보수 현황" {
                let res = dart.``이사감사 전체 보수 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "개인별 보수지급" {
                let res = dart.``개인별 보수지급 금액``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
            test "타법인 출자현황"  {
                let res =  dart.``타법인 출자 현황``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res ""
            }
        ]
        testList "corporation financial statement section" [
            test "fin state api call" {
                let res = dart.``단일회사 주요계정``("00126380", "2020", ReportCode.Annual)
                Expect.isOk res "getting fin state api response and parsing failed"
            }
            test "fin state many api call" {
                let res = dart.``다중회사 주요계정``(["00126380"; "00401731"], "2020", ReportCode.Annual)
                Expect.isOk res "getting fin state many api response and parsing failed"
            }
            test "xbrl file download api call" {
                let res = dart.``재무제표 원본파일``("20210517001185", ReportCode.Annual)
                Expect.isNonEmpty res "getting xbrl file bytes failed"
            }
            test "fin state all api call" {
                let res = dart.``단일회사 전체 재무제표``("00126380", "2020", ReportCode.Annual, "IS")
                Expect.isOk res "getting fin state many api response and parsing failed"
            }
            notImplemented "xbrl taxonomy form api call" // Not in plan
        ]
        testList "share disclosure section" [
            notImplemented "대량보유 상황보고"
            notImplemented "임원주요주주 상황보고"
        ]
    ]

