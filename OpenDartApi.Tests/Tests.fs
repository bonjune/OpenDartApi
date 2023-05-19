module Tests

open Expecto
open OpenDartApi.ApiCaller

let notImplemented implInfo =
    test implInfo { failtest "not implemented" }

[<Literal>]
let SAMSUNG_ELECTRONICS = "00126380"

let tests crtfc_key =
    let dart = DartApi(crtfc_key)

    testList
        "api calls"
        [ testList
              "disclosure information section"
              [ testCaseAsync "disclosure information api call"
                <| async {
                    let! disc = dart.공시정보 ()
                    Expect.isOk disc "getting disclosure api response and parsing failed"
                }

                testCaseAsync "overview api call"
                <| async {
                    let! res = dart.기업개황 (SAMSUNG_ELECTRONICS)
                    Expect.isOk res "getting overview api response and parsing failed"
                }
                notImplemented "disclosure file api call"
                testCaseAsync "corp code api call"
                <| async {
                    let! res = dart.CorpCodeZipBytes()
                    Expect.isNonEmpty res "getting corp codes failed"
                } ]
          testList
              "business report major information section"
              [ testCaseAsync "증자(감자) 현황"
                <| async {
                    for year in 2018..2022 do
                        let! res = dart.증자감자현황 (SAMSUNG_ELECTRONICS, year, ReportCode.Annual)
                        Expect.isOk res "getting capital inc/dec api response and parsing failed"
                }
                testCaseAsync "배당에 관한 사항"
                <| async {
                    for year in 2018..2022 do
                        let! res = dart.배당관련사항 (SAMSUNG_ELECTRONICS, year, ReportCode.Annual)
                        Expect.isOk res "배당관련사항 테스트 실패"
                }
                testCaseAsync "자기주식 취득"
                <| async {
                    for year in 2018..2022 do
                        let! res = dart.``자기주식 취득 및 처분 현황`` (SAMSUNG_ELECTRONICS, year, ReportCode.Annual)
                        Expect.isOk res "자기주식 취득 및 처분 현황 테스트 실패"
                }
                testCaseAsync "최대주주 현황"
                <| async {
                    let! res = dart.``최대주주 현황`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                }
                testCaseAsync "최대주주 변동"
                <| async {
                    let! res = dart.``최대주주 변동 현황`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                }
                testCaseAsync "소액주주현황"
                <| async {
                    let! res = dart.``소액주주 현황`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                }
                testCaseAsync "임원현황"
                <| async {
                    for year in 2018..2022 do
                        let! res = dart.``임원 현황`` (SAMSUNG_ELECTRONICS, year, ReportCode.Annual)
                        Expect.isOk res "임원현황 테스트 실패"
                }
                testCaseAsync "직원현황"
                <| async {
                    let! res = dart.``직원 현황`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                }
                testCaseAsync "이사ㆍ감사의 개인별 보수 현황"
                <| async {
                    let! res = dart.``이사감사 개인별 보수 현황`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                }
                testCaseAsync "이사ㆍ감사 전체의 보수 현황"
                <| async {
                    let! res = dart.``이사감사 전체 보수 현황`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                }
                testCaseAsync "개인별 보수지급"
                <| async {
                    let! res = dart.``개인별 보수지급 금액`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                }
                testCaseAsync "타법인 출자현황"
                <| async {
                    let! res = dart.``타법인 출자 현황`` (SAMSUNG_ELECTRONICS, "2020", ReportCode.Annual)
                    Expect.isOk res ""
                } ]
          testList
              "corporation financial statement section"
              [
                // test "fin state api call" {
                //     let res = dart.``단일회사 주요계정`` ("00126380", "2020", ReportCode.Annual)
                //     res |> Async.
                //     Expect.isOk res "getting fin state api response and parsing failed"
                // }

                testCaseAsync "단일회사 주요계정"
                <| async {
                    let! result = dart.``단일회사 주요계정`` ("00126380", "2020", ReportCode.Annual)
                    let account = Expect.wantOk result "Json decoding should work"
                    do Expect.equal account.Status "000" "API requst parameters should be valid"
                    do Expect.isGreaterThan account.List.Value.Length 0 "API should return meaningful lists"
                }

                testCaseAsync "fin state many api call"
                <| async {
                    let! res = dart.``다중회사 주요계정`` ([ "00126380"; "00401731" ], "2020", ReportCode.Annual)
                    Expect.isOk res "getting fin state many api response and parsing failed"
                }
                testCaseAsync "xbrl file download api call"
                <| async {
                    let! res = dart.``재무제표 원본파일`` ("20210517001185", ReportCode.Annual)
                    Expect.isNonEmpty res "getting xbrl file bytes failed"
                }
                testCaseAsync "fin state all api call"
                <| async {
                    let! res = dart.``단일회사 전체 재무제표`` ("00126380", "2020", ReportCode.Annual, FinStmt.CFS)
                    Expect.isOk res "getting fin state many api response and parsing failed"
                }
                notImplemented "xbrl taxonomy form api call" ] // Not in plan
          testList "share disclosure section" [ notImplemented "대량보유 상황보고"; notImplemented "임원주요주주 상황보고" ] ]
