module OpenDartApi.ResponseTypes

open Thoth.Json.Net

let toDecimal  =
    Option.bind (fun s ->
        match s with
        | _ when System.String.IsNullOrEmpty s -> None
        | "-" -> None
        | x ->
            match System.Decimal.TryParse x with
            | true, d -> d |> Some
            | _ -> None)

/// 1.2 기업개황 API 응답
type ``기업개황 Response`` = {
    Status : string
    Message : string
    정식명칭 : string
    영문명칭 : string
    종목명 : string
    종목코드 : string
    CEO명 : string
    법인구분 : string
    법인등록번호 : string
    사업자등록번호 : string
    주소 : string
    홈페이지 : string
    IR주소 : string
    전화번호 : string
    팩스번호 : string
    업종코드 : string
    설립일 : string
    결산월 : string
} with
    static member Decoder : Decoder<``기업개황 Response``> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            정식명칭 = get.Required.Field "corp_name" Decode. string
            영문명칭 = get.Required.Field "corp_name_eng" Decode.string
            종목명 = get.Required.Field "stock_name" Decode.string
            종목코드 = get.Required.Field  "stock_code" Decode.string
            CEO명 = get.Required.Field "ceo_nm" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string
            법인등록번호 = get.Required.Field "jurir_no"  Decode.string
            사업자등록번호 = get.Required.Field "bizr_no" Decode.string
            주소 = get.Required.Field "adres" Decode.string
            홈페이지 = get.Required.Field "hm_url" Decode.string
            IR주소 = get.Required.Field "ir_url" Decode.string
            전화번호 = get.Required.Field "phn_no" Decode.string
            팩스번호 = get.Required.Field "fax_no" Decode.string
            업종코드 = get.Required.Field "induty_code" Decode.string // 업종코드
            설립일 = get.Required.Field "est_dt" Decode.string
            결산월 = get.Required.Field "acc_mt" Decode.string
        })

/// 1.1 - 공시정보 API 응답
type ``공시정보 Response`` = {
    Status : string
    Message : string
    ``페이지 번호`` : int option
    ``페이지 수`` : int option
    ``총 건수`` : int option
    ``총 페이지 수`` : int option
    List : 공시정보 [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object
            (fun get -> {
                Status = get.Required.Field "status" Decode.string
                Message = get.Required.Field "message" Decode.string
                ``페이지 번호`` = get.Optional.Field "page_no" Decode.int
                ``페이지 수`` = get.Optional.Field "page_count" Decode.int
                ``총 건수`` = get.Optional.Field "total_count" Decode.int
                ``총 페이지 수`` = get.Optional.Field "total_page" Decode.int
                List = get.Optional.Field "list" <| Decode.array 공시정보.Decoder
            })
and 공시정보 = {
    기업구분 : string
    법인명 : string
    종목코드 : string
    보고서명 : string
    접수번호 : string
    ``공시 제출인 명`` : string
    접수일자 : string
    비고 : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            기업구분 = get.Required.Field "corp_cls" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            종목코드 = get.Required.Field "stock_code" Decode.string
            보고서명 = get.Required.Field "report_nm" Decode.string
            접수번호 = get.Required.Field "rcept_no" Decode.string
            ``공시 제출인 명`` = get.Required.Field "flr_nm" Decode.string
            접수일자 = get.Required.Field "rcept_dt" Decode.string
            비고 = get.Required.Field "rm" Decode.string
        })

/// 2.1 - 증자(감자) 현황
type ``증자감자현황 Response`` = {
    Status : string
    Message : string
    사업연도 : string option
    ``보고서 코드`` : string option
    List : 증자감자 [] option
} with
    static member Decoder : Decoder<``증자감자현황 Response``> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            사업연도 = get.Optional.Field "bsns_year" Decode.string
            ``보고서 코드`` = get.Optional.Field "reprt_code" Decode.string
            List = get.Optional.Field "list" <| Decode.array 증자감자.Decoder
        })
and 증자감자 = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    ``주식발행 감소일자`` : string
    ``발행 감소 형태`` : string
    ``발행 감소 주식 종류`` : string
    ``발행 감소 수량`` : decimal 
    ``발행 감소 주당 액면 가액`` : decimal
    ``발행 감소 주당 가액`` : decimal
} with
    static member Decoder : Decoder<증자감자> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string
            고유번호 = get.Required.Field "corp_code" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            ``주식발행 감소일자`` = get.Required.Field "stock_isu_dcrs_de" Decode.string
            ``발행 감소 형태`` = get.Required.Field "isu_dcrs_stle" Decode.string
            ``발행 감소 주식 종류`` = get.Required.Field "isu_dcrs_stock_knd" Decode.string
            ``발행 감소 수량`` = get.Required.Field "isu_dcrs_qy" Decode.decimal
            ``발행 감소 주당 액면 가액`` = get.Required.Field "isu_dcrs_mstvdv_fval_amount" Decode.decimal
            ``발행 감소 주당 가액`` = get.Required.Field "isu_dcrs_mstvdv_amount" Decode.decimal
        })

/// 2.2 - 배당에 관한 사항
type ``배당관련사항 Response`` = {
    Status : string
    Message : string
    List : 배당관련사항 [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array 배당관련사항.Decoder
        })
and 배당관련사항 = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    구분 : string // 구분
    ``주식 종류`` : string
    당기 : decimal
    전기 : decimal
    전전기 : decimal
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string
            고유번호 = get.Required.Field "corp_code" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            구분 = get.Required.Field "se" Decode.string
            ``주식 종류`` = get.Required.Field "stock_knd" Decode.string
            당기 = get.Required.Field "thstrm" Decode.decimal
            전기 = get.Required.Field "frmtrm" Decode.decimal
            전전기 = get.Required.Field "lwfr" Decode.decimal
        })

/// 2.3 - 자기주식 취득 및 처분 현황
type ``자기주식 취득 및 처분 현황 Response`` = {
    Status : string
    Message : string
    List : ``자기주식 취득 및 처분`` [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array ``자기주식 취득 및 처분``.Decoder
        })
and ``자기주식 취득 및 처분`` = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    ``취득방법 대분류`` : string
    ``취득방법 중분류`` : string
    ``취득방법 소분류`` : string
    ``주식 종류`` : string
    ``기초 수량`` : decimal
    ``변동 수량 취득`` : decimal
    ``변동 수량 처분`` : decimal
    ``변동 수량 소각`` : decimal
    ``기말 수량`` : decimal
    비고 : string
} with
    static member Decoder : Decoder<``자기주식 취득 및 처분``> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string 
            법인구분 = get.Required.Field "corp_cls" Decode.string
            고유번호 = get.Required.Field "corp_code" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            ``취득방법 대분류`` = get.Required.Field "acqs_mth1" Decode.string
            ``취득방법 중분류`` = get.Required.Field "acqs_mth2" Decode.string
            ``취득방법 소분류`` = get.Required.Field "acqs_mth3" Decode.string
            ``주식 종류`` = get.Required.Field "stock_knd" Decode.string
            ``기초 수량`` = get.Required.Field "bsis_qy" Decode.decimal
            ``변동 수량 취득`` = get.Required.Field "change_qy_acqs" Decode.decimal
            ``변동 수량 처분`` = get.Required.Field "change_qy_dsps" Decode.decimal
            ``변동 수량 소각`` = get.Required.Field "change_qy_incnr" Decode.decimal
            ``기말 수량`` = get.Required.Field "trmend_qy" Decode.decimal
            비고 = get.Required.Field "rm" Decode.string
        })

/// 2.4 - 최대주주 현황
type ``최대주주 현황 Response`` = {
    Status : string
    Message : string
    List : 최대주주현황 [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field  "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array 최대주주현황.Decoder
        })
and 최대주주현황 = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    이름 : string
    관계 : string
    ``주식 종류`` : string
    ``기초 소유 주식 수`` : decimal
    ``기초 소유 주식 지분 율`` : float
    ``기말 소유 주식 수`` : decimal
    ``기말 소유 주식 지분 율`` : float
    비고 : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string
            고유번호 = get.Required.Field "corp_code" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            이름 = get.Required.Field "nm" Decode.string
            관계 = get.Required.Field "relate" Decode.string
            ``주식 종류`` = get.Required.Field "stock_knd" Decode.string
            ``기초 소유 주식 수`` = get.Required.Field "bsis_posesn_stock_co" Decode.decimal
            ``기초 소유 주식 지분 율`` = get.Required.Field "bsis_posesn_stock_qota_rt" Decode.float
            ``기말 소유 주식 수`` = get.Required.Field "trmend_posesn_stock_co" Decode.decimal
            ``기말 소유 주식 지분 율`` = get.Required.Field "trmend_posesn_stock_qota_rt" Decode.float
            비고 = get.Required.Field "rm" Decode.string
        })

/// 2.5 - 최대주주 변동 현황
type ``최대주주 변동 현황 Response`` = {
    Status : string
    Message : string
    List : ``최대주주 변동 현황`` [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field  "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array ``최대주주 변동 현황``.Decoder
        })
and ``최대주주 변동 현황`` = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    변동일 : string
    ``최대 주주 명`` : string
    ``소유 주식 수`` : decimal
    지분율 : float
    ``변동 원인`` : string
    비고 : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string
            고유번호 = get.Required.Field "corp_code" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            변동일 = get.Required.Field "change_on" Decode.string
            ``최대 주주 명`` = get.Required.Field "mxmm_shrholdr_nm" Decode.string
            ``소유 주식 수`` = get.Required.Field "posesn_stock_co" Decode.decimal
            지분율 = get.Required.Field "qota_rt" Decode.float
            ``변동 원인`` = get.Required.Field "change_cause" Decode.string
            비고 = get.Required.Field "change_cause" Decode.string
        })

/// 2.6 - 소액주주 현황
type ``소액주주 현황 Response`` = {
    Status : string
    Message : string
    List : ``소액주주 현황`` [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array ``소액주주 현황``.Decoder
        })
and ``소액주주 현황`` = {
    접수번호 : string
    법인구분 : string // corp_cls
    고유번호 : string // corp_code
    법인명 : string // corp_name
    구분 : string // se
    주주수 : decimal // shrholdr_co
    ``전체 주주수`` : decimal // shrholdr_tot_co
    ``주주 비율`` : string // shrholdr_rate
    ``보유 주식수`` : decimal  // hold_stock_co
    ``총발행 주식수`` : decimal // stock_tot_co
    ``보유 주식 비율`` : string // hold_stock_rate
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string 
            고유번호 = get.Required.Field "corp_code" Decode.string 
            법인명 = get.Required.Field "corp_name" Decode.string 
            구분 = get.Required.Field "se" Decode.string 
            주주수 = get.Required.Field "shrholdr_co" Decode.decimal 
            ``전체 주주수`` = get.Required.Field "shrholdr_tot_co" Decode.decimal 
            ``주주 비율`` = get.Required.Field "shrholdr_rate" Decode.string 
            ``보유 주식수`` = get.Required.Field "hold_stock_co" Decode.decimal
            ``총발행 주식수`` = get.Required.Field "stock_tot_co" Decode.decimal 
            ``보유 주식 비율`` = get.Required.Field "hold_stock_rate" Decode.string 
        })

/// 2.7 - 임원현황
type ``임원현황 Response`` = {
    Status : string
    Message : string
    List : 임원현황 [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array 임원현황.Decoder
        })
and 임원현황 = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    이름 : string
    성별 : string
    ``출생 년월`` : string
    직위 : string
    ``등기 임원 여부`` : string
    ``상근 여부`` : string
    ``담당 업무`` : string
    ``주요 경력`` : string
    ``최대 주주 관계`` : string
    ``재직 기간`` : string
    ``임기 만료일`` : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string
            고유번호 = get.Required.Field "corp_code" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            이름 = get.Required.Field "nm" Decode.string
            성별 = get.Required.Field "sexdstn" Decode.string
            ``출생 년월`` = get.Required.Field "birth_ym" Decode.string
            직위 = get.Required.Field "ofcps" Decode.string
            ``등기 임원 여부`` = get.Required.Field "rgist_exctv_at" Decode.string
            ``상근 여부`` = get.Required.Field "fte_at" Decode.string
            ``담당 업무`` = get.Required.Field "chrg_job" Decode.string
            ``주요 경력`` = get.Required.Field "main_career" Decode.string
            ``최대 주주 관계`` = get.Required.Field "mxmm_shrholdr_relate" Decode.string
            ``재직 기간`` = get.Required.Field "hffc_pd" Decode.string
            ``임기 만료일`` = get.Required.Field "tenure_end_on" Decode.string
        })

/// 2.8 - 직원현황
type ``직원현황 Response`` = {
    Status : string
    Message : string
    List : 직원현황 [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <|  Decode.array 직원현황.Decoder
        })
and 직원현황 = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    사업부문 : string
    성별 : string
    ``개정 전 직원 수 정규직`` : string
    ``개정 전 직원 수 계약직`` : string
    ``개정 전 직원 수 기타`` : string
    ``정규직 수`` : decimal
    ``정규직 단시간 근로자 수`` : decimal
    ``계약직 수`` : decimal
    ``계약직 단시간 근로자 수`` : decimal
    합계 : decimal
    ``평균 근속 연수`` : decimal
    ``연간 급여 총액`` : decimal
    ``1인평균 급여 액`` : decimal
    비고 : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string 
            고유번호 = get.Required.Field "corp_code" Decode.string 
            법인명 = get.Required.Field "corp_name" Decode.string
            사업부문 = get.Required.Field "fo_bbm" Decode.string
            성별 = get.Required.Field "sexdstn" Decode.string
            ``개정 전 직원 수 정규직``  = get.Required.Field "reform_bfe_emp_co_rgllbr" Decode.string
            ``개정 전 직원 수 계약직``  = get.Required.Field "reform_bfe_emp_co_cnttk" Decode.string
            ``개정 전 직원 수 기타``  = get.Required.Field "reform_bfe_emp_co_etc" Decode.string
            ``정규직 수``  = get.Required.Field "rgllbr_co" Decode.decimal
            ``정규직 단시간 근로자 수``  = get.Required.Field "rgllbr_abacpt_labrr_co" Decode.decimal
            ``계약직 수``  = get.Required.Field "cnttk_co" Decode.decimal
            ``계약직 단시간 근로자 수``  = get.Required.Field "cnttk_abacpt_labrr_co" Decode.decimal
            합계 = get.Required.Field "sm" Decode.decimal
            ``평균 근속 연수``  = get.Required.Field "avrg_cnwk_sdytrn" Decode.decimal
            ``연간 급여 총액``  = get.Required.Field "fyer_salary_totamt" Decode.decimal
            ``1인평균 급여 액``  = get.Required.Field "jan_salary_am" Decode.decimal
            비고  = get.Required.Field "rm" Decode.string
        })

/// 2.9 이사, 감사의 개인별 보수 현황
type ``이사감사 개인별 보수 현황 Response`` = {
    Status : string
    Message : string
    List : ``이사감사 개인별 보수 현황`` [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array``이사감사 개인별 보수 현황``.Decoder
        })
and ``이사감사 개인별 보수 현황`` = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    ``이름`` : string
    ``직위`` : string
    ``보수 총액`` : decimal
    ``보수 총액 비 포함 보수`` : decimal
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string
            고유번호 = get.Required.Field "corp_code" Decode.string
            법인명 = get.Required.Field "corp_name" Decode.string
            ``이름`` = get.Required.Field "nm" Decode.string
            ``직위`` = get.Required.Field "ofcps" Decode.string
            ``보수 총액`` = get.Required.Field "mendng_totamt" Decode.decimal
            ``보수 총액 비 포함 보수`` = get.Required.Field "mendng_totamt_ct_incls_mendng" Decode.decimal
        })

/// 2.10 - 이사, 감사 전체의 보수 현황
type ``이사감사 전체의 보수 현황 Response`` = {
    Status : string
    Message : string
    List : ``이사감사 전체의 보수 현황`` [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array ``이사감사 전체의 보수 현황``.Decoder
        })
and ``이사감사 전체의 보수 현황`` = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    인원수 : string
    ``보수 총액`` : string
    ``1인 평균 보수 액`` : string
    비고 : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string 
            고유번호 = get.Required.Field "corp_code" Decode.string 
            법인명 = get.Required.Field "corp_name" Decode.string
            인원수 = get.Required.Field "nmpr" Decode.string
            ``보수 총액`` = get.Required.Field "mendng_totamt" Decode.string
            ``1인 평균 보수 액`` = get.Required.Field "jan_avrg_mendng_am" Decode.string
            비고 = get.Required.Field "rm" Decode.string
        })

/// 2.11 - 개인별 보수지급 금액(5억이상 상위5인)
type ``개인별 보수지급 금액 Response`` = {
    Status : string
    Message : string
    List : ``개인별 보수지급 금액`` [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array ``개인별 보수지급 금액``.Decoder
        })
and ``개인별 보수지급 금액`` = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    이름 : string
    직위 : string
    ``보수 총액`` : decimal
    ``보수 총액 비 포함 보수`` : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string 
            고유번호 = get.Required.Field "corp_code" Decode.string 
            법인명 = get.Required.Field "corp_name" Decode.string
            이름 = get.Required.Field "nm" Decode.string
            직위 = get.Required.Field "ofcps" Decode.string
            ``보수 총액`` = get.Required.Field "mendng_totamt" Decode.decimal
            ``보수 총액 비 포함 보수``  = get.Required.Field "mendng_totamt_ct_incls_mendng" Decode.string
        })

/// 2.12 - 타법인 출자현황
type ``타법인 출자현황 Response`` = {
    Status : string
    Message : string
    List : ``타법인 출자현황`` [] option
} with 
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            Status = get.Required.Field "status" Decode.string
            Message = get.Required.Field "message" Decode.string
            List = get.Optional.Field "list" <| Decode.array ``타법인 출자현황``.Decoder
        })
and ``타법인 출자현황`` = {
    접수번호 : string
    법인구분 : string
    고유번호 : string
    법인명 : string
    대상법인명 : string
    최초취득일자 : string
    출자목적 : string
    최초취득금액 : string
    ``기초 잔액 수량`` : string
    ``기초 잔액 지분 율`` : string
    ``기초 잔액 장부 가액`` : string
    ``증가 감소 취득 처분 수량`` : string
    ``증가 감소 취득 처분 금액`` : string
    ``증가 감소 평가 손액`` : string
    ``기말 잔액 수량`` : string
    ``기말 잔액 지분 율`` : string
    ``기말 잔액 장부 가액`` : string
    ``최근 사업 연도 재무 현황 총 자산`` : string
    ``최근 사업 연도 재무 현황 당기 순이익`` : string
} with
    static member Decoder : Decoder<_> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            법인구분 = get.Required.Field "corp_cls" Decode.string 
            고유번호 = get.Required.Field "corp_code" Decode.string 
            법인명 = get.Required.Field "corp_name" Decode.string
            대상법인명 = get.Required.Field "inv_prm" Decode.string
            최초취득일자 = get.Required.Field "frst_acqs_de" Decode.string
            출자목적 = get.Required.Field "invstmnt_purps" Decode.string
            최초취득금액 = get.Required.Field "frst_acqs_amount" Decode.string
            ``기초 잔액 수량`` = get.Required.Field "bsis_blce_qy" Decode.string
            ``기초 잔액 지분 율`` = get.Required.Field "bsis_blce_qota_rt" Decode.string
            ``기초 잔액 장부 가액`` = get.Required.Field "bsis_blce_acntbk_amount" Decode.string
            ``증가 감소 취득 처분 수량`` = get.Required.Field "incrs_dcrs_acqs_dsps_qy" Decode.string
            ``증가 감소 취득 처분 금액`` = get.Required.Field "incrs_dcrs_acqs_dsps_amount" Decode.string
            ``증가 감소 평가 손액`` = get.Required.Field "incrs_dcrs_evl_lstmn" Decode.string
            ``기말 잔액 수량`` = get.Required.Field "trmend_blce_qy" Decode.string
            ``기말 잔액 지분 율`` = get.Required.Field "trmend_blce_qota_rt" Decode.string
            ``기말 잔액 장부 가액`` = get.Required.Field "trmend_blce_acntbk_amount" Decode.string
            ``최근 사업 연도 재무 현황 총 자산`` = get.Required.Field "recent_bsns_year_fnnr_sttus_tot_assets" Decode.string
            ``최근 사업 연도 재무 현황 당기 순이익`` = get.Required.Field "recent_bsns_year_fnnr_sttus_thstrm_ntpf" Decode.string
        })

/// 3 - 상장기업 재무정보
/// 3.1 - 단일회사 주요계정 (재무상태표, 손익계산서)
/// 3.2 - 다중회사 주요계정 (재무상태표, 손익계산서)
/// 3.4 - 단일회사 전체 재무제표
type ``상장기업 주요계정 Response`` = {
    Status : string
    Message : string
    List : 계정 [] option
} with
    static member Decoder : Decoder<_> =
        Decode.object
            (fun get -> {
                Status = get.Required.Field "status" Decode.string
                Message = get.Required.Field "message" Decode.string
                List = get.Optional.Field "list" <| Decode.array 계정.Decoder
            })
and 계정 = {
    접수번호 : string
    사업연도 : int
    종목코드 : string option
    ``리포트 코드`` : string
    계정Id : string option
    계정명 : string
    계정상세 : string option
    ``개별/연결구분`` : string option
    ``개별/연결명`` : string option
    재무제표구분 : string
    재무제표명 : string
    당기명 : string
    당기일자 : string option
    당기금액 : decimal option
    당기누적금액 : decimal option
    전기명 : string
    전기일자 : string option
    전기금액 : decimal option
    전기누적금액 : decimal option
    ``전기명(분/반기)`` : string option
    ``전기금액(분/반기)`` : decimal option
    전전기명 : string option
    전전기일자 : string option
    전전기금액 : decimal option
    ``계정과목 정렬순서`` : int
} with
    static member Decoder : Decoder<계정> =
        Decode.object (fun get -> {
            접수번호 = get.Required.Field "rcept_no" Decode.string
            사업연도 = get.Required.Field "bsns_year" Decode.int
            종목코드 = get.Optional.Field "stock_code" Decode.string
            ``리포트 코드`` = get.Required.Field "reprt_code" Decode.string
            계정Id = get.Optional.Field "account_id" Decode.string
            계정명 = get.Required.Field "account_nm" Decode.string
            계정상세 = get.Optional.Field "account_detail" Decode.string
            ``개별/연결구분`` = get.Optional.Field "fs_div" Decode.string
            ``개별/연결명`` = get.Optional.Field "fs_nm" Decode.string
            재무제표구분 = get.Required.Field "sj_div" Decode.string
            재무제표명 = get.Required.Field "sj_nm" Decode.string
            당기명 = get.Required.Field "thstrm_nm" Decode.string
            당기일자 = get.Optional.Field "thstrm_dt" Decode.string
            당기금액 =
                get.Optional.Field "thstrm_amount" Decode.string
                |> toDecimal
            당기누적금액 =
                get.Optional.Field "thstrm_add_amount" Decode.string
                |> toDecimal
            전기명 = get.Required.Field "frmtrm_nm" Decode.string
            전기일자 = get.Optional.Field "frmtrm_dt" Decode.string
            전기금액 =
                get.Optional.Field "frmtrm_amount" Decode.string
                |> toDecimal
            ``전기명(분/반기)`` = get.Optional.Field "frmtrm_q_nm" Decode.string
            ``전기금액(분/반기)`` = get.Optional.Field "frmtrm_q_amount" Decode.decimal
            전기누적금액 =
                get.Optional.Field "frmtrm_add_amount" Decode.string
                |> toDecimal
            전전기명 = get.Optional.Field "bfefrmtrm_nm" Decode.string
            전전기일자 = get.Optional.Field "bfefrmtrm_dt" Decode.string
            전전기금액 =
                get.Optional.Field "bfefrmtrm_amount" Decode.string
                |> toDecimal
            ``계정과목 정렬순서`` = get.Required.Field "ord" Decode.int
        })
