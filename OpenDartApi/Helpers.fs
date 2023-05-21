module OpenDartApi.Informations

open System

type AccountDate =
    | Exact of DateOnly
    | Span of fromDate: DateOnly * toDate: DateOnly

let parseDateOnly date = DateOnly.ParseExact(date, "yyyy.MM.dd")

let parseAccountDate (s: string) =
    match s.Split(' ') with
    | [| date; "현재" |] -> Exact(parseDateOnly date)
    | [| fromDate; "~"; toDate |] ->
        let fromDate = parseDateOnly fromDate
        let toDate = parseDateOnly toDate
        Span(fromDate, toDate)
    | _ -> failwith "Failed to parse account date"

let accountTerm (term: string) =
    match term.Split(' ') with
    | [| _; termNum; _ |] -> int32 termNum
    | _ -> failwithf "Malformed account term string: %s" term

let (|CFS|OFS|) s =
    match s with
    | "CFS" -> CFS
    | "OFS" -> OFS
    | _ -> failwithf "Malformed Consolidated/Separate Financial Statements string: %s" s

let (|BS|IS|) s =
    match s with
    | "BS" -> BS // 재무상태표
    | "IS" -> IS // 손익상태표
    | _ -> failwithf "Malformed Balance/Income Statement string: %s" s

type ConsolidatedFinStat<[<Measure>] 'TCurrency> =
    { CurrentAssets: decimal<'TCurrency>
      NonCurrentAssets: decimal<'TCurrency>
      TotalAssets: decimal<'TCurrency>
      CurrentLiabilities: decimal<'TCurrency>
      NonCurrentLiabilities: decimal<'TCurrency>
      TotalLiabilities: decimal<'TCurrency> }


module AccountName =
    let (|CurrentAssets|_|) =
        function
        | "유동자산" -> Some()
        | _ -> None

    let (|NonCurrentAssets|_|) =
        function
        | "비유동자산" -> Some()
        | _ -> None

    let (|TotalAssets|_|) =
        function
        | "자산총계" -> Some()
        | _ -> None

    let (|CurrentLiabilities|_|) =
        function
        | "유동부채" -> Some()
        | _ -> None

    let (|NonCurrentLiabilities|_|) =
        function
        | "비유동부채" -> Some()
        | _ -> None

    let (|TotalLiabilities|_|) =
        function
        | "부채총계" -> Some()
        | _ -> None

    let (|CapitalStock|_|) =
        function
        | "자본금" -> Some()
        | _ -> None

    let (|RetainedEarnings|_|) =
        function
        | "이익잉여금" -> Some()
        | _ -> None

    let (|TotalEquity|_|) =
        function
        | "자본총계" -> Some()
        | _ -> None

    let (|Revenue|_|) =
        function
        | "매출액" -> Some()
        | _ -> None

    let (|OperatingProfit|_|) =
        function
        | "영업이익" -> Some()
        | _ -> None

    let (|IncomeBeforeTax|_|) =
        function
        | "법인세차감전 순이익" -> Some()
        | _ -> None

    let (|NetIncome|_|) =
        function
        | "당기순이익" -> Some()
        | _ -> None
