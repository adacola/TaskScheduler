namespace Adacola.TaskScheduler

module Holiday =
    open System
    open FSharp.Data
    open Basis.Core
    
    type HolidayType =
        | Fixed of month : int * day : int
        | Fluctuation of month : int * weekOfMonth : int * dayOfWeek : DayOfWeek
        | VernalEquinoxDay
        | AutumnalEquinoxDay

    type HolidayDefinition = {
        Type : HolidayType
        StartYear : int option
        EndYear : int option
    }

    type HolidayDefinitionProvider = JsonProvider<"JapaneseHoliday.json">

    let parseHolidayDefinition jsonString =
        let toDefinition index (element : HolidayDefinitionProvider.Root) =
            match element.Type with
            | "fixed" ->
                let description = element.Fixed |> Option.getOrElse (fun () -> failwithf "%d番目の要素が不正です。fixed項目がありません。" index)
                { Type = Fixed(description.Month, description.Day); StartYear = element.StartYear; EndYear= element.EndYear }
            | "fluctuation" ->
                let description = element.Fluctuation |> Option.getOrElse (fun () -> failwithf "%d番目の要素が不正です。fluctuation項目がありません。" index)
                { Type = Fluctuation(description.Month, description.WeekOfMonth, enum description.DayOfWeek); StartYear = element.StartYear; EndYear= element.EndYear }
            | "vernalEquinoxDay" -> { Type = VernalEquinoxDay; StartYear = element.StartYear; EndYear= element.EndYear }
            | "autumnalEquinoxDay" -> { Type = AutumnalEquinoxDay; StartYear = element.StartYear; EndYear= element.EndYear }
            | otherwise -> failwithf "%d番目の要素が不正です。不正なtype(%s)です。fixed,fluctuation,vernalEquinoxDay,autumnalEquinoxDayのいずれかを指定してください。" index otherwise

        let definitionRoot = HolidayDefinitionProvider.Parse jsonString
        definitionRoot |> Seq.mapi toDefinition |> Seq.toList

    let tryGetDate year holidayDefinition =
        let getEquinoxDay start year = start + 0.242194 * float (year - 1980) - float ((year - 1980) / 4) |> int
        let getVernalEquinoxDay year = DateTime(year, 3, getEquinoxDay 20.8431 year)
        let getAutumnalEquinoxDay year = DateTime(year, 9, getEquinoxDay 23.2488 year)

        let startYear = holidayDefinition.StartYear |> defaultArg <| 2000
        let endYear = holidayDefinition.EndYear |> defaultArg <| Int32.MaxValue
        if startYear <= year && year <= endYear then
            match holidayDefinition.Type with
            | Fixed(month, day) -> DateTime(year, month, day)
            | Fluctuation(month, weekOfMonth, dayOfWeek) ->
                [for i in -6 .. 0 -> DateTime(year, month, weekOfMonth * 7 + i)]
                |> List.find (fun x -> x.DayOfWeek = dayOfWeek)
            | VernalEquinoxDay -> getVernalEquinoxDay year
            | AutumnalEquinoxDay -> getAutumnalEquinoxDay year
            |> Some
        else None

    let japaneseHolidayDefinitions =
        let defaultDef = { Type = Fixed(1, 1); StartYear = None; EndYear = None }
        [
            { defaultDef with Type = Fixed(1, 1) }
            { defaultDef with Type = Fluctuation(1, 2, DayOfWeek.Monday) }
            { defaultDef with Type = Fixed(2, 11) }
            { defaultDef with Type = VernalEquinoxDay }
            { defaultDef with Type = Fixed(4, 29) }
            { defaultDef with Type = Fixed(5, 3) }
            { defaultDef with Type = Fixed(5, 4); StartYear = Some 2007 }
            { defaultDef with Type = Fixed(5, 5) }
            { defaultDef with Type = Fixed(7, 20); EndYear = Some 2002 }
            { defaultDef with Type = Fluctuation(7, 3, DayOfWeek.Monday); StartYear = Some 2003 }
            { defaultDef with Type = Fixed(8, 11); StartYear = Some 2016 }
            { defaultDef with Type = Fixed(9, 15); EndYear = Some 2002 }
            { defaultDef with Type = Fluctuation(9, 3, DayOfWeek.Monday); StartYear = Some 2003 }
            { defaultDef with Type = AutumnalEquinoxDay }
            { defaultDef with Type = Fluctuation(10, 2, DayOfWeek.Monday) }
            { defaultDef with Type = Fixed(11, 3) }
            { defaultDef with Type = Fixed(11, 23) }
            { defaultDef with Type = Fixed(12, 23) }
        ]
        
    let getHolidays includesSubstituteHoliday includesNatioalHoliday holidayDefinitions year =
        let rec getSubstituteHolidays result = function
            | [] -> result |> List.rev
            | (holiday : DateTime)::holidays when holiday.DayOfWeek = DayOfWeek.Sunday ->
                let substituteHoliday =
                    Seq.initInfinite (fun i -> holiday.AddDays(float i + 1.0))
                    |> Seq.find (fun d -> holidays |> List.forall ((<>) d))
                getSubstituteHolidays (substituteHoliday::result) holidays
            | _::holidays -> getSubstituteHolidays result holidays

        let getNationalHolidays holidays =
            holidays |> List.pairwise |> List.choose (function
                | (x : DateTime), y when y - x = TimeSpan(2, 0, 0, 0) -> x.AddDays 1.0 |> Some
                | _ -> None)
        let holidays = holidayDefinitions |> List.choose (tryGetDate year)
        let substituteHolidays = if includesSubstituteHoliday then getSubstituteHolidays [] holidays else []
        let nationalHolidays = if includesNatioalHoliday then getNationalHolidays holidays else []
        set holidays + set substituteHolidays + set nationalHolidays
