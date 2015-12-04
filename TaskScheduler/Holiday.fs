namespace Adacola.TaskScheduler

module Holiday =
    open System
    
    type HolidayType =
        | Fixed of month : int * day : int
        | Fluctuation of month : int * weekOfMonth : int * dayOfWeek : DayOfWeek
        | VernalEquinoxDay
        | AutumnalEquinoxDay

    let getDate year holidayType =
        let getEquinoxDay start year = start + 0.242194 * float (year - 1980) - float ((year - 1980) / 4) |> int
        let getVernalEquinoxDay year = DateTime(year, 3, getEquinoxDay 20.8431 year)
        let getAutumnalEquinoxDay year = DateTime(year, 9, getEquinoxDay 23.2488 year)

        match holidayType with
        | Fixed(month, day) -> DateTime(year, month, day)
        | Fluctuation(month, weekOfMonth, dayOfWeek) ->
            [for i in -6 .. 0 -> DateTime(year, month, weekOfMonth * 7 + i)]
            |> List.find (fun x -> x.DayOfWeek = dayOfWeek)
        | VernalEquinoxDay -> getVernalEquinoxDay year
        | AutumnalEquinoxDay -> getAutumnalEquinoxDay year

    let japaneseHolidayTypes = [
        Fixed(1, 1)
        Fluctuation(1, 2, DayOfWeek.Monday)
        Fixed(2, 11)
        VernalEquinoxDay
        Fixed(4, 29)
        Fixed(5, 3)
        Fixed(5, 4)
        Fixed(5, 5)
        Fluctuation(7, 3, DayOfWeek.Monday)
        Fixed(8, 11)
        Fluctuation(9, 3, DayOfWeek.Monday)
        AutumnalEquinoxDay
        Fluctuation(10, 2, DayOfWeek.Monday)
        Fixed(11, 3)
        Fixed(11, 23)
        Fixed(12, 23)
    ]
        
    let getJapaneseHolidays year =
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
        let holidays = japaneseHolidayTypes |> List.map (getDate year)
        holidays @ getSubstituteHolidays [] holidays @ getNationalHolidays holidays
