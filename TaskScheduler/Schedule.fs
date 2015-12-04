namespace Adacola.TaskScheduler

open System

[<Measure>] type Hour
[<Measure>] type Day

type DateHour = { Date : DateTime; Hour : int<Hour> }
type Task = { Name : string; Workload : decimal<Day> }
type Schedule = { Task : Task; Start : DateHour; End : DateHour }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Schedule =

    /// <summary>
    /// スケジュールを計算します。
    /// </summary>
    /// <param name="specialHolidays">土日以外の祝日、休日の集合</param>
    /// <param name="weight">作業に取り組める割合</param>
    /// <param name="startDate">作業開始日</param>
    /// <param name="tasks">作業リスト。先に行うタスクから並んでいる前提です。</param>
    /// <returns>スケジュールのリスト</returns>
    let calculate specialHolidays weight (startDate : DateTime) tasks =
        let add specialHolidays { Date = startDate; Hour = startHour } (duration : decimal<Day>) =
            // TODO O(n)をO(1)に直せる
            let rec add (date : DateTime) = function
                | 0<Day> -> date
                | n ->
                    let d = date.AddDays 1.0
                    let diff =
                        if d.DayOfWeek = DayOfWeek.Saturday || d.DayOfWeek = DayOfWeek.Sunday || Set.contains d specialHolidays
                        then 0<Day> else 1<Day>
                    n - diff |> add d 

            let durationDayDecimal = decimal duration |> floor |> (*) 1M<Day>
            let durationHour = (duration - durationDayDecimal) * 8M |> decimal |> int |> (*) 1<Hour>
            let durationDay = durationDayDecimal |> decimal |> int |> (*) 1<Day>

            let addDay, endHour = (startHour + durationHour) / 8<Hour/Day> + durationDay, (startHour + durationHour) % 8<Hour>
            let endDate = add startDate addDay
            { Date = endDate; Hour = endHour }

        (({ Date = startDate.Date; Hour = 0<Hour> }, []), tasks) ||> Seq.fold (fun (dateHour, schedules) task ->
            let workload = task.Workload / weight
            let endDateHour = add specialHolidays dateHour workload
            endDateHour, { Task = task; Start = dateHour; End = endDateHour }::schedules)
        |> snd |> List.rev

    /// スケジュールのリストをTSV形式の文字列に変換します。
    let toTsv schedules =
        let dToS (d : DateTime) = sprintf "%04d/%02d/%02d" d.Year d.Month d.Day
        schedules |> Seq.map (fun r ->
            sprintf "%s\t%O\t%s\t%s" r.Task.Name r.Task.Workload (dToS r.Start.Date) (dToS r.End.Date))
        |> String.concat "\r\n"
