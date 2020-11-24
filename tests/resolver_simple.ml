let mem (t : Time.t) (timestamp : Time.timestamp) : bool =
  let open Time in
  let rec aux t timestamp =
    match Time.Date_time.of_timestamp timestamp with
    | Error () -> failwith (Printf.sprintf "Invalid timestamp: %Ld" timestamp)
    | Ok dt -> (
        let weekday =
          Result.get_ok
          @@ Time.weekday_of_month_day ~year:dt.year ~month:dt.month
            ~mday:dt.day
        in
        let second_of_day =
          make_hms ~hour:dt.hour ~minute:dt.minute ~second:dt.second
          |> Result.get_ok
          |> second_of_day_of_hms
        in
        match t with
        | Timestamp_interval_seq (_, s) ->
          OSeq.exists
            (fun (start, end_exc) ->
               start <= timestamp && timestamp < end_exc)
            s
        | Pattern (_, pattern) ->
          let year_is_fine =
            match pattern.years with [] -> true | l -> List.mem dt.year l
          in
          let month_is_fine =
            match pattern.months with [] -> true | l -> List.mem dt.month l
          in
          let mday_is_fine =
            match pattern.month_days with
            | [] -> true
            | l -> List.mem dt.day l
          in
          let wday_is_fine =
            match pattern.weekdays with [] -> true | l -> List.mem weekday l
          in
          let hour_is_fine =
            match pattern.hours with [] -> true | l -> List.mem dt.hour l
          in
          let minute_is_fine =
            match pattern.hours with [] -> true | l -> List.mem dt.minute l
          in
          let second_is_fine =
            match pattern.hours with [] -> true | l -> List.mem dt.second l
          in
          let timestamp_is_fine =
            match pattern.timestamps with
            | [] -> true
            | l -> List.mem timestamp l
          in
          year_is_fine
          && month_is_fine
          && mday_is_fine
          && wday_is_fine
          && hour_is_fine
          && minute_is_fine
          && second_is_fine
          && timestamp_is_fine
        | Branching (_, branching) ->
          List.exists
            (fun year_range ->
               match year_range with
               | `Range_inc (x, y) -> x <= dt.year && dt.year <= y
               | `Range_exc (x, y) -> x <= dt.year && dt.year < y)
            branching.years
          && List.exists
            (fun month_range ->
               match month_range with
               | `Range_inc (x, y) ->
                 month_le x dt.month && month_le dt.month y
               | `Range_exc (x, y) ->
                 month_le x dt.month && month_lt dt.month y)
            branching.months
          && ( match branching.days with
              | Month_days days ->
                List.exists
                  (fun day_range ->
                     match day_range with
                     | `Range_inc (x, y) -> x <= dt.day && dt.day <= y
                     | `Range_exc (x, y) -> x <= dt.day && dt.day < y)
                  days
              | Weekdays _ -> failwith "Unimplemented" )
          && List.exists
            (fun hmss_range ->
               match hmss_range with
               | `Range_inc (x, y) ->
                 let x = Time.second_of_day_of_hms x in
                 let y = Time.second_of_day_of_hms y in
                 x <= second_of_day && second_of_day <= y
               | `Range_exc (x, y) ->
                 let x = Time.second_of_day_of_hms x in
                 let y = Time.second_of_day_of_hms y in
                 x <= second_of_day && second_of_day < y)
            branching.hmss
        | Unary_op (_, op, t) -> (
            match op with
            | Not -> Stdlib.not (aux t timestamp)
            | _ -> failwith "Unimplemented" )
        | Binary_op (_, op, t1, t2) -> (
            match op with
            | Union -> aux t1 timestamp || aux t2 timestamp
            | Inter -> aux t1 timestamp && aux t2 timestamp )
        | Interval_inc (_, start, end_inc) ->
          let start = Date_time.to_timestamp start in
          let end_inc = Date_time.to_timestamp end_inc in
          start <= timestamp && timestamp <= end_inc
        | Interval_exc (_, start, end_inc) ->
          let start = Date_time.to_timestamp start in
          let end_inc = Date_time.to_timestamp end_inc in
          start <= timestamp && timestamp < end_inc
        | Merge_list (_, l) -> List.exists (fun t -> aux t timestamp) l )
  in
  aux t timestamp
