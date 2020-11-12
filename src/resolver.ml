module Search_param = struct
  type start =
    [ `Unix_second of int64
    | `Date_time of Time.Date_time.t
    ]

  type typ =
    | Intervals of Time.Interval.t list
    | Years_ahead of {
        start : start;
        years_ahead : int;
      }

  type t = {
    search_using_tz_offset_s : Time.tz_offset_s option;
    typ : typ;
  }

  type error =
    | Invalid_start
    | Invalid_intervals
    | Invalid_search_years_ahead
    | Too_far_into_future

  let push_search_param_to_later_start ~(start : int64) (search_param : t) :
    (t, unit) result =
    match search_param.typ with
    | Intervals intervals -> (
        match Time.Intervals.Bound.min_start_and_max_end_exc_list intervals with
        | None -> Ok search_param
        | Some (start', end_exc') ->
          let start = max start' start in
          let intervals =
            intervals
            |> List.to_seq
            |> Time.Intervals.inter (Seq.return (start, end_exc'))
            |> List.of_seq
          in
          Ok { search_param with typ = Intervals intervals } )
    | Years_ahead { years_ahead; start = start' } -> (
        match start' with
        | `Unix_second start' ->
          let start = max start' start in
          Ok
            {
              search_param with
              typ = Years_ahead { years_ahead; start = `Unix_second start };
            }
        | `Date_time start' -> (
            match Time.Date_time.to_unix_second start' with
            | Error () -> Error ()
            | Ok start' ->
              let start = max start' start in
              Time.Date_time.of_unix_second
                ~tz_offset_s_of_date_time:
                  search_param.search_using_tz_offset_s start
              |> Result.map (fun start ->
                  {
                    search_param with
                    typ =
                      Years_ahead { years_ahead; start = `Date_time start };
                  }) ) )

  let start_date_time_and_search_years_ahead_of_search_param (search_param : t)
    : (Time.Date_time.t * int) option =
    match search_param.typ with
    | Intervals intervals -> (
        match Time.Intervals.Bound.min_start_and_max_end_exc_list intervals with
        | None -> None
        | Some (start, end_exc) ->
          let start =
            Time.Date_time.of_unix_second
              ~tz_offset_s_of_date_time:search_param.search_using_tz_offset_s
              start
            |> Result.get_ok
          in
          let end_exc =
            Time.Date_time.of_unix_second
              ~tz_offset_s_of_date_time:search_param.search_using_tz_offset_s
              end_exc
            |> Result.get_ok
          in
          let search_years_ahead = end_exc.year - start.year + 1 in
          Some (start, search_years_ahead) )
    | Years_ahead { years_ahead; start } -> (
        match start with
        | `Unix_second start ->
          let start =
            Time.Date_time.of_unix_second
              ~tz_offset_s_of_date_time:search_param.search_using_tz_offset_s
              start
            |> Result.get_ok
          in
          Some (start, years_ahead)
        | `Date_time start -> Some (start, years_ahead) )

  module Check = struct
    let check_search_param (x : t) : (unit, error) result =
      match x.typ with
      | Intervals intervals ->
        if
          List.for_all
            (fun (x, y) ->
               Time.Interval.Check.is_valid (x, y)
               && Time.Date_time.of_unix_second ~tz_offset_s_of_date_time:None
                 x
                  |> Result.is_ok
               && Time.Date_time.of_unix_second ~tz_offset_s_of_date_time:None
                 y
                  |> Result.is_ok)
            intervals
        then Ok ()
        else Error Invalid_intervals
      | Years_ahead { years_ahead; start } -> (
          match start with
          | `Unix_second start -> (
              match
                Time.Date_time.of_unix_second
                  ~tz_offset_s_of_date_time:x.search_using_tz_offset_s start
              with
              | Error () -> Error Invalid_start
              | Ok start ->
                if years_ahead <= 0 then Error Invalid_search_years_ahead
                else if start.year + years_ahead > Time.Date_time.max.year
                then Error Too_far_into_future
                else Ok () )
          | `Date_time start ->
            if Time.Check.date_time_is_valid start then
              if years_ahead <= 0 then Error Invalid_search_years_ahead
              else if start.year + years_ahead > Time.Date_time.max.year then
                Error Too_far_into_future
              else Ok ()
            else Error Invalid_start )
  end

  let of_intervals ?search_using_tz_offset_s (intervals : Time.Interval.t list)
    : (t, error) result =
    let t = { search_using_tz_offset_s; typ = Intervals intervals } in
    match Check.check_search_param t with Ok () -> Ok t | Error e -> Error e

  let of_years_ahead ?search_using_tz_offset_s ?(start : start option)
      years_ahead : (t, error) result =
    let t =
      {
        search_using_tz_offset_s;
        typ =
          Years_ahead
            {
              start =
                Option.value
                  ~default:(`Unix_second (Time.Current.cur_unix_second ()))
                  start;
              years_ahead;
            };
      }
    in
    match Check.check_search_param t with Ok () -> Ok t | Error e -> Error e
end
