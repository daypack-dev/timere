# Changelog

## Timere-parse 0.0.5

- Updated use of Timere API

- Added support for parsing "24:00" and "24:00:00"

- Added exception raising version of functions

- Upgraded ruleset

- Renamed `Timere_parse.duration` to `Timere_parse.span`

## Timere 0.6.0

- Added `inc_exc` argument to `bounded_intervals`

  - This fixes expressiveness issues caused by lack of an inclusive version

- Renamed `bounded_intervals` to `pattern_intervals`

- Swapped to using `inc_exc` for `hms_intervals` API

- Added nanosecond support for `pattern` and `Points.t`

- Added `Timere.resolve_exn`

- Minor fixes of `resolve` for edge cases

- Upgraded `Timere.Points.make` to do a lot more deduction of missing arguments

- Renamed `&` to `&&&`

- Removed `Timere.Hms` module, replaced use of `Timere.Hms.t` with `Timedesc.Time.t`

## Timedesc 0.5.0

- Made `Timedesc.Span.t` abstract

- Added accessors `get_s`, `get_ns_offset` and conversion function `to_s_ns` to `Timedesc.Span` module

- Made `Timedesc.Time_zone_info.t` abstract

- Added accessors `tz` and `fixed_offset_from_utc` to `Timedesc.Time_zone_info` module

- Added format string system to `Timedesc.Span.For_human.pp` and `to_string`

## Timedesc 0.4.0

- Added following to `Timedesc.Utils`

  - `jd_of_ymd`

  - `jd_of_date`

  - `jd_of_unix_epoch`

  - `jd_span_of_unix_epoch`

- Added `Zoneless` module

  - Moved "date and time" API into `Zoneless` module

  - `Zoneless` also includes extra ISO8601 parsing functions for handling ISO8601 strings
    with no time zone designators

- Updated definition of `Time_zone.equal` and `Time_zone.utc`

- Added comparison functions

  - `compare_chrono_min`

  - `compare_chrono_max`

  - `compare_struct`

## Timedesc 0.3.1

- Fixed `Timedesc.Span.make` handling of `ns` when `ns = Int.min_int`

- Added `Timedesc.Span.Out_of_range` exception for when it is not possible represent the value even after normalization

## Timere 0.5.0

- Fixed unhandled exception/crash due to trying to construct invalid date times when resolving patterns

- Fixed lossy behaviour in `Pattern_resolver.Branch.to_date_time`

- Fixed incorrect resolution due to incorrect definition of `Resolver.timestamp_safe_sub`

- General fixes in `pattern_resolver` and `resolver` for edge cases nearing `Timedesc.Timestamp.min_val` and `Timedesc.Timestamp.max_val`

- Bumped Timedesc dep to `0.3.0` for easier to handle time zone transitions
  (due to the updated definition of `Timedesc.Timestamp.max_val`)

  - This makes resolver code simpler

- Fixed crash due to `Points.to_date_time` by changing to simply return `None` if date time conversion fails

- Added automatic bound deduction by default for `bounded_intervals`

- Overhaul of search space optimization code in `resolver`

## Timedesc 0.3.0

- Added `ceil`, `floor` and `round` to `Timedesc.Span` (and also re-exported them in `Timedesc.Timestamp`)

- Rounded down `Timedesc.Timestamp.max_val` to closest integer

- Added automatic second fraction precision support for non-RFC3339 pretty printers

- Updated default format string for pretty printers to include second fraction

- Updated format string system to better support second fraction

## Timere 0.4.0

- Migration of date time components to Timedesc

- Changed `inter []` to mean `always` instead of `empty`

- Fixed `overapproximate_search_space_bottom_up` handling of bounded intervals

  - Previously the default search time zone was used during points to date time
    conversion instead of the time zone passed during execution

## Timedesc 0.2.0

- Added ISO int conversion functions for `weekday`

- Fixed `Timedesc.Date.day` accessor, which is used by `Timedesc.day`

- Renamed `week` to `iso_week` for ISO week date API

- Replaced use of `CCOpt.get_exn` with `CCOpt.get_exn_or`

- Made `Timedesc.Time.t` abstract

- Modified `Timedesc.Time.t` underlying representation

- Fixed `Timedesc.Time.make` to handle `24:00:00` "properly" by rewriting it to `23:59:59.999_999_999`

## Timedesc 0.1.0

- Migration of date time components from Timere

- Changes to date time components

  - Added recognition of "UTC+/-offset", e.g. "UTC+7", "UTC-07:00", to `Timere.Time_zone.make`

  - Tuning behaviour of `Timere.Time_zone.make` for handling time zones with `UTC` prefix

  - Updated ISO8601 parser to tolerate separator other than `T`

  - Updated ISO8601 parser to tolerate numbers expressed in only one digit

  - Added size checking to `Timere.Time_zone.make_offset_only*`

  - Removed raising of `Invalid_argument` outside of Timere combinators, `pp*` and some `*_exn` functions

  - Fixed `Points.make` error checking

  - Moved hms into `Hms` module

  - Improved hms error returning

  - Added `Timere.Week_date_time` module for handling ISO week date time

  - Removed type `month`, replaced use of it with just `int`

  - Documentation overhaul with readability improvement and introduction to date time handling added

    - See [issue #25](https://github.com/daypack-dev/timere/issues/25) and [#26](https://github.com/daypack-dev/timere/issues/26)

  - Renamed `Timere.Date_time.to_weekday` to `Timere.Date_time.weekday`

  - Unified `Duration` and `Span` module. Now there is only `Span` module with human friendly constructors
    and `view` type in `Timere.Span.For_human`

    - See [issue #27](https://github.com/daypack-dev/timere/issues/27) for discussion

  - Many changes and restructuring for better usability...

## Timere 0.3.1

- Minor refactoring on use of pattern resolver to simplify reasoning about undefined/uncertain behaviour

- Added `Timere.Span.make_small`

## Timere 0.3.0 (unreleased)

- Added support for parsing "24:00" and "24:00:00" in `Timere.Date_time.of_iso8601`

- Changed `make_hms` to replace second with 59 when 60 is provided

- Renamed `Date_time.make_precise` to `Date_time.make_unambiguous`

- Added leap second handling to `Date_time`

- Fixed crash in `Timere.Date_time.of_iso8601` due to failed date time construction

- Upgraded `Time_zone.make_offset_only` API

- Updated `tz_info` and `Time_zone.t` definition to remove overlap of functionality for representing
  time zone with only constant offset

- Renamed constants `min` and `max` to `min_val` and `max_val` for `Timere.Timestamp` and `Timere.Date_time`

- Added `since` and `since_timestamp`

- Pattern resolver overhaul

- Resolver search space slicing fix in

  - `slice_search_space`

  - `overapproximate_search_space_bottom_up`
  
  - `restrict_search_space_top_down`

- Changed `{mday:...}` to `{day:...}` in format string system

- Better error messages when local time zone cannot be determined ([PR #23](https://github.com/daypack-dev/timere/pull/23))

## Timere 0.2.2

- Moved timestamp functions into `Timere.Timestamp` module

- Added fractional second support to ISO8601 parsing

- Fixed `Span.of_float` handling of negative floats

- Renamed label argument `precision` to `frac_s` for RFC3339 related functions

- Removed rounding behaviour of fractional digits for RFC3339 related functions

- Changed `make_hms` to accept second field to be 60

## Timere 0.2.1

- Fixed OCaml 4.06.1 compatibility

## Timere 0.2.0 (unreleased)

- Added support for nanosecond precision for date times and timestamps

- Updated tzdb

- Fixed `Timere.Date_time.make` and `Timere.Date_time.make_exn` types

- `Timere.Date_time` API tuning

- Extended format string to support number form of month

- Extended format string to support fractional seconds

- Formatted date time string construction now fails explicitly when tz offset is required but cannot be deduced

## Timere-parse 0.0.4

- Replaced `Int` module with `CCInt` for building in version 4.06.1

## Timere-parse 0.0.3

- Ruleset update

- Added time zone support

- Improved bounded interval bound choosing (e.g. "10am to 2pm" would use a bound of 2 days now instead of 366 days)

- Fixed parsing of hmss

## Timere 0.1.5

- Fixed following functions which may exception when `tzlocal.none` backend is used
 
  - `Timere.intervals`

  - `Timere.resolve`

- Added `tzlocal.utc` backend

- Fixed slowdowns in `inter` resolution caused by incorrect batch and search space slicing logic

- Fixed `sexp_of_timestamp` which previously did not specify time zone explicitly as UTC after the default time zone parameter change in API

- Fixed use of `Date_time'.of_timestamp` at places by specifying the time zone explicitly as UTC

- Updated `bounded_intervals` to try to avoid constructing terms that result in (almost) non-termination

## Timere-parse 0.0.2

- Added corpus

- General parser upgrade

- Added parser for `hms`

## Timere 0.1.4

- Added missing exception handling in `Time.pattern` for `Range.Range_is_invalid`

- Added local time zone detection (`Timere.Time_zone.local`)

- Swapped to using local time zone by default in `Timere.Date_time` API

- Added missing to/of sexp functions for `Date_time` and `Duration` modules

- Added pretty printers for `Timere.hms`

## Timere-parse 0.0.1

- Base version

## Timere 0.1.3

- Base version
