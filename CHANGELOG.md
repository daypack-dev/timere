# Changelog

## Timere-parse 0.0.5

- Updated use of Timere API

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
