# Changelog

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
