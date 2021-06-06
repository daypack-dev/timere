# Timere

OCaml date time handling and reasoning suite

This repo houses:

- Timedesc - date time handling library

- Timere - date time reasoning library

- Timere-parse - natural language parsing library

[API documentation](https://daypack-dev.github.io/timere/)

__Note__: The project core is largely complete, but still undergoing testing - you're welcome to use it in prototypes, but avoid using it in production for now. The NLP component is WIP.

__Disclaimer__: Timere is not designed to handle prehistoric events. For prehistoric planning and booking software, please consult [appropriate experts](https://en.wikipedia.org/wiki/The_Flintstones).

## Examples

Christmases which fall on Wednesday from now

```ocaml
let () =
  let open Timere in
  match
    resolve (
      since (Timedesc.now ())
      &&& months [12]
      &&& days [25]
      &&& weekdays [`Wed]
    )
  with
  | Error msg -> failwith msg
  | Ok s ->
    Fmt.pr "%a@." (Timedesc.Interval.pp_seq ~sep:(Fmt.any "@.") ()) s
```

gives

```
[2024 Dec 25 00:00:00 +00:00:00, 2024 Dec 26 00:00:00 +00:00:00)
[2030 Dec 25 00:00:00 +00:00:00, 2030 Dec 26 00:00:00 +00:00:00)
[2041 Dec 25 00:00:00 +00:00:00, 2041 Dec 26 00:00:00 +00:00:00)
[2047 Dec 25 00:00:00 +00:00:00, 2047 Dec 26 00:00:00 +00:00:00)
[2052 Dec 25 00:00:00 +00:00:00, 2052 Dec 26 00:00:00 +00:00:00)
[2058 Dec 25 00:00:00 +00:00:00, 2058 Dec 26 00:00:00 +00:00:00)
...
```

See `examples/` for more examples

## Features

#### Timedesc

- Timestamp and date time handling with platform independent time zone support

  - Subset of the IANA time zone database is built into this library

- Supports Gregorian calendar date, ISO week date, and ISO ordinal date

- Supports nanosecond precision

- ISO8601 parsing and RFC3339 printing

#### Timere

- Reasoning over time intervals via `timere` objects/expressions, examples:

  - Pattern matching time and intervals. These work across DST boundaries.
  
  - Intersection and union

  - Chunking at year or month boundary, or in fixed sizes

  - Evaluate (sub)expressions with a different time zone (e.g. intersection of 9am to 5pm of Sydney and 9am to 5pm of New York)
    
## Basic usage

Include `timedesc` (`timere` and `timere-parse` if needed) in the `libraries` stanza in your dune file

#### Tzdb backend

You can optionally pick one of the following two concrete implementations of time zone data source

- `timedesc.tzdb.full`

  - This is the default implementation which embeds the IANA time zone database from year 1850 to year 2100 exclusive

- `timedesc.tzdb.none`

  - This embeds no database.
    This is suitable for when you want to retrieve time zone data during run time, for instance, to reduce the built artifact size.
  
  - The following resources should allow you to implement said approach readily
  
    - A usable and test suite covered data source is provided under `tzdb-json/`.
    
    - List of available time zones is available as `gen-artifacts/available-time-zones.txt`

    - `Time_zone.JSON.of_string` can load files in `tzdb-json/`

#### Tzlocal backend

You can optionally pick one of the following three concrete implementations of local time zone detection

- `timedesc.tzlocal.unix`

  - This is the default implementation which tries to look up info of OS for local time zone name. Should work for common Linux distros.

- `timedesc.tzlocal.none`

  - This simply returns no time zone guesses

- `timedesc.tzlocal.utc`

  - This simply returns UTC as the only guess

__Note__: While `tzdb-json/` may be useful and usable outside of Timere,
we make no guarantees that the JSON format stays unmodified
(though changes of the format should be a rare occurrence, if ever occurring)

## Advanced usage

#### Using Timedesc in utop

To use Timedesc in utop, you need to pick the backends explicitly first

For example, to use the default implementations, one can first type the following lines in utop

```
#require "timedesc.tzdb.full";;
#require "timedesc.tzlocal.unix";;
#require "timedesc";;
```

then `Timedesc` should be accessible in utop

#### Building a custom Tzdb backend

Timedesc itself can help build a time zone data source backend.

TODO

## Changelog

See [here](CHANGELOG.md)

## License

Code files are licensed under the MIT license as specified in the `LICENSE` file

Time zone database derived files are licensed under its original terms (public domain)

## Acknowledgement

- Time zone information is extracted via `zdump` command output into `gen-artifacts/time_zone_db.sexp`, using the IANA database (as time zone files) distributed on Linux

- Time zone data handling code copies approach used by [chrono-tz](https://github.com/chronotope/chrono-tz)

  - This includes data representation and choices of some algorithms

- Local time zone detection approach for `timedesc.tzlocal.unix` backend is copied from [tzlocal](https://github.com/regebro/tzlocal)

- Some internal calculations/formulas in Timedesc based around Julian day are copied from [Ptime](https://github.com/dbuenzli/ptime)
