# Timere
OCaml time reasoning library with platform independent time zone support and NLP-style parsing

__Note__: The project is largely complete, but still undergoing testing - you're welcome to use it in prototypes, but avoid using it in production for now

## Introduction

Timere (short for time reasoning) is a comprehensive library for handling time (timestamps/date times) which aims to make things often not straightforward to solve easily accessible

This includes

- Platform independent time zone aware date times (the IANA time zone database is built into this library)

- Efficient time query resolution

  - A time query (or a `timere` object) is an expression language that represent sequence of time intervals, that can be further combined or modified via use of various combinators, e.g.

    - Intersection and union

    - Pattern matching of time (e.g. day is 23th, hour is 9am)

      - This is time zone aware and works across DST boundaries

    - Chunking at year or month boundary, or in fixed sizes

    - Evaluate expressions with a different time zone (e.g. intersection of 9am to 5pm of Australia and 9am to 5pm of New York)

  - Time query can thus be used as a robust solution for various applications, e.g.

    - Part of your scheduling process (e.g. 9am to 10am time intervals between two dates)

    - Obtaining explicit time intervals from a natural representation to be used for log analysis

## Usage

Include `timere` in the `libraries` in your dune file, and optionally one of the following two to select
a concrete implementation of time zone data source

- `timere.tz.full`

  - This is the default implementation which embeds the full time zone database

- `timere.tz.none`

  - This embeds no database. This is suitable for when you want to provide your own time zone data sourcing method during run time, and want to reduce the built artifact size.
  
  - The following resources should allow you to construct your own sourcing method
  
    - A usable and test suite covered data source is provided under `tzdb-json/`.
    
    - List of available time zones is available as `gen_artifacts/available-time-zones.txt`

    - A utility JSON string parsing function is available as `Timere.Utils.tz_table_of_json_string`

[API documentation](https://daypack-dev.github.io/timere/)

## License

Code files are licensed under the MIT license as specified in the `LICENSE` file

Time zone database derived files are licensed under its original terms (public domain)

## Acknowledgement

- Time zone information is extracted via `zdump` command output into `src/time_zone_data.ml`, using the IANA database (as time zone files) distributed on Linux

- Time zone data handling code copies approach used by [chrono-tz](https://github.com/chronotope/chrono-tz)

  - This includes data representation and choices of some algorithms
