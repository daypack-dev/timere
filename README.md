# Timere
OCaml time reasoning library with time zone support and NLP-style parsing 

## Introduction

Timere (short for time reasoning), is a comprehensive library for handling time (timestamps/date times), and aims to make things often not straightforward easily accessible

This includes

- Platform independent time zone aware date times (the IANA time zone database is built into this library)

- Efficient time query resolution

  - A time query (or a `timere` object) is an expression language that represent sequence of time intervals, that can be further combined or modified via use of various combinators

    - This includes

      - Intersection and union

      - Pattern matching of time (e.g. day is 23th, hour is 9am)

        - This is time zone aware and works across DST boundaries

      - Chunking at year or month boundary, or in fixed sizes

      - Evaluate expressions with a different time zone (e.g. intersection of 9am to 5pm of Australia and 9am to 5pm of New York)

  - Time query can thus be used as a robust solution for various applicationsas, e.g.

    - Part of your scheduling process (e.g. 9am to 10am time intervals between these two dates)

    - Finding out appropriate meeting time

## Usage

TODO

## Acknowledgement

- Time zone information is extracted via `zdump` command output into `src/time_zone_data.ml`

- Time zone data handling code copies approach used by [chrono-tz](https://github.com/chronotope/chrono-tz)

  - This includes data representation and choices of some algorithms
