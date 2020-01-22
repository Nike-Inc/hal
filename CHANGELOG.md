# Change log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][chg] and this project adheres to
[Haskell's Package Versioning Policy][pvp]

## `0.4.0` - 2019-02-26

  - Add support for Value-based runtimes for handling JSON conversion errors explicitly

## `0.3.0` - 2019-02-26

  - Add support for triggered S3 events

## `0.2.0` - 2019-02-07

  - Expose `AWS.Lambda.Combinators` package for building other runtimes
  (e.g. to support Lambda Event triggers)

## `0.1.2` - 2019-01-10

  - Fix PVP bounds when building/uploading the package

## `0.1.1` - 2019-01-08

### Fixed

  - Incorrect error message when getting events from AWS

## `0.1.0` - 2018-12-20

### Added

  - Initial release!

[chg]: http://keepachangelog.com
[pvp]: http://pvp.haskell.org
