# Change log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][chg] and this project adheres to
[Haskell's Package Versioning Policy][pvp]

## `0.4.8` - unreleased

  - Add `ToJSON` instances for `ProxyRequest` types
  - Add `FromJSON` instance for `ProxyResponse`

## `0.4.7` - 2021-04-12

  - Drop http-conduit as a dependency for potentially smaller binaries
  - Add more detailed error messages to context parsing failures
  - Increase compatibility by loosening envy version bounds
  - Add mRuntimeWithContext' and mRuntime
  - Deprecate withIoInterface, withFallibleInterface, withPureInterface, HasLambdaContext, runRaderTLambdaContext, mRuntimeWithContext

## `0.4.2` - 2020-07-23

  - Fix an issue where errors from the runtime were not retried

## `0.4.1` - 2020-01-22

  - Constrain the envy version for correctly building with Cabal
  - Fix documentation examples for Runtime and Combinators

## `0.4.0` - 2020-01-22

  - Add support for Value-based runtimes for handling JSON conversion errors explicitly

## `0.3.1` - 2020-07-23

  - Fix an issue where errors from the runtime were not retried

## `0.3.0` - 2019-02-26

  - Add support for triggered S3 events

## `0.2.1` - 2020-07-23

  - Fix an issue where errors from the runtime were not retried

## `0.2.0` - 2019-02-07

  - Expose `AWS.Lambda.Combinators` package for building other runtimes
  (e.g. to support Lambda Event triggers)

## `0.1.3` - 2020-07-23

  - Fix an issue where errors from the runtime were not retried

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
