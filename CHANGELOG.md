# Change log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][chg] and this project adheres to
[Haskell's Package Versioning Policy][pvp]

## unreleased - 2023-11-23

  - Add support for aeson 2.2
  - `fallibleRuntime` and `fallibleRuntimeWithContext` reports errors to AWS
    instead of returning `{"Left": "some error"}`.
  - `fallibleRuntime` and `fallibleRuntimeWithContext` no longer wrap their
    successful responses in `{"Right": ...}`
  - `pureRuntime`, `pureRuntimeWithContext`, `fallibleRuntime` and `fallibleRuntimeWithContext`
    no longer crash the Lambda runtime if input can't be parsed from JSON.

## `1.0.0.1` - 2022-09-10

  - Support GHC 9.4 by eliminating compiler errors

## `1.0.0` - 2022-05-21

  - Remove deprecated functions and classes:
    - `AWS.Lambda.Combinators.withFallibleInterface`
    - `AWS.Lambda.Combinators.withIOInterface`
    - `AWS.Lambda.Combinators.withPureInterface`
    - `AWS.Lambda.Context.HasLambdaContext` (`class`)
    - `AWS.Lambda.Context.runReaderTLambdaContext`
    - `AWS.Lambda.Runtime.Value.mRuntimeWithContext`

  - Rename `mRuntimeWithContext'` to `mRuntimeWithContext` in the following modules:
    - `AWS.Lambda.Runtime`
    - `AWS.Lambda.Runtime.Value`

  - Remove dependency on `envy`

## `0.4.10.1` - 2022-03-28

  - `instance FromJSON AWS.Lambda.Events.EventBridge.Detail.SSM.ParameterStoreChange.Operation`
    now accepts arbitrary `Text` instead of the four currently-known
    operations.

## `0.4.10` - 2022-03-22

  - Add `AWS.Lambda.Events.EventBridge.EventBridgeEvent` type for
    subscribing Lambda functions to AWS EventBridge Events
  - Add `AWS.Lambda.Events.EventBridge.Detail.SSM.ParameterStoreChange`
    type for parsing AWS Systems Manager Parameter Store Change events
    delivered via AWS EventBridge.
  - When the runtime encounters an error, write it out to the Cloudwatch logs
    (via stderr), and do so in a format that can be guaranteed and later
    extended.
  - Eliminate redundant import compiler warnings

## `0.4.9` - 2022-02-28

  - Add `KafkaEvent` type for subscribing Lambda functions to Kafka
    topics
  - Support `aeson ^>=2.0.0.0`
  - Drop official support for GHC 8.0.2
  - Expand hedgehog bounds to include 1.1 to keep ahead of Stack nightly

## `0.4.8` - 2021-05-07

  - Add `ToJSON` instances for `ProxyRequest` types, and test
    round-tripping as much as possible
  - Add `FromJSON` instance for `ProxyResponse`, and test
    round-tripping as much as possible

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
