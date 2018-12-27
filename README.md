# hal

A runtime environment for [Haskell] applications running on [AWS Lambda].

#### Flexible

This library uniquely supports different types of AWS Lambda Handlers for your needs/comfort with advanced Haskell.
Instead of exposing a single function that constructs a Lambda, this library exposes many.

For lambdas that are pure and safe, then `pureRuntime` is ideal.
It accepts a handler with the signature `(FromJSON a, ToJSON b) => a -> b`.
This runtime guarantees that side-effects cannot occur.

For advanced use cases `mRuntimeWithContext` unlocks the full power of Monad Transformers.
It accepts handlers with the signature `(HasLambdaContext r, MonadCatch m, MonadReader r m, MonadIO m, FromJSON event, ToJSON result) =>  (event -> m result)`
This enables users to add caching logic or expose complex environments.

With numerous options in between these two, developers can choose the right balance of flexibility vs simplicity.

#### Performant

Measuring lambda performance is tricky, so investigation and optimization is ongoing.
Current indications show a _warm_ execution overhead of only ~20% more than the official [Rust Runtime] (a much lower level language).

#### Robust

While testing continues, we have executed over 30k test events without error caused by the runtime.
Naive approaches lead to error rates well over 10%.

## Table of Contents

  - [Quick Start](#quick-start)
  - [Usage](#usage)
  - [Local Testing](#local-testing)

## Quick Start

This quick start assumes you have the following tools installed:

  - [Stack][stack.yaml]
  - [Docker]
  - [aws-cli]

Add `aws-lambda-runtime` to your [stack.yaml]'s [`extra-deps`] and enable
[Docker] integration so that your binary is automatically compiled in a
compatible environment for AWS. Also add `aws-lambda-runtime` to your project's
dependency list (either `project-name.cabal` or `package.yaml`)

```yaml
#...
packages:
  - '.'
  - aws-lambda-runtime-0.1.0
# ...
docker:
  enable: true
# ...
```

Then, define your types and handler:

```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import AWS.Lambda.Runtime (simpleLambdaRuntime)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Request = Request {
  input :: String
} deriving (Generic)

instance FromJSON Request

data Response = Response {
  output :: String
} deriving (Generic)

instance ToJSON Response

idHandler :: Request -> Response
idHandler Request { input } = Response { output = input }

main :: IO ()
main = simpleLambdaRuntime idHandler
```

Don't forget to define your [CloudFormation] stack:

```yaml
# file: template.yaml
AWSTemplateFormatVersion: '2010-09-09'
Transform: 'AWS::Serverless-2016-10-31'
Description: Test for the Haskell Runtime.
Resources:
  HelloWorldApp:
    Type: 'AWS::Serverless::Function'
    Properties:
      Handler: NOT_USED
      Runtime: provided
      CodeUri: .stack-work/docker/_home/.local/bin/
      Description: My Haskell runtime.
      MemorySize: 128
      Timeout: 3
```

Finally, build, upload and test your lambda!

```bash
# Build the binary, make sure your executable is named `bootstrap`
stack build --copy-bins

# Create your function package
aws cloudformation package \
  --template-file template.yaml
  --s3-bucket your-existing-bucket > \
  deployment_stack.yaml

# Deploy your function
aws cloudformation deploy \
  --stack-name "hello-world-haskell" \
  --region us-west-2 \
  --capabilities CAPABILITY_IAM \
  --template-file deployment_stack.yaml

# Take it for a spin!
aws lambda invoke \
  --function-name your-function-name \
  --region us-west-2
  --payload '{"input": "foo"}'
  output.txt
```

## Usage

TODO

## Local Testing

### Dependencies

  - [Stack][stack.yaml]
  - [Docker]
  - [aws-sam-cli] (>v0.8.0)

### Build

```bash
docker pull fpco/stack-build:lts-12.21 #first build only
stack build --copy-bins
```

### Execute

```bash
echo '{ "accountId": "byebye" }' | sam local invoke --region us-east-1
```

[AWS Lambda]: https://docs.aws.amazon.com/lambda/latest/dg/welcome.html
[Haskell]: https://www.haskell.org/
[stack.yaml]: https://docs.haskellstack.org/
[`extra-deps`]: https://docs.haskellstack.org/en/stable/yaml_configuration/#yaml-configuration
[Docker]: https://www.docker.com/why-docker
[aws-cli]: https://aws.amazon.com/cli/
[CloudFormation]: https://aws.amazon.com/cloudformation/
[aws-sam-cli]: https://github.com/awslabs/aws-sam-cli
[Rust Runtime]: https://github.com/awslabs/aws-lambda-rust-runtime
