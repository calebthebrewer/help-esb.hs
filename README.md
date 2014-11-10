help-esb.hs
===========

A Haskell client for our ESB.

## Installation
1. Install the [haskell-platform](https://www.haskell.org/platform).
2. `cabal install missingh aeson uuid`
3. Import `HelpEsbClient` into your microservice and go!

## examples/Basic.hs
Included is a [basic implementation](examples/Basic.hs) that simply logs into the ESB, verifies the login,
then listens and logs out raw input from its subscriptions.

To run it for yourself do:

1. `cd examples`
2. `ghc -i../ Basic.hs`
3. `./Basic`
