help-esb.hs
===========

A Haskell client for our ESB.

It's at `v0.0.1` Because it's still catching up to the Node.js one.

## Installation
1. Install the [haskell-platform](https://www.haskell.org/platform).
2. `cabal install missingh`
3. `cabal install aeson`
4. Import `EsbClient` into your microservice and go!

## test.hs
Included is a [test implementation](test/test.hs) that simply logs into the ESB, verifies the login,
then listens and logs out raw input from its subscriptions.

To run it for yourself do:

1. `cd test`
2. `ghc -i../ test.hs`
3. `./test`
