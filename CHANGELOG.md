# Revision history for ghc-trace-events

## v0.1.2.2 - 2021-02-26

* Support GHC 9.0.1 ([#5](https://github.com/maoe/ghc-trace-events/pull/5))
* Switch from Travis CI to GitHub Actions
* Switch from criterion to tasty-bench


## v0.1.2.1 - 2020-06-09

* Delete some haddock comments that are not applicable to this library

## v0.1.2 - 2020-06-09

* Revert "Use unsafeUseAsCString in place of useAsCString in Debug.Trace.Text" because it's unsafe
* Minimize the scope of NOINLINE to get better performance when user tracing is disabled

## v0.1.1 - 2020-06-07 (abondoned)

* Remove redundant checks of userTracingEnabled
* Optimize Debug.Trace.Text

## v0.1.0.1 - 2020-04-06

* Support GHC 8.10.1
* Minor documentation updates

## v0.1.0 - 2019-11-20

* Add traceBinaryEvent and traceBinaryEventIO
* Switch to cabal-version: 2.2
* Rename Debug.Trace.Internal to Debug.Trace.Flags
* Test with GHC 8.8.1 and 8.6.5
* Update docs

## v0.0.0.1 - 2018-08-07

* Test with GHC 8.4.3 and HEAD
* Update docs

## v0.0.0 - 2018-05-21

* First version
