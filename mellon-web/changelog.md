## 0.8.0.3 (2018-01-11)

- Use hpack.

- Support for GHC 8.2.2.

- Bump swagger2, optparse-applicative, doctest, QuickCheck, hspec-wai
  upper bounds.

- Try to make the Mellon/Web/ClientSpec.hs test a little less
  time-sensitive.

- Maintainer-related changes (better Nix support, Makefile, etc.).

## 0.8.0.2 (2017-09-04)

- Fix .cabal file for now-removed Paw file.

## 0.8.0.1 (2017-09-04)

- Hackage compliance fix.

## 0.8.0.0 (2017-09-04)

- Remove out-of-date Paw file.

- Port to Servant 0.11. Due to Servant 0.11 API changes, our API
  has changed, as well (just the Haskell bits; the web API remains
  the same).

## 0.7.1.1 (2017-05-24)

No changes; copyright has been assigned to Quixoftic, LLC.

## 0.7.1.0 (2017-04-28)

- Now requires servant-client >= 0.9.

- Bump various dependency upper bounds.

## 0.7.0.3 (2016-09-23)

- Bump servant upper bounds.

## 0.7.0.2 (2016-09-23)

- Add an "--active-low" flag to `gpio-mellon-server` example.

## 0.7.0.1 (2016-06-13)

- Packaging fixes only.

## 0.7.0.0 (2016-06-02)

- Port to new `mellon-core` package.
- Fix Servant bitrot.
