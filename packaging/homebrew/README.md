# Homebrew packaging

This directory holds the **canonical Homebrew formula** for `adarkroom-port`.
The published tap at `jordangedney/homebrew-tap` mirrors this file — when
the formula changes here and lands on `master`, the overseer copies it
across.

## Installing from this repo

For local testing before the tap exists, or for hacking on the formula:

```
brew install --HEAD --build-from-source ./packaging/homebrew/adarkroom-port.rb
adarkroom-port
```

This builds from `master` via `cabal v2-install`. `cabal-install` (the
only declared build dependency) brings GHC with it.

## Installing via the published tap

Once the overseer has published `jordangedney/homebrew-tap`:

```
brew tap jordangedney/tap
brew install --HEAD adarkroom-port
adarkroom-port
```

The `--HEAD` flag is required until a tagged release exists — see
"Tagged releases" below.

## Publishing the tap (overseer only)

1. Create `jordangedney/homebrew-tap` on GitHub (the repo name **must**
   start with `homebrew-`; `brew tap jordangedney/tap` resolves to it).
2. Add a `Formula/` directory at the tap repo root.
3. Copy this file across:
   ```
   cp packaging/homebrew/adarkroom-port.rb \
      /path/to/homebrew-tap/Formula/adarkroom-port.rb
   ```
4. Commit and push.

Users can then run the `brew tap` / `brew install` commands above.

## Tagged releases (future)

The formula is currently HEAD-only because no version tag exists yet.
When `v0.1.0` is cut:

1. Bump `version` in `adarkroom-port.cabal`, update `CHANGELOG.md`.
2. Tag the commit and push: `git tag v0.1.0 && git push origin v0.1.0`.
3. Compute the tarball sha256:
   ```
   curl -sL https://github.com/jordangedney/adarkroom-port/archive/refs/tags/v0.1.0.tar.gz \
     | shasum -a 256
   ```
4. Add `url` and `sha256` lines to the formula (keep `head` so
   `--HEAD` installs still work):
   ```ruby
   url "https://github.com/jordangedney/adarkroom-port/archive/refs/tags/v0.1.0.tar.gz"
   sha256 "<digest from step 3>"
   ```
5. Re-publish the formula to the tap. Users can drop `--HEAD`.

This is tracked as a follow-up; it isn't part of the initial packaging
work.

## Verifying changes locally

Before committing a formula change, run:

```
brew audit --new-formula --strict ./packaging/homebrew/adarkroom-port.rb
brew install --HEAD --build-from-source ./packaging/homebrew/adarkroom-port.rb
brew test adarkroom-port
```

The source build is slow (GHC compilation); expect tens of minutes on
first run.
