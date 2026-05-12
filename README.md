# A Dark Room (terminal port)

a minimalist text adventure game for your terminal

---

## Install

### Homebrew (macOS / Linuxbrew)

Once the tap is published at `jordangedney/homebrew-tap`:

```
brew tap jordangedney/tap
brew install --HEAD adarkroom-port
adarkroom-port
```

Or directly from this repo, without the tap:

```
brew install --HEAD --build-from-source ./packaging/homebrew/adarkroom-port.rb
```

The canonical formula lives in [`packaging/homebrew/`](packaging/homebrew/);
see that directory's README for the publishing workflow and the
tagged-release follow-up.

### From source

```
cabal v2-update
cabal v2-install --installdir=$HOME/.local/bin
```

On Debian/Ubuntu you may also need `libtinfo-dev`:

```
sudo apt-get install libtinfo-dev
```

---

A very much WIP terminal port of A Dark Room, which can be found here:

https://github.com/doublespeakgames/adarkroom

--

![](example.png)
