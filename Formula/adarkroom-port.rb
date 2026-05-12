# frozen_string_literal: true

class AdarkroomPort < Formula
  desc "Terminal port of A Dark Room (Haskell + brick)"
  homepage "https://github.com/jordangedney/adarkroom-port"
  url "https://github.com/jordangedney/adarkroom-port/archive/refs/tags/v0.1.0.tar.gz"
  # After tagging v0.1.0, replace with:
  #   curl -sL https://github.com/jordangedney/adarkroom-port/archive/refs/tags/v0.1.0.tar.gz | shasum -a 256
  sha256 "0000000000000000000000000000000000000000000000000000000000000000"
  license "MPL-2.0"
  head "https://github.com/jordangedney/adarkroom-port.git", branch: "master"

  depends_on "cabal-install" => :build
  # Pinned to the GHC series the project is known to build against
  # (see commit 97f71d9 "fix build for ghc-9.10 / brick-2.10 / vty-6").
  # Bump in lockstep with the .cabal file's `base` bound when upgrading.
  depends_on "ghc@9.10" => :build

  def install
    system "cabal", "v2-update"
    system "cabal", "v2-install", "--installdir=#{bin}",
           "--install-method=copy",
           "--overwrite-policy=always"
  end

  test do
    # The binary is a Brick TUI with no --help/--version flag, so we
    # cannot exercise it from `brew test` (no controlling terminal).
    # At minimum, verify the install produced a runnable executable.
    assert_predicate bin/"adarkroom-port", :executable?
  end
end
