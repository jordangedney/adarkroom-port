# frozen_string_literal: true

class AdarkroomPort < Formula
  desc "Terminal port of A Dark Room (Haskell + brick)"
  homepage "https://github.com/jordangedney/adarkroom-port"
  license "MPL-2.0"
  head "https://github.com/jordangedney/adarkroom-port.git", branch: "master"

  depends_on "cabal-install" => :build

  def install
    system "cabal", "v2-update"
    system "cabal", "v2-install", "--installdir=#{bin}",
           "--install-method=copy",
           "--overwrite-policy=always"
  end

  test do
    # The binary is a Brick TUI with no --help/--version flag, so it
    # can't be exercised non-interactively. At minimum, verify the
    # install produced a runnable executable.
    assert_predicate bin/"adarkroom-port", :executable?
  end
end
