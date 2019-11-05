with {
  rev = "9fe4e068123146afda5f2b9a6d630ba76d1daff5";
  sha256 = "1fwjf3gvhinzs9yvp7dh3im0123lbzhfrg371ingp61n7jkb73c9";

  config = {
    allowUnfree = true;
  };

  overlays = [];
};

import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
}) { inherit config overlays; }
