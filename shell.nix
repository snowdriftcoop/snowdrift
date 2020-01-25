with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/83b35508c6491103cd16a796758e07417a28698b.tar.gz) {};
let ghc = haskell.compiler.ghc802;
in haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ postgresql gmp libpqxx gnumake openssl zlib.dev zlib.out ];
}