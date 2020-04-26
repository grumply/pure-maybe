{ mkDerivation, base, pure-elm, pure-default, stdenv }:
mkDerivation {
  pname = "pure-maybe";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm pure-default ];
  homepage = "github.com/grumply/pure-maybe";
  description = "Suspense-ful Maybe viewer";
  license = stdenv.lib.licenses.bsd3;
}
