let pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "smlfut";
  buildInputs = [
    pkgs.mlton
    pkgs.mlkit
    pkgs.smlfmt
    pkgs.smlpkg
  ];
}
