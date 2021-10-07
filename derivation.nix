{ stdenv, ocaml, ocamlPackages }:

stdenv.mkDerivation {
  name = "pds-tp2-ocaml-etu";

  src = ./.;

  nativeBuildInputs = with ocamlPackages; [ ocaml camlp4 ocamlbuild ];

  installPhase = ''
    cp main.native $out
  '';
}
