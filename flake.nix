{
  nixConfig.bash-prompt = "[nix(moonracer)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/fe0dabfd8acf96f1b5cff55766de6284517868cf";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;

      haskellExtend = hpFinal: hpPrev: {
        moonracer = hpPrev.callCabal2nix "moonracer" self { };
        GLUT = pkgs.haskell.lib.doJailbreak hpPrev.GLUT;
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;

      baseTools = with pkgs; [ cabal-install hsPkgs.cabal-fmt hlint fourmolu ];
      exe = pkgs.haskell.lib.justStaticExecutables hsPkgs.moonracer;
    in {
      packages."x86_64-linux".default = exe;
      apps."x86_64-linux".default = {
        type = "app";
        program = let
          wrapper = pkgs.writeScriptBin "moonracer" ''
            #!/bin/sh
            exec ${pkgs.nixGLIntel}/bin/nixGLIntel ${exe}/bin/moonracer $*
          '';
        in "${wrapper}/bin/moonracer";
      };
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.moonracer ];
        buildInputs = with pkgs; [ ghcid haskell-language-server ] ++ baseTools;
      };
    };
}
