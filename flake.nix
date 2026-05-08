{
  description = "Ambiente de desenvolvimento Haskell (GHC 9.8, cabal, HLS) para NixOS 25.05";

  # Nixpkgs da release 25.05 compatível com seu NixOS
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";

    # Importa nixpkgs fixado
    pkgs = import nixpkgs {
      inherit system;
    };
  in {

    # Ambiente de desenvolvimento
    devShells.${system}.default = pkgs.mkShell {

      # O que estará disponível no shell
      buildInputs = with pkgs; [
        ghc                     # GHC 9.8
        cabal-install           # Cabal
        haskell-language-server # HLS compatível
        # pkgs.pkg-config       # essencial p/ libs C
        zlib                    # libs C comuns
        # pkgs.openssl          # libs C frequentes
      ];

      # Variáveis úteis
      shellHook = ''
        export OLD_PS1="$PS1"
        export PS1="$PS1 (haskell)"
        ghc --version
        trap 'export PS1="$OLD_PS1"' EXIT
      '';
    };
  };
}

