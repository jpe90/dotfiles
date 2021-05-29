{ pkgs, ... }:

let
  extensions = (with pkgs.vscode-extensions; [
      bbenoist.Nix
      ms-python.python
      ms-azuretools.vscode-docker
      ms-vscode-remote.remote-ssh
    ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
    name = "dart-code";
    publisher = "Dart-Code";
    version = "3.22.0";
    sha256 = "121mlnhk2v2idr0zx76xn7hbdaz3dkxi0x8lfpbidbx5b70xwx6n";
  }
  {
    name = "flutter";
    publisher = "Dart-Code";
    version = "3.22.0";
    sha256 = "1kn6s7hbjj2y1kk7xk269mk9rll794wc1amca7v56njfm0vs1362";
  }
  {
    name = "haskell";
    publisher = "haskell";
    version = "1.4.0";
    sha256 = "1jk702fd0b0aqfryixpiy6sc8njzd1brd0lbkdhcifp0qlbdwki0";
  }
  {
    name = "language-haskell";
    publisher = "justusadam";
    version = "3.4.0";
    sha256 = "0ab7m5jzxakjxaiwmg0jcck53vnn183589bbxh3iiylkpicrv67y";
  }

];
  vscode-with-extensions = pkgs.vscode-with-extensions.override {
      vscodeExtensions = extensions;
    };
in {
  config = {
    environment.systemPackages = [
      vscode-with-extensions
    ];
  };
}
