with import <nixpkgs> {};

let
  jdk = temurin-jre-bin-17;

in mkShell {
  name = "Doc Generator Back dev env";

  shellHook = ''
      export JAVA_HOME=${jdk}
  '';

  packages = [
    cacert
    jdk
    (sbt.override { jre = jdk; })
  ];
}
