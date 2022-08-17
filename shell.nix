# Use this file with nix-shell or similar tools; see https://nixos.org/
with import <nixpkgs> {};

mkShell {
  buildInputs = [
    futhark
    pkgconfig
    SDL2
    SDL2_ttf
    opencl-headers
    ocl-icd
    xxd
  ];
}
