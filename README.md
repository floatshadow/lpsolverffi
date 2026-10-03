On Debian 13 (trixie), install the development packages for the solvers you need:

```sh
sudo apt install build-essential pkg-config coinor-libclp-dev libhighs-dev libglpk-dev
```

`dune build` automatically detects and enables the installed solver libraries.
