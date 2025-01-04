# Hadley

Hadley renders static pages for Haskell projects.

Hadley is in development and can only render itself. Current pages include:

- The README file rendered using Pandoc,
- The Haskell code,
- The output of HLint when run over Hadley's script,
- The Cabal file,
- Processes output (in particular `cabal build`).


## Building

```
$ nix-build release.nix
```


## Running

```
$ ./generate.sh
```

The files are generated in the `_site/` directory.


## 80 columns

    56789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890
