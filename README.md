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


## Docker images

Hadley itself and the build process use Docker images.

The build image context is `images/stack-pandoc`, which itself depends on
[Reesd stack image](https://github.com/noteed/reesd-stack). It can be built
with:

    > docker build -t images.reesd.com/reesd/stack-pandoc images/stack-pandoc

(Rename the image and the `FROM` instruction as appropriate.)

The Hadley image context is `images/hadley`. See the `run-image.sh` script to
see how to use it. To build the image, it is easier to use the provided
`Makefile` as some files must be generated and copied to the context.


## TODO

- It seems I have uploaded the image at https://hub.docker.com/r/noteed/hadley/
  two years ago and thus is not up-to-date.

- `noteed/hadley` is `FROM stack-pandoc` and should be leaner.


## 80 columns

    56789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890
