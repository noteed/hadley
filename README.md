# Hadley

Hadley renders static pages for Haskell projects.

Hadley is in development and can only render itself. Current pages include:

- The README file rendered using Pandoc,
- The Haskell code,
- The output of HLint when run over Hadley's script,
- The Cabal file,
- Processes output (in particular `cabal build`).

## Docker images

To build Hadley, I am using a Docker image. A Dockerfile is provided in this
repository. The image depends on the [Reesd stack
image](https://github.com/noteed/reesd-stack) and can be built with

    > docker build -t images.reesd.com/reesd/stack-pandoc images/stack-pandoc

(Rename the image and the `FROM` instruction as appropriate.)

Hadley itself is also packaged as a Docker image. The Dockerfile context is
`images/hadley`. See the `run-image.sh` script to see how to use it. To build
the image, it is easier to use the provided `Makefile` as some files must be
generated ant copied to the context.

## 80 columns

    123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890
