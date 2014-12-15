# Hadley

Hadley renders static pages for Haskell projects.

Hadley is in development and can only render itself. Current pages include:

- A README file rendered using Pandoc,
- The raw README file,
- The output of HLint when run over Hadley's script.

## Docker images

To build Hadley, I am using a Docker image. A Dockerfile is provided in this
repository. The image depends on the [Reesd stack
image](https://github.com/noteed/reesd-stack) and can be built with

    > docker build -t images.reesd.com/reesd/stack-pandoc images/stack-pandoc

(Rename the image and the `FROM` instruction as appropriate.)

## 80 columns

    123456789 123456789 123456789 123456789 123456789 123456789 123456789 1234567890
