#! /bin/bash

export SCALA_VERSION=2.13.2

# Build the morphir JAR
./mill "morphir.cli.jvm[${SCALA_VERSION}].assembly"

java -jar out/morphir/cli/jvm/${SCALA_VERSION}/assembly/dest/out.jar "$@"
