#!/usr/bin/env bash

set -eux

mill mill.scalalib.SonatypeCentralPublishModule/publishAll \
    --publishArtifacts __.publishArtifacts \
    --readTimeout 600000 \
    --release true \
    --signed true