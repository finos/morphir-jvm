#!/usr/bin/env bash

set -eux

mill mill.javalib.SonatypeCentralPublishModule/publishAll \
    --gpgArgs --passphrase,$GPG_PASSWORD,--batch,--yes,-a,-b \
    --publishArtifacts __.publishArtifacts \
    --readTimeout 600000 \
    --release true \
    --signed true