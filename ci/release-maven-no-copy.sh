#!/usr/bin/env bash

set -eux

mill mill.scalalib.PublishModule/publishAll \
    --sonatypeCreds $SONATYPE_USERNAME:$SONATYPE_PASSWORD \
    --gpgArgs --passphrase,$GPG_PASSWORD,--batch,--yes,-a,-b \
    --publishArtifacts __.publishArtifacts \
    --readTimeout 600000 \
    --release true \
    --signed true