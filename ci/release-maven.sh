#!/usr/bin/env bash

set -eu

echo "$PGP_SECRET" | base64 --decode > gpg_key

gpg --import  --no-tty --batch --yes gpg_key

rm gpg_key

# Build all artifacts
./mill -i __.publishArtifacts

# Publish all artifacts
./mill -i \
    mill.scalalib.PublishModule/publishAll \
    --sonatypeCreds "$SONATYPE_USER":"$SONATYPE_PASSWORD" \
    --gpgArgs --passphrase="$PGP_PASSWORD",--no-tty,--pinentry-mode,loopback,--batch,--yes,-a,-b \
    --publishArtifacts __.publishArtifacts \
    --readTimeout  3600000 \
    --awaitTimeout 3600000 \
    --release true \
    --signed  true \
    --sonatypeUri https://s01.oss.sonatype.org/service/local \
    --sonatypeSnapshotUri https://s01.oss.sonatype.org/content/repositories/snapshots
