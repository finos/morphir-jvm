#!/usr/bin/env bash

set -eux

#echo $GPG_PRIVATE_KEY_B64 | base64 --decode > gpg_key
echo $GPG_SECRET > gpg_key

gpg --import gpg_key

rm gpg_key

mill.scalalib.PublishModule/publishAll \
    --sonatypeCreds lihaoyi:$SONATYPE_PASSWORD \
    --gpgArgs --passphrase,$GPG_PASSWORD,--batch,--yes,-a,-b \
    --publishArtifacts __.publishArtifacts \
    --readTimeout 600000 \
    --release true \
    --signed true