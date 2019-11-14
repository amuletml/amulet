#!/usr/bin/env bash

set -e
export GPG_TTY=$(tty)

ls result/* | while read -r file; do
  printf "==> Signining %s\n" $file

  test -r $file || ( echo "Can't read $file"; exit 1 )

  echo $SIGNING_KEY_PW | \
    gpg --batch --passphrase-fd 0 --detach-sign \
        --pinentry-mode loopback \
        --armor $file

  echo $SIGNING_KEY_PW | \
    gpg --batch --passphrase-fd 0 --detach-sign \
        --pinentry-mode loopback \
        $file
done
