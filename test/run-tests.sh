#!/bin/sh
# Run the dired-image-thumbnail ERT suite in batch mode.
set -e
DIR="$(cd "$(dirname "$0")" && pwd)"
emacs -Q --batch -L "$DIR/.." \
      -l "$DIR/dired-image-thumbnail-test.el" \
      -f ert-run-tests-batch-and-exit