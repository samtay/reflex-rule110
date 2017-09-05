#!/usr/bin/env bash
set -eo pipefail

main() {
    errEcho "Building via ghcjs..."
    cabal configure --ghcjs
    cabal build

    errEcho "Cloning gh-pages into tempdir..."
    tmpdir=$(mktemp -d)
    git clone --single-branch --branch gh-pages \
        git@github.com:samtay/reflex-rule110.git \
        $tmpdir

    errEcho "Pushing last dist/build to gh-pages branch..."
    cp -R dist/build/rule110/rule110.jsexe/* $tmpdir
    cd $tmpdir
    git add .
    git commit -m 'Update ghcjs build output'
    git push

    errEcho "Done!"
}

errEcho() {
    CYAN='\033[1;36m'
    CLEAR='\033[0m'
    >&2 echo -e "${CYAN}""$@""${CLEAR}"
}

main "$@"
