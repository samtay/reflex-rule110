#!/usr/bin/env bash
set -eo pipefail

main() {
    errEcho "Building via ghcjs..."
    cabal configure --ghcjs
    cabal build
    errEcho "Pushing last dist/build to gh-pages branch..."
    git checkout gh-pages
    cp -R dist/build/rule110/rule110.jsexe/* .
    git add .
    git commit -m 'Update ghcjs build output'
    git push
    errEcho "Checking master branch back out..."
    git checkout master
    errEcho "Done!"
}

errEcho() {
    YELLOW='\033[1;36m'
    CLEAR='\033[0m'
    >&2 echo -e "${YELLOW}""$@""${CLEAR}"
}

main "$@"
