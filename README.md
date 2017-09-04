# rule 110

This is a small working example of showing the
[rule 110](https://en.wikipedia.org/wiki/Rule_110) computation via
[reflex](https://github.com/reflex-frp/reflex), an FRP library.

**Note**: I recently wrote a little Haskell implementation of rule 110 as an
interview challenge problem and told the potential employer that I would not
post the solution online. Hence, the `Rule` module is gitignored here.

### hacking
To play with this on your own:
```shell
# clone this repo and the reflex platform
git clone git@github.com:samtay/rule110.git
git clone git@github.com:reflex-frp/reflex-platform.git
cd rule110
# write your own src/Rule.hs or modify the code to play with something else

# enter nix-shell to build this SPA via ghcjs
../reflex-platform/work-on ghcjs ./.
cabal configure --ghcjs
cabal build
xdg-open dist/build/rule110/rule110.jsexe/index.html

# optionally, enter nix-shell that can build this via ghc (native GUI)
../reflex-platform/work-on ghc ./.
cabal configure
cabal run
```
