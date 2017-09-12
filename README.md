# rule 110
See this in action at
[samtay.github.io/reflex-rule110](https://samtay.github.io/reflex-rule110).

## about
This is a small working example of showing the
[rule 110](https://en.wikipedia.org/wiki/Rule_110) computation via
[reflex](https://github.com/reflex-frp/reflex), an FRP library.

**Note**: I recently wrote a little Haskell implementation of rule 110 as an
interview challenge problem and told the potential employer that I would not
post the solution online. Hence, the `Rule` module is gitignored here.

## building

#### dependencies
The only build requirement is [nix](https://nixos.org/nix/download.html)
which can be installed by running a single script:
```shell
curl https://nixos.org/nix/install | sh
```

#### using `build`
Everything you should need is encompassed in my `build` script. The first time
you run it, be prepared to wait a while. The second time around everything should
be cached and snappy.

First, write your own Rule.hs file or something else to
play with. Then:
```shell
# build via ghcjs to web
./build
xdg-open dist/build/rule110/rule110.jsexe/index.html

# build via ghc to native webkitgtk
./build --ghc
./dist/build/rule110/rule110

# enter nix-shell for either environment
./build -s [--ghc|--ghcjs]
```
