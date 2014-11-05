shelly-examples
===============

To learn Haskell, I decided to convert my Bash/Perl scripts into Haskell using
Shelly.

## Installing
To install Haskell, I recommand the Haskell platform. [See the official site for
more information](https://www.haskell.org/platform/).  
To install Shelly, `cabal` is the easiest way. [For more details, see this
reference](https://www.haskell.org/haskellwiki/Cabal-Install).

To build and install the script:

    cabal install -j
    ./.cabal-sandbox/bin/encryption

To generate the documentation:

    cabal haddock --executables

View it in `dist/doc/html/shelly-examples/encryption/Main.html`.
