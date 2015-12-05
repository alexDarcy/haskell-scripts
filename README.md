shelly-examples
===============

![Build status](https://travis-ci.org/alexDarcy/shelly-examples.svg)

To learn Haskell, I decided to convert my Bash/Perl scripts into Haskell using
Shelly. Details of the scripts are given below.

Haskell can be installed using [Haskell platform](https://www.haskell.org/platform/).  
To install Shelly, `cabal` is the easiest way. [For more details, see this
reference](https://www.haskell.org/haskellwiki/Cabal-Install).

## Installation

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal build

The executables will be in dist/build/X/X where X is the executable name.

## `Krypt`

Encrypt or decrypt a list of files using GPG with symmetric encryption. Usage :

    krypt [-e | -d] file1 [file2 ...]

For encryption, the output file is `file1.gpg` at the same location. Decrypted
files have the `.gpg` extension removed.
The passphrase is read from `$HOME/.passphrase_files`. 

It works for GPG 2.

## `MplayerRR`
A wrapper around mplayer. At the moment, it only deactivates the mouse cursor.
Use it as the normal mplayer.
