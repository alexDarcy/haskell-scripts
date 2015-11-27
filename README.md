shelly-examples
===============

![Build status](https://travis-ci.org/alexDarcy/shelly-examples.svg)

To learn Haskell, I decided to convert my Bash/Perl scripts into Haskell using
Shelly. Details of the scripts are given below.

Haskell can be installed using [Haskell platform](https://www.haskell.org/platform/).  
To install Shelly, `cabal` is the easiest way. [For more details, see this
reference](https://www.haskell.org/haskellwiki/Cabal-Install).

## `Krypt`

Encrypt or decrypt a list of files using GPG with symmetric encryption. Usage :

    krypt [-e | -d] file1 [file2 ...]

For encryption, the output file is `file1.gpg` at the same location. Decrypted
files have the `.gpg` extension removed.
The passphrase is read from `$HOME/.passphrase_files`. 

It works for GPG 2.

### Installation

Install the dependencies in a sandbox with cabal and compile all the scripts:

    cabal sandbox init
    cabal install --only-dependencies
    cabal build
