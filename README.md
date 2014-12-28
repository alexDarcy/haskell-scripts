shelly-examples
===============

To learn Haskell, I decided to convert my Bash/Perl scripts into Haskell using
Shelly. Details of the scripts :

### `Encryption`

Encrypt or decrypt a (hard-coded) list of files using GPG
with symmetric encryption. Usage :

    encrypt [-e | -d]

The files are created in the same input directory. Also, the script must be run
in the root directory.  The passphrase is read from `$HOME/.passphrase_files`. 

Note it only works for gpg 1 at the moment. 


## Requirements

To install Haskell, I recommand the Haskell platform. [See the official site for
more information](https://www.haskell.org/platform/).  
To install Shelly, `cabal` is the easiest way. [For more details, see this
reference](https://www.haskell.org/haskellwiki/Cabal-Install).

## Installing

Install the dependencies in a sandbox with cabal and compile all the scripts:

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

Run the `encrypt` script with :

    ./dist/build/encrypt/encrypt -e
