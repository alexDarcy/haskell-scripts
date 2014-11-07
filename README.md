shelly-examples
===============

To learn Haskell, I decided to convert my Bash/Perl scripts into Haskell using
Shelly. Details of the scripts :

* `encryption.hs`: encrypt or decrypt a (hard-coded) list of files using GPG
  with symmetric encryption. The passphrase must be stored in
  `../../passphrase_files`.

## Requirements
To install Haskell, I recommand the Haskell platform. [See the official site for
more information](https://www.haskell.org/platform/).  
To install Shelly, `cabal` is the easiest way. [For more details, see this
reference](https://www.haskell.org/haskellwiki/Cabal-Install).

## Run the scripts
Simply use 

    runhaskell encryption.hs
