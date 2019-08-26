Haskell-scripts
===============

![Build status](https://travis-ci.org/alexDarcy/shelly-examples.svg)

To learn Haskell, I decided to convert my Bash/Perl scripts into Haskell. Some
of theme use Shelly. Details of the scripts are given below.

Haskell can be installed using Stack.

## Installation

    $ stack init
    $ stack install

The executables will be in `$HOME/.local/bin`.

## `Krypt` (Shelly)

Encrypt or decrypt a list of files using GPG with symmetric encryption. Usage :

    krypt [-e | -d] file1 [file2 ...]

For encryption, the output file is `file1.gpg` at the same location. Decrypted
files have the `.gpg` extension removed.
The passphrase is read from `$HOME/.passphrase_files`. 

It works for GPG 2.

## `MplayerRR` (Shelly)
A wrapper around mplayer. At the moment, it only deactivates the mouse cursor.
Use it as the normal mplayer.

## `random_wallpaper` (Shelly)
Set a random wallpaper from a given folder.

## Parser example with attoparsec : `stages`
Work in progress. The idea is to count identical lines in a given format.
