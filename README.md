# meme

A simple implementation of R6RS scheme.

Why `meme`? It's built out of monads, and LISP is a huge meme.

## Building

a Haskell toolchain and stack are required to build.
Just run `stack build` in the project root.

## Running

`stack run`

or if you like it that much:

`stack install && meme`

Invoking `meme` without any arguments will load the REPL, and with arguments will run your files directly.

Invoking `meme -i <file>` will load the file in interactive mode.

### Motivation

I was trying to work through SICP and found both MIT scheme and Chez scheme to be annoying to use, especially in terms of loading files.
Loading files is easy, by the way. Just run `(load "path-to-your-file")` in the REPL.

This is the result of a Thanksgiving weekend of hacking. I followed the tutorial `Write yourself a Scheme in 48 hours`, which I highly recommend to anyone else wanting to learn language development.

In all honestly, this was my actual inspiration: [![actual inspiration](https://img.youtube.com/vi/rdj6deraQ6k/maxresdefault.jpg)](https://www.youtube.com/watch?v=rdj6deraQ6k)


### Notes
Floating and complex numbers are currently not supported, nor is a proper printing function. Use the REPL for now!

Macros still have yet to be implemented.

### Contributing
Contributions are greatly appreciated. Check the TODO list for ideas on how to contribute.
We need floating points and complex numbers, and tests.


