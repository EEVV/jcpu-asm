# jcpu-asm
Jift CPU assembler rewritten in Haskell.

## Compiling
### Compiling with Cabal
#### Build only
```
$ cabal build
```
#### Build and install
```
$ cabal install
```
You may have to add `$HOME/.cabal/bin/` to your path.
## Running
### Preview mode
Preview mode allows you to check and verify the output. The output is dumped to `stdout` in 32 bit binary format. Each 32 bit is seperated by a newline.
```
$ jcpu-asm [PATH TO INPUT]
```
### Save mode
Save mode simply saved to the output file given the path.
```
$ jcpu-asm [PATH TO INPUT] [PATH TO OUTPUT]
```