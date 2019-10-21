## <a href="http://arrayfire.com/"><img src="http://arrayfire.com/logos/arrayfire_logo_whitebkgnd.png" width="300"></a>
`ArrayFire` is a general-purpose library that simplifies the process of developing software that targets parallel and massively-parallel architectures including CPUs, GPUs, and other hardware acceleration devices.

`arrayfire-haskell` is a [Haskell](https://haskell.org) binding to [ArrayFire](https://arrayfire.com).

## Table of Contents
 - [Installation](#Installation)
   - [Haskell Installation](#haskell-installation)
 - [Documentation](#Documentation)
 - [Hacking](#Hacking)
 - [Example](#Example)


## Installation
Install `ArraryFire` via the download page.
  - https://arrayfire.com/download/

`ArrayFire` can also be fetched from [nixpkgs](https://github.com/nixos/nixpkgs) `master`.

### Haskell Installation

`arrayfire` can be installed w/ `cabal`, `stack` or `nix`.

```
cabal install arrayfire
```

```
stack install arrayfire
```


Also note, if you plan on using ArrayFire's visualization features, you must install `fontconfig` and `glfw` on OSX or Linux.

## Documentation
  - [Hackage](http://hackage.haskell.org/package/arrayfire)
  - [ArrayFire](http://arrayfire.org/docs/gettingstarted.htm)

## Hacking
To hack on this library locally, complete the installation step above. We recommend installing the [nix](https://nixos.org/nix/download.html) package manager to facilitate development.

After the above tools are installed, clone the source from Github.

```bash
git clone git@github.com:arrayfire/arrayfire-haskell.git
cd arrayfire-haskell
```

To build and run all tests in response to file changes

```bash
nix-shell --run test-runner
```

To perform interactive development w/ `ghcid`

```bash
nix-shell --run ghcid
```

To interactively evaluate code in the `repl`

```bash
nix-shell --run repl
```

To produce the haddocks and open them in a browser

```bash
nix-shell --run docs
```


## Example
```haskell
module Main where

import qualified ArrayFire as A

main :: IO ()
main = print newArray `catch` (\(e :: A.AFException) -> print e)
  where
    newArray = A.matrix @Double (2,2) [ [1..], [1..] ] * A.matrix @Double (2,2) [ [2..], [2..] ]

{-|

ArrayFire Array
[2 2 1 1]
    2.0000     6.0000
    2.0000     6.0000

-}
```