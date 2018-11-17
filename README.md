<a href="http://arrayfire.com/"><img src="http://arrayfire.com/logos/arrayfire_logo_whitebkgnd.png" width="300"></a>

ArrayFire is a general-purpose library that simplifies the process of developing
software that targets parallel and massively-parallel architectures including
CPUs, GPUs, and other hardware acceleration devices.

`haskfire` is a [Haskell](https://haskell.org) binding for ArrayFire.

### Installation
```
nix-build
```

### Develop
```
nix-shell
```

### Example
```
module Main where

import Data.Array.Fire

main :: IO ()
main = putStrLn "hello world"
```
