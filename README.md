# moonracer - a tiny game

![badge](https://img.shields.io/badge/haskell-game-blue?logo=haskell)
[![License CC-BY-SA][badge-license]][license]

[badge-license]: https://img.shields.io/badge/license-CC_BY--SA_4.0-blue.svg?dummy
[license]: https://github.com/TristanCacqueray/moonracer/blob/main/LICENSE

![moonracer](https://github.com/TristanCacqueray/moonracer/assets/154392/caa234a6-57fc-4c3c-99ac-999d5bb653f4)


Goal: reach 10 targets as fast as possible.
Control: arrow keys to move the ship.

My personal best is `14.2 sec`.

## Usage

Run the regular version: `cabal run moonracer`

Run the lighter version: `cabal run moonracer-light`

Tiny versions (10 lines, 80 columns) are also provided:

Build regular: `ghc -package base -package gloss MoonRacer.min.hs && ./MoonRacer.min `

Run the tiny version: `./MoonRacerLight.min.hs`.

## Tiny versions

The minified code is generated using [simple-haskell-minifier](https://github.com/TristanCacqueray/simple-haskell-minifier#readme).

The regular version doesn't have room for a self executable.
The lighter version sacrifices the velocity vector display, the ground and the down arrow key to save 80 chars:

```haskell
#!/usr/bin/env -S stack script --compile --resolver lts-20.13 --package gloss
import Imports;a=(b,b,b,[(m(p`mod`70-35),m(5+p`mod`50))|p<-[0,88..]]);b=(0,0)
main=play(InWindow"MoonRacer"(800,600)(5,5))white 30("",b,a)g i l;d p=scale p p
c(p,q)=translate(p*10)(q*10);e p=c p(Circle 3);f p q=c p(d 10$line[b,q])
h(EventKey(SpecialKey p)q _ _)=(p,bool 1 0$q==Up);h _=(KeyF1,0);j=(*)0.8;o=True
g(p,_,(_,q,_,r:_))=c(0,-29.7)(d 0.1(Text p)<>e q<>e r);i p(q,(r,s),t)=let
 (u,v)=h p;w|u==KeyRight=v|u==KeyLeft=n v|o=r;x|u==KeyUp=v|o=s in (q,(w,x),t)
k=0.3;n=negate;m=fromIntegral;l p(q,r@(s,t),((u,v),(w,x),(y,z),α@((β,γ):δ)))=let
 ε μ ν=abs(μ-ν)<1;(ζ,η)|ε w β&&ε x γ=(u+1,δ)|o=(u,α);θ=(w+(y+s)*k,x+(z+t)*k)
 ι=(j(s+y),j(t+z-k));κ=((ζ,v+p),θ,ι,η);λ|u>9=(show v,b,a)|o=(q,r,κ)in λ
-- ^10 ------------------------------------------------------------------ 80> --
{- hackage-10-80/moon-racer (tristanC)

- Keys: arrow to fly the ship

Copyright 2023, Tristan de Cacqueray
SPDX-License-Identifier: CC-BY-4.0
-}
```

## freeglut Internal error and nixgl

If the program fials with:

```ShellSession
freeglut (MoonRacer.min):  ERROR:  Internal error <FBConfig with necessary capabilities not found> in function fgOpenWindow
```

You need to use `nixGL`. To install the right version (see hspkgs input to match the nixpkgs pin):

```ShellSession
nix profile install --override-input nixpkgs github:NixOS/nixpkgs/e365e1db48d060b3e31b02ec8177f66f386f39b8 --impure github:guibou/nixGL
```

Then run:

```ShellSession
nixGL cabal run moonracer
```
