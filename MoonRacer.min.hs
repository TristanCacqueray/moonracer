import Imports;a=(b,b,b,b,[(m(p`mod`70-35),m(5+p`mod`50))|p<-[0,88..]]);b=(0,0)
c(p,q)=translate(p*10)(q*10);d p=scale p p;e p=c p(Circle 3);y=InWindow"MoonRac"
f p q=c p(d 10$line[b,q]);h(EventKey(SpecialKey p)q _ _)=(p,bool 1 0$q==Up);
h _=(KeyF1,0);g(p,(_,q,r,_,s:_))=c(0,-29.7)(d 0.1(Text p)<>e q<>f q r<>e s);
z=y(800,600)(5,5);l=10;m=fromIntegral;n=negate;o=True;i p(q,(r,s,t,(u,v),w))=let
 (x,y)=h p;z|x==KeyRight=y|x==KeyLeft=n y|o=u;α|x==KeyUp=y|x==KeyDown=n y|o=v in
 (q,(r,s,t,(z,α),w));j=(*)0.8;k p(q,((r,s),(t,u),(v,w),x@(y,z),α@((β,γ):δ)))=let
 ε ο π=abs(ο-π)<1;(ζ,η)|ε t β&&ε u γ=(r+1,δ)|o=(r,α);θ=t+(v+y)*p*10;κ=j(y+v);
 ι=max 0(u+(w+z)*p*10);λ=j(z+w-0.3);μ=((ζ,s+p),(θ,ι),(κ,λ),x,η);
 (ν,ξ)|r>9=("Best: "<>show s,a)|o=(q,μ)in(ν,ξ);main=play z white 30("",a)g i k;
-- ^10 ------------------------------------------------------------------ 80> --
{- hackage-10-80/moon-racer (tristanC)

- Play: stack runghc MoonRacer.hs.min.hs --resolver lts-20.13 --package gloss
- Keys: arrow to fly the ship

Copyright 2023, Tristan de Cacqueray
SPDX-License-Identifier: CC-BY-4.0
-}
