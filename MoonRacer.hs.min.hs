import Imports;a=(b,b,b,b,[(h(l`i`70-35),h(5+l`i`50))|l<-[0,88..]]);b=(0,0)
c(l,m)=k(l*10)(m*10)(Circle 3);main=play(w(20,20))white 30("",a)d f g;i=mod
d(l,(_,m,_,_,n:_))=k 0(-297)(scale 0.1 0.1(Text l)<>c m<>c n);y=(800,600)
e(EventKey(SpecialKey l)m _ _)=(l,bool 0 1$m==Down);e _=(KeyF1,0);z= -1;
w=(InWindow"MR"y);k=translate;j=True;h=fromIntegral;f l(m,(n,o,p,(q,r),s))=let
 (t,u)=e l;v|t==KeyRight=u|t==KeyLeft=z*u|j=q;w|t==KeyUp=u|t==KeyDown=z*u|j=r
 in (m,(n,o,p,(v,w),s));g l(m,((n,o),(p,q),(r,s),t@(u,v),(w,x):y))=let
 z ι κ=abs(ι-κ)<1;(α,β)=if z p w&&z q x then(n+1,y)else(n,(w,x):y);
 γ=p+(r+u)*l*10;δ=max 0(q+(s+v)*l*10);ε=(u+r)*0.8;ζ=(v+s)*0.8+(-0.5);
 (η,θ)|n>9=("Best: "<>show o,a)|j=(m,((α,o+l),(γ,δ),(ε,ζ),t,β)) in (η,θ);
