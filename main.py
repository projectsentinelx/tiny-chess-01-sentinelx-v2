import json,os,sys,urllib.request as u
V={1:100,2:320,3:330,4:500,5:900,6:0}
KN=(-33,-31,-18,-14,14,18,31,33)
BI=(-17,-15,15,17)
RO=(-16,-1,1,16)
KI=(-17,-16,-15,-1,1,15,16,17)
S=[s for s in range(128)if not s&0x88]
MT=99999
BD=59000
N=0
TT={}
BM=0
class X(Exception):pass
def ev(b,t):
 s=0
 k=b.index(6*t)
 for i,p in enumerate(b):
  if not p:continue
  a=abs(p)
  r=i>>4
  c=i&7
  v=V[a]
  if a==1:v+=(r if p>0 else 7-r)*5+(14 if 1<c<6 else-6)
  elif a<4:v+=(6-abs(3-c)-abs(3-r))*7-(9 if a>1 and r in(0,7)else 0)
  elif a==6:v+=abs(2*r-7)+abs(2*c-7)
  s+=v if p>0 else-v
 for d in(-17,-16,-15,15,16,17):
  g=k+d
  if not g&0x88 and b[g]==t:s+=13*t
 return t*s
def pa(f):
 p=f.split()
 b=[0]*128
 for i,r in enumerate(p[0].split('/')):
  c=0
  for h in r:
   if h in'12345678':c+=int(h)
   else:v='pnbrqk'.index(h.lower())+1;b[(7-i)*16+c]=v if h<'a'else-v;c+=1
 return b,1 if p[1]=='w'else-1
def um(s):
 f=(int(s[1])-1)*16+ord(s[0])-97
 g=(int(s[3])-1)*16+ord(s[2])-97
 return f|g<<7|(('nbrq'.index(s[4])+2)<<14 if len(s)>4 else 0)
def ap(b,m,t):
 b=b[:]
 f=m&127
 g=m>>7&127
 pr=m>>14
 p=b[f]
 b[f]=0
 b[g]=p if not pr else pr*t
 if abs(p)==6 and abs(g-f)==2:
  r=g-1 if g>f else g+1
  b[r]=b[r+2 if g>f else r-2]
  b[r+2 if g>f else r-2]=0
 return b,-t
def ge(b,t):
 ms=[]
 for q in S:
  p=b[q]
  a=abs(p)
  if p*t<=0:continue
  if a==1:
   d=16*t
   g=q+d
   if not g&0x88 and not b[g]:
    ms.append(q|g<<7|(5<<14)if g>>4 in(0,7)else q|g<<7)
    if q>>4==(1 if t>0 else 6)and not(g+d)&0x88 and not b[g+d]:ms.append(q|(g+d)<<7)
   for e in d-1,d+1:
    g=q+e
    if not g&0x88 and b[g]*t<0:ms.append(q|g<<7|(5<<14)if g>>4 in(0,7)else q|g<<7)
  else:
   for d in(KN if a==2 else(KI if a==6 else(BI if a==3 else RO if a==4 else KI))):
    g=q+d
    while not g&0x88:
     v=b[g]
     if v*t>0:break
     ms.append(q|g<<7)
     if v or a==2 or a==6:break
     g+=d
 return ms
def od(b,ms,tt):
 def k(m):
  v=b[m>>7&127]
  s=1e6 if m==tt else 0
  if v:s+=1e5+V[abs(v)]*10-V[abs(b[m&127])]
  if m>>14:s+=9e4+(m>>14)
  return-s
 return sorted(ms,key=k)
def qs(b,t,al,be,pl):
 global N
 N+=1
 if N>BD:raise X
 s=ev(b,t)
 if s>=be:return be
 if s>al:al=s
 if pl>10:return al
 cm=[m for m in ge(b,t)if m>>14 or b[m>>7&127]]
 for m in od(b,cm,0):
  if abs(b[m>>7&127])==6:return MT-pl
  nb,nt=ap(b,m,t)
  s=-qs(nb,nt,-be,-al,pl+1)
  if s>=be:return be
  if s>al:al=s
 return al
def ab(b,t,d,al,be,pl):
 global N
 N+=1
 if N>BD:raise X
 ky=t,tuple(b)
 e=TT.get(ky)
 tt=0
 if e:
  d0,s0,f0=tt=e
  if s0>MT-999:s0-=pl
  elif s0<-MT+999:s0+=pl
  if d0>=d:
   if f0==0:return s0
   if f0==1 and s0>=be:return s0
   if f0==2 and s0<=al:return s0
 if d<=0:return qs(b,t,al,be,pl)
 a0=al
 best=-MT*2
 for m in od(b,ge(b,t),tt):
  if abs(b[m>>7&127])==6:return MT-pl
  nb,nt=ap(b,m,t)
  s=-ab(nb,nt,d-1,-be,-a0,pl+1)
  if s>best:best=s
  if s>a0:a0=s
  if a0>=be:break
 if best==-MT*2:best=0
 sb=best
 if sb>MT-999:sb+=pl
 elif sb<-MT+999:sb-=pl
 TT[ky]=d,sb,0 if al<best<be else(1 if best>=be else 2)
 return best
def rt(b,t,ms,d,pv):
 global BM
 al=best=-MT*2
 for m in od(b,ms,pv):
  nb,nt=ap(b,m,t)
  s=-ab(nb,nt,d-1,-MT*2,-al,1)
  if s>best:best=s;BM=m
  if s>al:al=s
 return best
def ch(f,lg):
 global N,BM
 b,t=pa(f)
 D={um(x):x for x in lg if b[um(x)&127]*t>0}
 if not D:return lg[0]
 best=BM=list(D)[0]
 N=0
 if len(TT)>2e5:TT.clear()
 for d in range(1,13):
  BM=best
  try:sc=rt(b,t,list(D),d,best)
  except X:best=BM;break
  best=BM
  if sc>MT-999:break
 return D.get(best,lg[0])
def po(U,p,body):
 q=u.Request(U+p,data=json.dumps(body).encode(),headers={'content-type':'application/json'})
 try:
  with u.urlopen(q)as r:return json.load(r)
 except u.HTTPError as e:sys.exit(e.code)
def main():
 U=os.environ.get('MAKEFUN_ENV_URL')or sys.exit('MAKEFUN_ENV_URL not set')
 while 1:
  g=po(U,'/game',{})
  if g['status']=='done':break
  while g['status']=='your_move':g=po(U,'/move',{'game':g['game'],'move':ch(g['fen'],g['legal_moves'])})
  print(g,file=sys.stderr)
 print(g,file=sys.stderr)
if __name__=='__main__':main()
