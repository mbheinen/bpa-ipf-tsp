C    @(#)invert.f	20.3 2/13/96
      subroutine invert(nc)
C
      include 'ipfinc/work1.inc'
c	Global variables used:
c	aa(r*8), space(r*8), x(r*8), ierr
C
      double precision a(30,31), temp
      integer nj(30), ntemp
      equivalence (a,aa), (nj,space)

      zero = 1.0e-10
      ierr=0
      nm1=nc-1
      np1=nc+1
      ncv=nc+1
      do 100 i=1,nc
  100 nj(i)=i
      do 110 i=1,nc
  110 a(i,nc+1)=x(i)
      do 210 k=1,nm1
      temp=0.0
      do 120 i=k,nc
      do 120 j=k,nc
      if (dabs(a(i,j)).le.temp) go to 120
      temp=dabs(a(i,j))
      ii=i
      jj=j
  120 continue
      if (temp.gt.zero) go to 130
      ierr=k
      go to 260
  130 if (ii-k) 140,160,140
140   do 150 j=k,ncv
      temp=a(ii,j)
      a(ii,j)=a(k,j)
  150 a(k,j)=temp
  160 if (jj-k) 170,190,170
  170 do 180 i=1,nc
         temp=a(i,jj)
         a(i,jj)=a(i,k)
  180 a(i,k)=temp
      ntemp=nj(jj)
      nj(jj)=nj(k)
      nj(k)=ntemp
  190 l=k+1
      do 200 j=l,ncv
         a(k,j)=a(k,j)/a(k,k)
         do 200 i=l,nc
  200 a(i,j)=a(i,j)-a(i,k)*a(k,j)
  210 continue
      if (dabs(a(nc,nc)).gt.zero) go to 220
      ierr=nc
      go to 260

220   do 240 j=np1,ncv
      a(nc,j)=a(nc,j)/a(nc,nc)
      k=nc
      do 230 l=1,nm1
      k=k-1
      do 230 i=k,nm1
  230 a(k,j)=a(k,j)-a(k,i+1)*a(i+1,j)
  240 continue
      do 250 i=1,nc
      l=nj(i)
  250 x(l)=a(i,nc+1)
  260 return
      end
