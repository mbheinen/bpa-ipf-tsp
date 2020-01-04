C    @(#)addcon.f	20.3 2/13/96
        subroutine addcon
 
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/aref.inc'
c	Global Variables used:
c		aa(r*8), jcol, irow
      include 'ipfinc/smallp.inc'
c	Global Variables used:
c		istate, newy, isbig, size, size1, ybasis, xbasis,
c		 numslk, inbase, yr(r*8), inv(r*8), xr(r*8), slack(r*8)
 
        double precision aij
 
      if (size1.le.MXSIZE) then
         i = newy - size
         do 10 l = 1,size
            inv(l,size1) = 0.0d0
            inv(size1,l) = 0.0d0
   10    continue
         istart = irow(i)
         last = irow(i+1) - 1
         do 30 look = istart,last
            j = jcol(look)
            if (inbase(j).le.0) go to 30
            k = inbase(j)
            aij = aa(look)
            do 20 l = 1,size
               inv(size1,l) = inv(size1,l) - aij * inv(k,l)
   20       continue
   30    continue
         inv(size1,size1) = 1.0d0
         xr(size1) = slack(i)
         iseff(i) = size1
         xbasis(size1) = i + n
         ybasis(size1) = i
         yr(size1) = 0.0d0
         size = size1
         size1 = size1 + 1
         if (size.gt.isbig) isbig = size
         numslk = numslk + 1
         newy = size

      else       
         istate = 4
      endif

      return
      end
