C    @(#)newvec.f	20.3 2/13/96
      subroutine newvec
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/smallp.inc'

      double precision aij, geta
 
      xkpos = 1.0
      if (newx.gt.n) go to 40
      do 10 k = 1,size
   10 gr(k) = 0.0
      do 30 l = 1,size
         i = ybasis(l)
         aij = geta(i,newx)
         if (aij.eq.0.0) go to 30
         do 20 k = 1,size
            gr(k) = gr(k) + aij * inv(k,l)
   20    continue
   30 continue

      if (inbase(newx).eq.-1) xkpos = -1.0
      go to 60

   40 i = newx - n
      l = iseff(i)
      do 50 k = 1,size
   50 gr(k) = inv(k,l)
      if (s(i).eq.-1.0) xkpos = -1.0
   60 return
      end
