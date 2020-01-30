C    @(#)fcvmul.f	20.3 2/13/96
      subroutine fcvmul (a,i,b,j,c,k,n,f)

      integer f
      real a(*),b(*),c(*)

      ia = 1
      ib = 1
      ic = 1
      do 100 l=1,n
         if (f.ne.-1) then
            c(ic) = a(ia)*b(ib) - a(ia+1)*b(ib+1)
            c(ic+1) = a(ia+1)*b(ib) + a(ia)*b(ib+1)
         else
            c(ic) = a(ia)*b(ib) + a(ia+1)*b(ib+1)
            c(ic+1) = -a(ia+1)*b(ib) + a(ia)*b(ib+1)
         endif
         ia = ia + i
         ib = ib + j
         ic = ic + k
  100 continue
      return
      end
