C    @(#)fvsma.f	20.3 2/13/96
      subroutine fvsma (a,i,b,c,k,d,l,n)
      real a(*),c(*),d(*)

      ia = 1
      ic = 1
      id = 1
      do 100 m=1,n
         d(id) = a(ia) *b + c(ic)
         ia = ia + i
         ic = ic + k
         id = id + l
  100 continue
      return
      end
