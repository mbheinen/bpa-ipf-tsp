C    @(#)frect.f	20.3 2/13/96
      subroutine frect (a,i,c,k,n)

      real a(*),c(*)

      ia = 1
      ic = 1
      do 100 l=1,n
         c(ic) = a(ia) * cos( a(ia+1) )
         c(ic+1) = a(ia) * sin( a(ia+1) )
         ia = ia + i
         ic = ic + k
  100 continue
      return
      end
