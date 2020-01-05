C    @(#)swprt2.f	20.3 2/13/96
      subroutine swprt2(i,j)
 
      include 'ipfinc/cgrate.inc'
 
      do 10 k=1,3
         itemp=lrat(k,i)
         lrat(k,i)=lrat(k,j)
         lrat(k,j)=itemp

         temp=rat(k,i)
         rat(k,i)=rat(k,j)
         rat(k,j)=temp
10    continue
 
      return
      end
