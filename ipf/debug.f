C    @(#)debug.f	20.3 2/13/96
      subroutine debug(ktot)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/lfiles.inc'
      include 'ipfinc/red2.inc'
C
      character a*132
      imax = ncmax - ncmin + 1
      do 60 i = 1,nsize
      write (a,20) i,kolum(i),korder(i),i
   20 format(i10,50x,3i10)
      if (i.gt.ktot) go to 40
      write (a(31:70),30) kownt(1,i),kownt(2,i),ich1(i),ich2(i),
     1 loc(i)
   30 format(2i5,3i10)
      if (i.gt.imax) go to 40
      iv = ncmin + i - 1
      write (a(21:30),30) iv,indexx(iv+ix0)
   40 continue
      write (dbug,50) a
   50 format(a)
   60 continue
      return
      end
