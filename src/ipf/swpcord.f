C    @(#)swpcord.f	20.3 2/13/96
      subroutine swpcord(m, n)

      include 'ipfinc/srtdta.inc'

*********************** debug stuff ******************
*     print 9901, m, ascrec(m), n, ascrec(n)
*9901 format(' entering kmpcord'/i5, a,/i5,a)
*********************** debug stuff ******************

      itmp   = key(m)
      key(m) = key(n)
      key(n) = itmp

      temp      = ascrec(m)
      ascrec(m) = ascrec(n)
      ascrec(n) = temp

      bufsrt(1:10) = bus1(m)
      bus1(m)  = bus1(n)
      bus1(n)  = bufsrt(1:10)
 
      ftmp     = base1(m)
      base1(m) = base1(n)
      base1(n) = ftmp
      
      bufsrt(1:10) = bus2(m)
      bus2(m)  = bus2(n)
      bus2(n)  = bufsrt(1:10)

      ftmp     = base2(m)
      base2(m) = base2(n)
      base2(n) = ftmp

     
      itmp     = kntrec(m)
      kntrec(m) = kntrec(n)
      kntrec(n) = itmp

*********************** debug stuff ******************
*     print 9911
*9911 format(' leaving swpcord')
*********************** debug stuff ******************

      return
      end
