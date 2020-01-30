C    @(#)postpost.f	20.3 2/13/96
      subroutine postpost(record)
      character record*(*)

      include 'ipfinc/postdta.inc'
      include 'ipfinc/pasprm.inc'

      lengmx = len(record)
      do length = lengmx, 2, -1
          if (record(length:length) .ne. ' ') go to 110
      enddo
  110 continue
************ debug stuff ********************* 
*     print 9911, lengmx, length
*9911 format('in postpost - lengmx, length :', 2i5) 
*     print 9921, record(2:length)
*9921 format('in postpost :',  a )
************ debug stuff ********************* 
      write (lpost, 121) record(2:length)
  121 format( a)
      return
      end
