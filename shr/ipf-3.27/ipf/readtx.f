C    @(#)readtx.f	20.3 2/13/96
      subroutine readtx
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/data.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      character bigbuf * 512
C
C     "INPTLS"= 0/1 (contents of BUF have not/have been deciphered)
C
      if (inptls .eq. 1) then
C
C        READ NEXT RECORD
C
   50    read (inp,100, end=110) buf
  100    format (a)
C
C        Check for and concatenate continuation records.
C
  102    last = lastch (buf)
         if (buf(last:last) .eq. '-') then
            bigbuf = buf
            read (inp, 100, end=110) bigbuf(last:)
            buf = bigbuf
            go to 102
         endif
         if ( buf(1:1) .eq. '.' ) goto 50
         outbuf = ' ' // buf
         call prtout(1)
      endif
C
      card = buf(1:1)
      inptls = 0
      return
C
C     END-OF-FILE processing
C
  110 card = '('
      buf = '( STOP   -  END OF FILE )'
C
      if (inp .eq. 13 ) then
         write (errbuf(1),120)
  120    format('0 WARNING - NO "( STOP )" STATEMENT IN CONTROL FILE')
         call prterx ('W',1)
      endif
 
      inptls = 0
 
      return
C
C ************************************************************
C
      entry prntxt
         outbuf = ' ' // buf
         call prtout(1)
         card = buf(1:1)
         inptls = 0
      return
      end
