C    @(#)stortx.f	20.3 2/13/96
      subroutine stortx (text)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/prt.inc'
C
      character text*(*)
C
C     STORE BUS TEXT DATA
C
      entry bustxt (text)
      nbsmrg = nbsmrg + 1
      write (logmbs,90) text
   90 format (a)
      return
C
C     STORE BRANCH TEXT DATA
C
      entry brntxt  (text)
      nbrmrg = nbrmrg + 1
      write (logmbr,90) text
      return
C
C     STORE INTERFACE TEXT DATA
C
      entry facetx  (text)
      if (nifmrg.lt.400) then
         nifmrg = nifmrg + 1
         fcetxt(nifmrg) = text
      else
         write (errbuf(1),120) nifmrg
  120    format('0 MORE THAN (',i4,') MERGED INTERFACE BRANCH TEXT ',
     1  'ENCODED.')
         call prterx ('W',1)
      endif
      return
      end
