C    @(#)ext_ai.f	20.7 11/12/98
C****************************************************************
C
C   	File: ext_ai.f
C
C   	Purpose: Write area interchange "A" and "I" data onto saved
c                NETWORK_DATA file savfil.
C                                                                      *
C       Input parameters:
C
C             savfil   - the logical unit opened
C             dialect  - a character string (BPA, WSCC, WSCC1, PTI)
C                        denoting the dialect of the WSCC record.
c             size     - a character string 80 or 120 denoting the
c                        output record size
c             ratcod   - a character string denoting extended 
c                        line ratings used (E- EXTENDED, N-NOMINAL, 
C                        or M-MINIMUM)
c             sections - a character string <null> or "PSEUDOBUSES"
c                        denoting how line sections are to be 
c                        represented
c             type_e   - a character string <null> or "SYMMETRIC"
c                        denoting how assymetric type E-branches are
c                        to be represented
C
C   	Author: Walt Powell            Date: 13 January 1993
C   	Called by: savenetd.f
C
C****************************************************************
C
      integer function ext_ai ( savfil, dialect, lenrec, ratcod, 
     &                          sections, type_e )
 
      character*(*) dialect, ratcod, sections, type_e
      integer savfil, lenrec
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/wsccbase.inc'

      character*120 xbuf
      integer total1, total2
 
c     Extract area controls                                            *

      total1 = 0
      total2 = 0
      ext_ai = 0

      if (dialect .eq. 'BPA' .or. dialect .eq. 'PTI') then

         do nn = 1, ntotc
            call bcdarc(nn, xbuf)
            write (savfil, '(a80)') xbuf
            total1 = total1 + 1
            j1 = 1
            jt = MAXCAZ / MAXCAZR
            do while (j1 .lt. jt    .and. 
     &                arczns(j1*MAXCAZR+1,nn) .ne. ' ' )
               call bcdarc2 (nn, j1, xbuf)
               write (savfil, '(a80)') xbuf
               total1 = total1 + 1
               j1 = j1 + 1
            enddo
         enddo

      else if (dialect(1:4) .eq. 'WSCC') then
         do nn = 1, ntotc
            call bcdarc(nn, xbuf)
            basekv = ftn_atof (xbuf(22:25))
            mb = karea(1,nn)
            if (mb .ne. 0) then
              if (wsccbase(mb) .ne. ' ') then
                xbuf(22:25) = wsccbase(mb)
              else
                xbuf(22:25) = pti_name(mb)
              endif
            endif
            xbuf(2:2) = ' ' 
            write (savfil, '(a80)') xbuf
            total1 = total1 + 1
         enddo
 
      endif
 
      if (dialect .eq. 'BPA') then
 
c        Extract area-interchange tielines (low to high direction only)
 
         do nn = 1, ntotic
            if ( kompr (arcint(1,nn), arcint(2,nn), junk) .lt. 0) then
               call bcdari(nn, xbuf)
               write (savfil, '(a80)') xbuf
               total2 = total2 + 1
            endif
         enddo
 
      endif

      write ( errbuf(1), 100) total1, total2
  100 format (' Total area records extracted: ', i4, ' A and ', i4, 
     &   ' I ')
      call prterx ('I', 1)

      ext_ai = total1 + total2
 
      return
      end
