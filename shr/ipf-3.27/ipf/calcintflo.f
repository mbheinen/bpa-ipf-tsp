C    @(#)calcintflo.f	20.4 7/18/96
C****************************************************************
C
C   File: calcintflo.f
C
C   Purpose: Routine to calculate selected area intertie "I" flows.
c
C   Author: Walt Powell  
C   Date: 5 March 1996
C   Called by: gtoutput.f
C
C****************************************************************
C
        integer function calcintflo (ka1, ka2, sched_export, 
     &      actual_export, int_switch)
        
        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/dc2t.inc'
        include 'ipfinc/dcmt.inc'
        include 'ipfinc/intchg.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ordsta.inc'

        common /is_batch / is_batch

        common /area_data/ gentot, lodtot, lostot, arcact(4*MAXCAR),
     &                     arcflg(4*MAXCAR) 
        real gentot, lodtot, lostot, arcact
        integer arcflg

        logical found

        sched_export = 0.0
        actual_export = 0.0
        int_switch = 0
        calcintflo = 0

        jt = 1
        do while (jt .le. ntotic .and. 
     &            arcint(1,jt) .ne. arcnam(ka1))
           jt = jt + 1
        enddo
        if (jt .gt. ntotic .or.
     &      arcint(1,jt) .ne. arcnam(ka1)) go to 100
        do while (jt .le. ntotic .and. 
     &            arcint(1,jt) .eq. arcnam(ka1) .and.
     &            arcint(2,jt) .ne. arcnam(ka2))
           jt = jt + 1
        enddo   
        if (jt .le. ntotic .and.
     &      arcint(1,jt) .eq. arcnam(ka1) .and.
     &      arcint(2,jt) .eq. arcnam(ka2)) then
          sched_export = arcinp(jt)
          actual_export = arcact(jt)
          int_switch = 1
        endif
  100   return
        end                    

