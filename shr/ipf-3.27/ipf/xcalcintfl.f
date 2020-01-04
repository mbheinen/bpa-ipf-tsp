C    @(#)xcalcintfl.f	20.4 7/18/96
C****************************************************************
C
C   File: xcalcintfl.f
C
C   Purpose: Routine to calculate selected area intertie "I" flows.
c
C   Author: Walt Powell  
C   Date: 5 March 1996
C   Called by: gtaltopt.f
C
C****************************************************************
C
        integer function xcalcintfl (ka1, ka2, sched_export, 
     &      actual_export, int_switch)
        
        include 'ipfinc/parametr.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/lfiles.inc'

        common /is_batch / is_batch

        common /xarea_data/ gentot, lodtot, lostot, arcact(4*MAXCAR),
     &                      arcflg(4*MAXCAR) 
        real gentot, lodtot, lostot, arcact
        integer arcflg

        logical found

        sched_export = 0.0
        actual_export = 0.0
        int_switch = 0
        xcalcintfl = 0

        jt = 1
        do while (jt .le. ontotic .and. 
     &            oarcint(1,jt) .ne. oarcnam(ka1))
           jt = jt + 1
        enddo
        if (jt .gt. ontotic .or.
     &      oarcint(1,jt) .ne. oarcnam(ka1)) go to 100
        do while (jt .le. ontotic .and. 
     &            oarcint(1,jt) .eq. oarcnam(ka1) .and.
     &            oarcint(2,jt) .ne. oarcnam(ka2))
           jt = jt + 1
        enddo   
        if (jt .le. ontotic .and.
     &      oarcint(1,jt) .eq. oarcnam(ka1) .and.
     &      oarcint(2,jt) .eq. oarcnam(ka2)) then
          sched_export = oarcinp(jt)
          actual_export = arcact(jt)
          int_switch = 1
        endif
  100   return
        end                    

