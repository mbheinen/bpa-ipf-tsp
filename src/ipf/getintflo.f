C    @(#)getintflo.f	20.4 8/30/00
C****************************************************************
C
C       File: getintflo.f (GET area INTertie FLOw)
C       Purpose: Routine to compute the actual intertie flow between 
C                entity arcint(1,nb) and arcint(2,nb) 
C
C       Author: Walt Powell  Date: 04 October 2000
C       Called by: ext_ged.f
C
C****************************************************************
C
        subroutine getintflo (ib, p_export, q_export, dc_pexport, 
     &                        dc_qexport, option)
        character *(*) option(10)
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/dc2t.inc'
        include 'ipfinc/dcmt.inc'
        include 'ipfinc/ordsta.inc'
        include 'ipfinc/prt.inc'
 
        logical finished

        p_export = arcinp(ib)
        q_export = 0.0
        dc_pexport = 0.0
        dc_qexport = 0.0
 
        ka1 = 0
        ka2 = 0
        jt = 1
        finished = .false.
        do while (.not. finished)
          if (arcnam(jt) .eq. arcint(1,ib)) then
            ka1 = jt
          else if (arcnam(jt) .eq. arcint(2,ib)) then
            ka2 = jt
          endif
          jt = jt + 1
          finished = (jt .gt. ntotc .or. (ka1 .gt. 0 .and. ka2 .gt. 0))
        enddo
        if (ka1 .eq. 0 .or. ka2 .eq. 0) then
          write (errbuf(1), 10000) arcint(1,ib), arcint(2,ib), ka1, ka2
10000     format (' Cannot find area1 or area2 in "I" record (', a10,
     &      1x, a10, ') ', 2i4)
          call prterx ('W', 1)
        else
C
C         Check multi-terminal d-c
C 
          do mdc = 1, mtdcbs
 
            m1 = dcmtln(1,mdc)
            m2 = dcmtln(2,mdc)
            if (min0 (jarzn(m1), jarzn(m2)) .eq. min0 (ka1, ka2) .and.
     &          max0 (jarzn(m1), jarzn(m2)) .eq. max0 (ka1, ka2)) then
              if (jarzn(m1) .eq. ka1) then
                l1 = dcmtln(8,mdc)
                l2 = dcmtln(9,mdc)
              else
                l1 = dcmtln(9,mdc)
                l2 = dcmtln(8,mdc)
              endif
 
              v1 = dcmtbs(20,l1)
              v2 = dcmtbs(20,l2)
              pflow = v1 * (v1-v2) / dcmtln(4,mdc)
              dc_pexport = dc_pexport + pflow
            endif
          enddo
C
C         Check 2-terminal d-c arrays
C
          do mdc = 1, kdtot
 
            m1 = dc2t(1,mdc)
            m2 = dc2t(3,mdc)
            if (min0 (jarzn(m1), jarzn(m2)) .eq. min0 (ka1, ka2) .and.
     &          max0 (jarzn(m1), jarzn(m2)) .eq. max0 (ka1, ka2)) then
              if (jarzn(m1) .eq. ka1) then
                pflow = 0.001 * dc2t(39,mdc) * dc2t(40,mdc)
                dc_pexport = dc_pexport + pflow
              else
                pflow = -0.001 * dc2t(39,mdc) * dc2t(41,mdc)
                dc_pexport = dc_pexport + pflow
              endif
            endif
          enddo
        endif

        return
        end
