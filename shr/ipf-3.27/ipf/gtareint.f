C    @(#)gtareint.f	20.2 1/15/98
C****************************************************************
C
C       File: gtareint.f (Get AREa INTerchange tie flow)
C       Purpose: Routine to compute the actual area export for entity 
C       AREA((,NB)
C
C       Author: Walt Powell  Date: 27 October 1997
C       Called by: ext_ged.f
C
C****************************************************************
C
        subroutine gtareint (nb, p_export, q_export, dc_pexport, 
     &                       dc_qexport, option)
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
 
        p_export = 0.0
        q_export = 0.0
        dc_pexport = 0.0
        dc_qexport = 0.0
 
        do jt = 1, jtie
          if (tie(2,jt) .eq. nb) then
            xsign = 1.0
          else if (tie(8,jt) .eq. nb) then
            xsign = -1.0
          else 
            go to 900
          endif
          k1 = tie(1,jt)
          k2 = tie(7,jt)
          kdc = tie(9,jt)
          if (kdc .gt. 0) then
C
C           Check multi-terminal d-c
C 
            do mdc = 1, mtdcbs
 
              m1 = dcmtln(1,mdc)
              m2 = dcmtln(2,mdc)
              if (min0(m1,m2) .eq. min0(k1,k2) .and.
     &            max0(m1,m2) .eq. max0(k1,k2)) then
                if (m1 .eq. k1) then
                  l1 = dcmtln(8,mdc)
                  l2 = dcmtln(9,mdc)
                else
                  l1 = dcmtln(9,mdc)
                  l2 = dcmtln(8,mdc)
                endif
 
                v1 = dcmtbs(20,l1)
                v2 = dcmtbs(20,l2)
                pflow = v1 * (v1-v2) / dcmtln(4,mdc)
                p_export = p_export + xsign * pflow
                dc_pexport = dc_pexport + xsign * pflow
              endif
            enddo
C
C           Check 2-terminal d-c arrays
C
            do mdc = 1, kdtot
 
              if (dc2t(1,mdc) .eq. k1. and. dc2t(3,mdc) .eq. k2) then
                pflow = 0.001 * dc2t(39,mdc) * dc2t(40,mdc)
                p_export = p_export + xsign * pflow
              else if (dc2t(1,mdc) .eq. k2 .and. dc2t(3,mdc) .eq. k1)
     &          then
                pflow = -0.001 * dc2t(39,mdc) * dc2t(41,mdc)
                p_export = p_export + xsign * pflow
                dc_pexport = dc_pexport + xsign * pflow
              endif
            enddo
          else 
            if (ordvlt .eq. 1) then
              kt = k1
              mt = k2
            else
              kt = inp2opt(k1)
              mt = inp2opt(k2)
            endif
            ek = e(kt)
            fk = f(kt)
            em = e(mt)
            fm = f(mt)
            gk11 = tie(3,jt)
            bk11 = tie(4,jt)
            gk12 = tie(5,jt)
            bk12 = tie(6,jt)
            aik = ek * gk11 - fk * bk11 + em * gk12 - fm * bk12
            bik = ek * bk11 + fk * gk11 + em * bk12 + fm * gk12
            pflow = (ek*aik + fk*bik) * bmva
            qflow = (-ek*bik + fk*aik) * bmva
            p_export = p_export + xsign * pflow
            q_export = q_export + xsign * qflow
          endif

  900     continue
        enddo
        end
