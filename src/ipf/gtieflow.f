C    @(#)gtieflow.f	20.3 2/13/96
C****************************************************************
C
C       File: gtieflow.f (Get TIE FLOw)
C       Purpose: Routine to compute the tie flow for entity TIE(*,JT)
C
C       Author: Walt Powell  Date: 10 March 1993
C       Called by: p_ldardata.f
C
C****************************************************************
C
        real function gtieflow (jt)
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/dc2t.inc'
        include 'ipfinc/dcmt.inc'
        include 'ipfinc/ordsta.inc'
 
        k1 = tie(1,jt)
        k2 = tie(7,jt)
        kdc = tie(9,jt)
        gtieflow = 0.0

        if (kdc .gt. 0) then
C
C          Check multi-terminal d-c
C
           do 100 mdc = 1, mtdcbs
 
              m1 = dcmtln(1,mdc)
              m2 = dcmtln(2,mdc)
              if (min0(m1,m2) .ne. min0(k1,k2)) go to 100
              if (max0(m1,m2) .eq. max0(k1,k2)) then
                 if (m1 .eq. k1) then
                    l1 = dcmtln(8,mdc)
                    l2 = dcmtln(9,mdc)
                 else
                    l1 = dcmtln(9,mdc)
                    l2 = dcmtln(8,mdc)
                 endif
 
                 v1 = dcmtbs(20,l1)
                 v2 = dcmtbs(20,l2)
                 gtieflow = v1 * (v1-v2) / dcmtln(4,mdc)
                 return
              endif
  100      continue
C
C          Check 2-terminal d-c arrays
C
           do 110 mdc = 1, kdtot
 
              if (dc2t(1,mdc) .eq. k1. and. dc2t(3,mdc) .eq. k2) then
                 gtieflow = 0.001 * dc2t(39,mdc) * dc2t(40,mdc)
                 return
 
              else if (dc2t(1,mdc) .eq. k2 .and. dc2t(3,mdc) .eq. k1)
     1           then
                 gtieflow = -0.001 * dc2t(39,mdc) * dc2t(41,mdc)
 
                 return
              endif
  110      continue
 
           write (*,120)
  120      format (' Abnormal termination: gtieflow - 110 ')
           return
 
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
           gtieflow = (ek*aik + fk*bik) * bmva
           return

        endif
        end
