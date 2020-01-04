C    @(#)xgtieflow.f	20.3 2/13/96
C****************************************************************
C
C       File: xgtieflow.f (Get TIE FLOw)
C       Purpose: Routine to compute the tie flow for entity OTIE(*,JT)
C
C       Author: Walt Powell  Date: 10 March 1993
C       Called by: p_ldardata.f
C
C****************************************************************
C
        real function xgtieflow (jt)
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/alt_case.inc'
 
        xgtieflow = 0.0
        k1 = oltie(1,jt)
        k2 = oltie(7,jt)
        kdc = oltie(9,jt)
        if (kdc .gt. 0) then
C
C          Check multi-terminal d-c
C
           do 100 mdc = 1, omtdcbs
 
              m1 = okdcmtl(1,mdc)
              m2 = okdcmtl(2,mdc)
              if (min0(m1,m2) .ne. min0(k1,k2)) go to 100
              if (max0(m1,m2) .eq. max0(k1,k2)) then
                 if (m1 .eq. k1) then
                    l1 = okdcmtl(8,mdc)
                    l2 = okdcmtl(9,mdc)
                 else
                    l1 = okdcmtl(9,mdc)
                    l2 = okdcmtl(8,mdc)
                 endif
 
                 v1 = odcmtbs(20,l1)
                 v2 = odcmtbs(20,l2)
                 xgtieflow = v1 * (v1-v2) / odcmtln(4,mdc)
                 return
              endif
  100      continue
C
C          Check 2-terminal d-c arrays
C
           do 110 mdc = 1, okdtot
 
              if (okdc2t(1,mdc) .eq. k1. and. 
     &            okdc2t(3,mdc) .eq. k2) then
                 xgtieflow = 0.001 * odc2t(39,mdc) * odc2t(40,mdc)
                 return
 
              else if (okdc2t(1,mdc) .eq. k2 .and. 
     &                 okdc2t(3,mdc) .eq. k1) then
                 xgtieflow = -0.001 * odc2t(39,mdc) * odc2t(41,mdc)
 
                 return
              endif
  110      continue
 
           write (*,120)
  120      format (' Abnormal termination: xgtieflow - 110 ')
           return
 
        else
 
           kt = oinp2opt(k1)
           mt = oinp2opt(k2)
           ek = olde(kt)
           fk = oldf(kt)
           em = olde(mt)
           fm = oldf(mt)
           gk11 = otie(3,jt)
           bk11 = otie(4,jt)
           gk12 = otie(5,jt)
           bk12 = otie(6,jt)
           aik = ek * gk11 - fk * bk11 + em * gk12 - fm * bk12
           bik = ek * bk11 + fk * gk11 + em * bk12 + fm * gk12
           xgtieflow = (ek*aik + fk*bik) * bmva
           return

        endif
        end
