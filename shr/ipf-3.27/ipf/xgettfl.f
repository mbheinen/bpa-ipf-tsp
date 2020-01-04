C    @(#)xgettfl.f	20.3 2/13/96
        subroutine xgettfl (jt, pin, qin)
C                                                                  
C       This subroutine computes the line flow for area tie line JT
c       using the alternate base case data. (Reference: gettfl.for)
C                                               
C       Input parameters:                         
C                                               
C       JT     - index to OTIE.                    
C                                               
C       Output Parameters:                        
C                                               
C       PIN - Flow (MW) in tie line.
C       QIN - Flow (MVAR) in tie line.
C                                               
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/alt_case.inc'
 
        k1 = oltie(1,jt)
        k2 = oltie(7,jt)
        kdc = oltie(9,jt)
 
        if (kdc .gt. 0) then
C
C          Check multi-terminal d-c array.
C
           do 160 mdc = 1, omtdcbs
 
           m1 = okdcmtl(1,mdc)
           m2 = okdcmtl(2,mdc)
           if (min0(m1,m2) .ne. min0(k1,k2)) go to 160
           if (max0(m1,m2) .eq. max0(k1,k2)) then
              if (m1.eq.k1) then
                 l1 = okdcmtl(8,mdc)
                 l2 = okdcmtl(9,mdc)
              else
                 l1 = okdcmtl(9,mdc)
                 l2 = okdcmtl(8,mdc)
              endif
 
              v1 = odcmtbs(20,l1)
              v2 = odcmtbs(20,l2)
              pin =  v1 * (v1 - v2) / odcmtln(4,mdc)
              qin = 0.0
              return
           endif
  160      continue
C
C          Check 2-terminal d-c arrays
C
           do 194 mdc = 1,okdtot
 
           if (okdc2t(1,mdc) .eq. k1 .and. okdc2t(3,mdc) .eq. k2) then
 
              pin = 0.001*odc2t(39,mdc)*odc2t(40,mdc)
              qin = 0.0
              return
 
           else if (okdc2t(1,mdc) .eq. k2 .and. okdc2t(3,mdc) .eq. k1) 
     &        then
 
              pin = -0.001*odc2t(39,mdc)*odc2t(41,mdc)
              qin = 0.0
              return
 
           endif
  194      continue
 
           call erexit
 
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
           aik = ek*gk11-fk*bk11+em*gk12-fm*bk12
           bik = ek*bk11+fk*gk11+em*bk12+fm*gk12
           pin = (ek*aik+fk*bik)*bmva
           qin = (-ek*bik+fk*aik)*bmva
           return
 
       endif
       end
