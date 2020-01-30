C    @(#)senctl.f	20.3 2/13/96
        subroutine senctl (jt, ityp, lsw, kc, pkm, dpkm)
C
C       compute the Jacobian elements for LTC transformers.
C
C       ITYP and LSW define the constraint:
C
C       LSW = 2 and ITYP = 2:   1.0*Dt + RH*Da + RN*Dv = Dq(error)
C       LSW = 2 and ITYP = 5:   1.0*Dt + RH*Da + RN*Dv = Dq(error)
C       LSW = 2 and ITYP = 3:   1.0*Da + RH*Da + RN*Dv = Dp(error)
C
C       LSW = 1 and ITYP = x:   0.0001Da +     1.*Dv = Dv(error)
C
C       LSW = 0 and ITYP = x:   1.0*Dt                 = Dt(error)
C
C       Input parameters:  LSW,ITYP,LSW,KC (and for LSW = 1,2, PKM)
C       Output parameters: for LSW = 2, PKM and DPKM
C
C       Common Arrays:  KOLUM,ROWH,ROWJ,ROWL,and ROWN
C       Common Variables: LP
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xtran.inc'
 
        if (lsw.eq.2) then
           kt=ltran(1,jt)
           mt=ltran(9,jt)
           if (ordltc .eq. 1) then
              kt = inp2opt(kt)
              mt = inp2opt(mt)
           endif
           ek=e(kt) 
           fk=f(kt) 
           em=e(mt) 
           fm=f(mt) 
           if (xtrflg .eq. 0) then  
              la1 = ltran(3,jt) 
              gkm = gkmu(la1)                      !uur
              bkm = bkmu(la1)                      !uur
           else if (xtrflg .eq. 1) then 
              la1 = ltran(3,jt) 
              gkm = gkmu(la1)                      !uur
              bkm = bkmu(la1)                      !uur
           else 
              la1 = xtran(1,jt) 
              gkm = gkmu(la1)                      !uur
              bkm = bkmu(la1)                      !uur
           endif
C       
C          Determine 2-port admittances 
C       
           if (ityp.eq.3) then  
              cs = cos(tap(jt)) 
              sn = sin(tap(jt)) 
              if (tran(6,jt).eq.0.0) then   
                 tek = 1.0  
              else  
                 tek = 1.0/tran(6,jt)   
              endif 
              gkktx = (-cs*gkm-sn*bkm)*tek  
              bkktx = (-cs*bkm+sn*gkm)*tek  
           else 
              gkktx = -gkm/tap(jt)  
              bkktx = -bkm/tap(jt)  
           endif
C       
C          Compute currents, injections 
C       
           aim = em*gkm - fm*bkm
           bim = em*bkm + fm*gkm
           rh = -ek*bim + fk*aim
           rj = -ek*aim - fk*bim
           aim = aim + ek*gkktx - fk*bkktx  
           bim = bim + ek*bkktx + fk*gkktx  
C       
C          Compute Jacobian elements
C       
           if (ityp .eq. 3) then  
              pkm = ek * aim + fk * bim 
              a1 = -rh 
              a2 = pkm + gkktx * (ek ** 2 + fk ** 2)
              a3 = -rj
           else 
              pkm = -ek * bim + fk * aim
              a1 = -rj
              a2 = pkm - bkktx * (ek ** 2 + fk ** 2)
              a3 = rh   
           endif
C       
C          Store Jacobian elements  
C       
           lp = 1   
           kolum(lp) = jt   
           rowh(lp) = rh
           rowj(lp) = 0.0   
           rown(lp) = 0.0   
           rowl(lp) = 1.0   
           i1 = min0(kt,mt) 
           i2 = max0(kt,mt) 
  330      lp = lp + 1  
           ik = i1 + ntota  
           kolum(lp) = ik   
           if (i1.eq.kt) then   
              rowh(lp) = a1 
              rown(lp) = a2 
              rowj(lp) = 0.0
              rowl(lp) = 0.0
           else 
              rowh(lp) = -a1
              rown(lp) = a3 
              rowj(lp) = 0.0
              rowl(lp) = 0.0
           endif
           if (i1.ne.i2) then   
              i1 = i2   
              go to  330
           endif
        else if (lsw.eq.1) then 
C       
C          Simulate auto adjustment in Jacobian 
C       
           lp = 1   
           kolum(lp) = jt   
           rowh(lp) = 0.0001
           rown(lp) = 0 
           rowj(lp) = 0.0   
           rowl(lp) = 1.0   
           lp = lp + 1  
           kolum(lp) = kc + ntota   
           rowh(lp) = 0.0   
           rown(lp) = 1.0   
           rowj(lp) = 0.0   
           rowl(lp) = 0.0   
        else
C       
C          Simulate manual adjustment in Jacobian   
C       
           lp = 1   
           kolum(lp) = jt   
           rowh(lp) = 1.0   
           rown(lp) = 0 
           rowj(lp) = 0.0   
           rowl(lp) = 1.0   
        endif   
        return  
        end 
