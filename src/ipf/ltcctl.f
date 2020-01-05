C    @(#)ltcctl.f	20.4 7/18/96
        subroutine ltcctl (jt, ityp, lsw, kc, pkm, dpkm)
C
C       compute the Jacobian elements for LTC transformers.
C
C       ITYP and LSW define the constraint:
C
C       LSW = 0 and ITYP = x: 1.0*Dt                 = Dt(error)
C       LSW = 1 and ITYP = x: 0.0001*Dt +    1.0*Dv  = Dv(error)
C       LSW = 1 and ITYP = 6: 0.0001*Dt +    1.0*Dv  = DV(error)
C       LSW = 1 and ITYP = 7: 0.0001*Dt +    1.0*Dv  = DV(error)
C       LSW = 2 and ITYP = 2: 1.0*Dt + RH*Da + RN*Dv = Dq(error)
C       LSW = 2 and ITYP = 5: 1.0*Dt + RH*Da + RN*Dv = Dq(error)
C       LSW = 2 and ITYP = 3: 1.0*Da + RH*Da + RN*Dv = Dp(error)
C       LSW = 3 and ITYP = 8: 1.0*DT + J'*DA + L'*DV + ... = DQk'
C       LSW = 4 and ITYP = 9: 1.0*DT + J'*DA + L'*DV + ... = DQm'
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
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tran.inc'
 
C     Double precision conversion
C
      double precision rh, rj
C       
      if (lsw .eq. 0) then

C       Implement LTC adjustment for direct T control in Jacobian 

        lp = 1  
        kolum(lp) = jt  
        rowh(lp) = 1.0  
        rown(lp) = 0
        rowj(lp) = 0.0  
        rowl(lp) = 1.0  
        dpt(1,jt) = pkm 
        dpt(2,jt) = 0.0 

      else if (lsw .eq. 1) then   

C       Implement LTC adjustment for direct V control in Jacobian 

        lp = 1  
        kolum(lp) = jt  
        rowh(lp) = 0.00001   
        rown(lp) = 0
        rowj(lp) = 0.0  
        rowl(lp) = 1.0  
        lp = lp + 1 
        kolum(lp) = kc + ntota  
        rowh(lp) = 0.0  
        rown(lp) = 1.0  
        rowj(lp) = 0.0  
        rowl(lp) = 0.0  
        dpt(1,jt) = pkm
        dpt(2,jt) = 0.0 

      else if (lsw .eq. 2) then

C       Implement LTC adjustment for direct Qkm control in Jacobian 

        kt=ltran(1,jt)  
        mt=ltran(9,jt)  
        ek=e(kt)
        fk=f(kt)
        em=e(mt)
        fm=f(mt)
        la1=ltran(3,jt) 
        gkm = gkmu(la1)                            
        bkm = bkmu(la1)                            

C       Determine 2-port admittances

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

C       Compute currents, injections 

        aim = em*gkm - fm*bkm   
        bim = em*bkm + fm*gkm   
        rh = -ek*bim + fk*aim   
        rj = -ek*aim - fk*bim   
        aim = aim + ek*gkktx - fk*bkktx 
        bim = bim + ek*bkktx + fk*gkktx 

C       Compute Jacobian elements 

        if (ityp.eq.3) then 
           pkm=ek*aim+fk*bim
           a1=-rh   
           a2=pkm+gkktx*(ek**2+fk**2)   
           a3=-rj   
        else

C          "PKM" below is a misnomer; it is actually "QKM".

           pkm=-ek*bim+fk*aim   
           a1=-rj   
           a2=pkm-bkktx*(ek**2+fk**2)   
           a3 = rh  
        endif   

C       Compute residual "DPKM"  

        if (ityp.eq.5) then 
           dpkm = tran(4,jt) - pkm  
        else if (ityp.eq.1) then
           dpkm = -pkm  
        else
           dpkm = -dim(pkm,tran(4,jt)) + dim(tran(5,jt),pkm)
           if (abs (dpkm) .le. option(6) .and.  
     1         tran(4,jt)-tran(5,jt) .gt.2.0*option(6)) then
              dpkm = 0.0
           endif
        endif   

C       Store Jacobian elements

        lp = 1  
        kolum(lp) = jt  
        rowh(lp) = rh   
        rowj(lp) = 0.0  
        rown(lp) = 0.0  
        rowl(lp) = 1.0  
        i1 = min0(kt,mt)
        i2 = max0(kt,mt)
  330   lp = lp + 1 
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
        dpt(1,jt) = dpkm
        dpt(2,jt) = 0.0 

      else if (lsw .eq. 3 .or. lsw .eq. 4) then

C       Implement LTC adjustment for direct Qk (lsw = 3) or 
C       Qm (lsw = 4) control in Jacobian 

        if (lsw .eq. 3) then
          kt = ltran(1,jt)  
          dqkm = dpkm          
        else
          kt = ltran(9,jt)  
          dqkm = -dpkm          
        endif

C       Retrieve Jacobian elements for dQk/dx or dQm/dx

        call jacbus (kt,1)   
        lp = korder(0)
        kl = lp
        do while (lp .gt. 0)
          rowh(lp) = rowj(lp) 
          rown(lp) = rowl(lp) 
          rowj(lp) = 0.0  
          rowl(lp) = 0.0
          lp = korder(lp)
        enddo
        rowl(kl) = 1.0
        dpt(1,jt) = dqkm
        dpt(2,jt) = 0.0 
      endif 
      return
      end   
