C    @(#)curjac.f	20.6 7/18/96
        subroutine curjac (qsw,tsw)
 
C       compute Jacobian elements for bus KT
C       constraints.
C
C       QSW weighs Q-constraints with V-constraints:
C
C          QSW * (Q-constraints) + (1 - QSW) * (V-constraints)
C         (QSW = 0 treats all PQ nodes as PV nodes)
C
C       TSW determines the weight on V/theta constraints:
C
C          TSW * (P-constraints) + (1 - TSW) * (Theta-constraints)
C          TSW * (Q-constraints) + (1 - TSW) * (V-constraints)
C         (TSW = 0 treats all BJ, BK, and BL nodes as V/theta nodes)
C
C       Note: TSW is incompatible with S-contraints - it creates
C             a singular system.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/errbus.inc'
      include 'ipfinc/factor.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
 
 
C
        common /auxslk/ nxslck,xslck(2,MAXREI+MAXTIE),
     1                  kxslck(MAXREI+MAXTIE)
c
c***kln	Local double precision variables
c
	double precision b12, g12, rn, rj, rh, rl, imi, imr, iki
	double precision ikr, abdp, abdq, pin, ek, em, fk, fm
	double precision v1, v2, rhmax, rlmax, factor, vksq, angle
C
c***KLN Now doubles        REAL IKR,IKI,IMR,IMI
C
        lp = 0
        kl = 0
        korder(0) = 0
        kolum(0) = 0
        mend = lp + 1   
        kta = kt+ntota  
        ek = e(kt)  
        fk = f(kt)  
        vksq = ek**2 + fk**2
        vk = sqrt (vksq)
        ls = km(kt)-1 +kmlen(kt)
C                                     Determine type of constraint:
C        1. PQ or PV
C        2. SQ or SV
C        3. Identity (passive d-c node or system slack bus) 
C       
        if (ntypu(kt).eq. 3 .or.(ntypu(kt).eq. 12  
     +   .and. kmlen(kt) .eq. 0)) then            !uua
C       
C          Process constraints for passive BM nodes or for system   
C          slack buses. 
C       
           lp = lp + 1  
           kl = lp  
           mta = kta
           max = mta
           mend = lp + 1
           ko = lp  
           kolum(kl) = kta  
           korder(0) = 1
           korder(kl) = 0   
           rowh(kl) = 1.0   
           rown(kl) = 0.0   
           rowj(kl) = 0.0   
           rowl(kl) = 1.0   
C       
C          Distinquist between passive d-c buses and system slack bus   
C          buses.   
C       
           jt = jckikk(kta,9)   
           if (jt .gt. 0) then  
              if (vk .gt. 0.0) then 
                 angle = atan2 (fk,ek)  
              else  
                 angle = 0.0
              endif 
              dpt(1,kta) = xslck(2,jt) - angle  
              dpt(2,kta) = (xslck(1,jt) - vk) / vk  
           else 
              dpt(1,kta) = 0.0  
              dpt(2,kta) = 0.0  
           endif
           abdp = dabs (dpt(1,kta))  
           abdq = dabs (dpt(2,kta))  
           ddtot = ddtot + abdp + abdq  
           go to 900
        endif   
C       
C       Process nodal constraints.  
C       
        ikr = 0.0   
        iki = 0.0   
        lsw = 0 
C       
C       Process branches
C       
        do 290 l = km(kt), ls                      
        mt = ikmu(l)                                
        mta = mt + ntota
        lp = lp + 1 
        if (lsw .eq. 0) then
           if (mt .gt. kt) then 
              kl = lp   
              kolum(lp) = kta   
              korder(lp) = lp + 1   
              lp = lp + 1   
              lsw = 1   
           endif
        endif   
        kolum(lp) = mta 
        korder(lp) = lp + 1 
        
        em = e(mt)  
        fm = f(mt)  
        g12 = gkmu(l)                               
        b12 = bkmu(l)                               
        imr = em*g12 - fm*b12   
        imi = em*b12 + fm*g12   
        ikr = ikr + imr 
        iki = iki + imi 
        rh = imr*fk - imi*ek
        rn = imr*ek + imi*fk
                rl = rh 
        if (ikk(1,kta) .eq. 2 ) then
           rowh(lp) = rh
           rown(lp) = rn
           rowj(lp) = rj
           rowl(lp) = rl
        else
           rowh(lp) = rh
           rown(lp) = rn
           rowj(lp) = 0.0   
           rowl(lp) = 0.0   
        endif   
  290   continue
C       
C       Process diagonal
C       
        if (kl .eq. 0) then 
           lp = lp + 1  
           kl = lp  
           kolum(lp) = kta  
           korder(lp) = lp + 1  
           mta = kta
        endif   
        imr = inetr(kt) * vk                       
        imi = ineti(kt) * vk                       
        pk = gkku(kt) * vksq + ikr*ek + iki*fk + imr    
        qk = -bkku(kt) * vksq + ikr*fk - iki*ek - imi   
        rh = -qk - bkku(kt) * vksq - imi            
        rn = pk + gkku(kt) * vksq                   
        if (ikk(1,kta) .eq. 2) then 
           rj = pk - gkku(kt) * vksq -imr           
           rl = qk -bkku(kt) * vksq                 
           dpt(1,kta) = pnetu(kt) -pk               
           dpt(2,kta) = qnetu(kt) -qk               
        else
           rj = 0.0 
           rl = 1.0 
           dpt(1,kta) = pnetu(kt) -pk               
           dpt(2,kta) = 0.0 
        endif   
        rowh(kl) = rh   
        rown(kl) = rn   
        rowj(kl) = rj   
        rowl(kl) = rl   
        korder(0) = 1   
        korder(lp) = 0  
        max = mta   
        mend = lp + 1   
        ko = lp 
C       
C       Process Sx constraints. Assure that all P-constraints   
C       are set to zero.
C       
        jt = jckikk(kta,3)  
        if ((jt .eq. 0) .or. (tsw .gt. 0.0)) then   
           abdp = dabs (dpt(1,kta))  
           abdq = dabs (dpt(2,kta))  
           dptot = dptot + abdp 
           dqtot = dqtot + abdq 
           if (dmax1(abdp,abdq).gt.option(4)) kownt = kownt + 1 
        else
           do 500 lp = 1,mend - 1
              rowh(lp) = 0.0
              rown(lp) = 0.0
  500      continue
           dpt(1,kta) = 0.0
           abdp = dabs (dpt(1,kta))
           abdq = dabs (dpt(2,kta))
           dptot = dptot + abdp
           dqtot = dqtot + abdq
           if (dmax1(abdp,abdq).gt.option(4)) kownt = kownt + 1
           lp = 0
           ko = lp
           lp = korder(lp)
           xatot = 0.0
           j1 = karea(3,jt)
           js = karea(4,jt) + j1 - 1
           do 700 j = j1,js 
           ix = kaloc(j)
           iax = iabs(ix)   
           k1 = tie(1,iax) 
           k2 = tie(7,iax) 
           mt = k1  
           if (ix .lt. 0) mt = k2   
           ka1 = tie(2,iax)
           kdc = tie(9,iax)
           if (kdc .gt. 0) then 
              if (ix .lt. 0) go to 700  
              kd = kdc  
  530         k1x = dmin1 (dcline(1,kd),dcline(2,kd))
              k2x = dmax1 (dcline(1,kd),dcline(2,kd))
              if (k1x .ne. min0(k1,k2)) then
                 if (kd .ne. kdc) call erexit   
                 if (mtdcln .eq. 0) call erexit 
                 kd = kdc + mtdcln  
                 go to 530  
              else if (k2x .ne. max0(k1,k2)) then   
                 call erexit
              endif 
              if (k1 .eq. dcline(1,kd)) then
                 l1 = dcline(8,kd)
                 l2 = dcline(9,kd)
              else
                 l1 = dcline(9,kd)
                 l2 = dcline(8,kd)
              endif
              v1 = dcbus(20,l1)
              v2 = dcbus(20,l2)
              pin = v1 * (v1 - v2) / (dcline(4,kd) * bmva)
              if (jt .ne. ka1) pin = -pin
              xatot = xatot + pin
           else
              mta = mt + ntota
  590         if (kolum(lp) - mta) 600,620,610
  620         ko = lp
              go to 630
 
  600         ko = lp
              lp = korder(lp)
              if (lp .ne. 0) go to 590
              max = mta
  610         korder(mend) = korder(ko)
              korder(ko) = mend
              kolum(mend) = mta
              ko = mend
              lp = mend
              mend = mend + 1
              rowh(lp) = 0.0
              rown(lp) = 0.0
              rowj(lp) = 0.0
              rowl(lp) = 0.0
 
  630         ek = e(k1)
              fk = f(k1)
              em = e(k2)
              fm = f(k2)
              vksq = ek*ek + fk*fk  
              g12 = tie(5,iax)  
              b12 = tie(6,iax)  
              ikr = g12*em - b12*fm 
              iki = b12*em + g12*fm 
              rh = -ek*iki + fk*ikr 
              rn = ek*ikr + fk*iki  
              pin = rn + vksq*tie(3,iax)
              if (ix.gt.0) then 
                 rh = -rh   
                 rn = rn + 2.0*vksq*tie(3,iax)  
              endif 
              if (ka1.ne.jt) then   
                 pin = -pin 
                 rh = -rh   
                 rn = -rn   
              endif 
              rowh(lp) = rowh(lp) + (1.0 - tsw) * rh
              rown(lp) = rown(lp) + (1.0 - tsw) * rn
  690         if (ix.gt.0) xatot = xatot + pin  
           endif
  700      continue
 
           abdp = abs (area(2,jt) - xatot)
           dpt(1,kta) = (1.0 - tsw) * (area(2,jt) - xatot)
           datot = datot + abdp
           if (abs(abdp) .gt. option(5)) kowntc = kowntc + 1
        endif   
C       
C       Determine whether Px or Sx constraints should be Theta- 
C       or V-weighed.   
C       
  800   jt = jckikk(kta,9)  
        if (jt .gt. 0) then 
           pfact = tsw  
           qfact = tsw  
        else if (ikk(1,kta) .eq. 2) then
           jt = 0   
           pfact = 1.0  
           qfact = qsw  
        else
           go to 900
        endif
C
C       Locate diagonal element
C
        rhmax = 0.0
        rlmax = 0.0
        do 810 l = 1,mend-1
           rhmax = rhmax + dabs (rowh(l))
           rlmax = rlmax + dabs (rowl(l))
           rowh(l) = rowh(l) * pfact
           rown(l) = rown(l) * pfact
           rowj(l) = rowj(l) * qfact
           rowl(l) = rowl(l) * qfact
           if (kolum(l) .eq. kta) kl = l
  810   continue
        dpt(1,kta) = dpt(1,kta) * pfact 
        dpt(2,kta) = dpt(2,kta) * qfact 
        if (rhmax .eq. 0.0) rhmax = 1.0 
        if (rlmax .eq. 0.0) rlmax = 1.0 
        if (kolum(kl) .ne. kta) then
           ko = 0   
           kl = korder(ko)  
  820      if (kolum(kl) - kta) 830, 850, 840   
  830      ko = kl  
           kl = korder(kl)  
           if (kl .ne. 0) go to 820 
           max = kta
  840      korder(mend) = korder(ko)
           korder(ko) = mend
           kolum(mend) = kta
           ko = mend
           kl = mend
           mend = mend + 1  
           rowh(kl) = 0.0   
           rown(kl) = 0.0   
           rowj(kl) = 0.0   
           rowl(kl) = 0.0   
  850      continue 
        endif   
        ek = e(kt)  
        fk = f(kt)  
        vk = sqrt (ek**2 + fk**2)   
        if (vk .gt. 0.0) then   
           angle = atan2 (fk,ek)
        else
           angle = 0.0  
        endif   
        if (jt .eq. 0) then 
           factor = rlmax * ( 1.0 - qfact ) 
           factor = sign (factor,rowl(kl))  
           rowl(kl) = rowl(kl) + factor 
           nb = opt2inp(kt)   
           dpt(2,kta) = dpt(2,kta) + factor * (vstart(nb) - vk) / vk
        else
           factor = rhmax * ( 1.0 - pfact ) 
           factor = sign (factor,rowh(kl))  
           rowh(kl) = rowh(kl) + factor 
           dpt(1,kta) = dpt(1,kta) + factor * (xslck(2,jt) - angle) 
           factor = rlmax * ( 1.0 - qfact ) 
           factor = sign (factor,rowl(kl))  
           rowl(kl) = rowl(kl) + factor 
           dpt(2,kta) = dpt(2,kta) + factor * (xslck(1,jt) - vk) / vk   
           ddtot = ddtot + abs (xslck(2,jt) - angle)
     1                   + abs (xslck(1,jt) - vk) / vk  
        endif   
C       
C       Align indices such that LP = KORDER(KO) and KOLUM(LP) = MAX.
C       
  900   lp = 0  
  902   ko = lp 
        lp = korder(lp) 
        if (lp .gt. 0) then 
           if (kolum(lp) .eq. max) return   
           go to 902
        else
           call erexit  
        endif   
        end 
