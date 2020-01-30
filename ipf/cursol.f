C    @(#)cursol.f	20.6 7/18/96
        subroutine cursol (kerr)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		gkku(r*8), bkku(r*8), gkmu(r*8), bkmu(r*8), 
c		qnetu(r*8), pnetu(r*8) km, kmlen, ikmu
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		dpt(r*8), amtrx(r*8)
      include 'ipfinc/beta.inc'
c	Global variables used:
c		gkm(r*8), bkm(r*8), vk(r*8), kt, kta, max
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8)
      include 'ipfinc/dc.inc'
c	Global variables used:
c		dcbus(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		dptot(r*8), dqtot(r*8), datot(r*8), ddtot(r*8), 
c		kownt, kownta, kowntc, idswb, itsw, ntotx, idcsw
      include 'ipfinc/gamma.inc'
c	Global Variables used:
c		korder, kolum, rowh(r*8), rown(r*8), rowj(r*8), rowl(r*8)
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
c	Global variables used:
c		None
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
C	Global variables used: 
c		phse, xsen
      include 'ipfinc/tran.inc'
c	Global variables used:
c		tran(r*4), ltran(i*4), tap(r*8), 
      include 'ipfinc/xdata.inc'
c	Global variables used:
c		xdata(r*8)
 
        common /auxslk/ nxslck,xslck(2,MAXREI+MAXTIE),
     1                  kxslck(MAXREI+MAXTIE)
C
        integer fichsx
        dimension phtx(11,MAXPHS), iktemp(MAXREI+MAXTIE)
	double precision phtx

c	Single to double precision
c
c	Single to double precision.  Local variables
c
	double precision xl, xj, rh, va, ah, theta, ysq, rn, pkm,
     &                   rkm, an, aij, bij, rj, rl, dptk, dqtk, dptm, 
     &                   dqtm, cutoff, xn, xh, vm, ei, fi, ej, fj, dx1, 
     &                   dx2, tmax, tmin, xkm, angle, aold
C
        lprtsv=lprtsw
        fichsx=fichsw
        kerr = 0
        lprtsw=1
        if(kspare(16).ge.0) fichsw=1
C       
C     TEMPORARILY REPLACE ALL LTC PHASE SHIFTERS WITH CONSTANT  
C     TERMINAL INJECTION
C       
      iph = 0   
      if (itsw .eq. 0 .or. itsw .eq. 3) go to 370   
      do 360 jt = 1,ntota   
      if(ikk(1,jt) .ne. 1) go to 360
      if(ltran(10,jt) .ne. 3) go to 360 
      kt=ltran(1,jt)
      mt=ltran(9,jt)
      ksw = 0   
      angle = tap(jt)   
      if (tran(4,jt)-tran(5,jt) .gt. 0.50) go to 360
      angle = tap(jt)   
      dx1 = ddim(dble(tran(7,jt)),angle)   
      dx2 = ddim(angle,dble(tran(8,jt)))   
      if (dmin1(dx1,dx2) .le. 0.500) go to 360  
      pkm = 0.5*(tran(4,jt) + tran(5,jt))   
      jx = jt   
  320 ls = kmlen(kt) +km(kt)-1                  !uur
      do 330 l = km(kt), ls                       !uur
      if (ikmu(l) .eq. mt) go to 340               !uur
  330 continue  
      call erexit   
  340 iph = iph + 1 
      phtx(1,iph) = kt 
      phtx(2,iph) = mt 
      phtx(9,iph)=jx   
      phtx(3,iph) = pnetu(kt)                      !uur
      phtx(4,iph) = gkku(kt)                       !uur
      phtx(5,iph) = bkku(kt)                       !uur
      phtx(6,iph) = l  
      gkm = gkmu(l)                                !uur
      bkm = bkmu(l)                                !uur
C       
C     REMOVE PHASE SHIFTER FROM Y-MATRIX
C       
      cs = cos(angle)   
      sn = sin(angle)   
      phtx(7,iph) = gkm*cs + bkm*sn 
      phtx(8,iph) = -gkm*sn + bkm*cs
      gkku(kt) = gkku(kt) +phtx(7,iph)
      bkku(kt) =  bkku(kt) +phtx(8,iph)   
      if (idswb .ne. 0) write (dbug,350) iph,kt,mt,jt,pkm,angle,gkm,bkm 
     1 ,phtx(7,iph),phtx(8,iph) 
  350 format(' LTC PHS-SFT ',i2,2i5,i3,6e10.3)  
      pnetu(kt) = pnetu(kt) -pkm  
      gkmu(l) = 0.0
      bkmu(l) = 0.0
      if (ksw .ne. 0) go to 360 
      ksw = 1   
      kx = kt   
      kt = mt   
      mt = kx   
      angle = -angle
      pkm = -pkm
      jx = -jt
      go to 320
 
  360 continue
      do 362 i=1,iph
         if (phtx(9,i) .gt. 0) then
            kt = phtx(1,i)
            mt = phtx(2,i)
            phtx(10,i) = ikk(1,kt+ntota)
            phtx(11,i) = ikk(1,mt+ntota)
         endif
  362 continue
      do 364 i=1,iph
         if (phtx(9,i) .gt. 0) then
            kt = phtx(1,i)
            mt = phtx(2,i)
            ikk(1,kt+ntota) = 1
            ikk(1,mt+ntota) = 1
         endif
  364 continue
  370 continue
C
C     MODEL DC CONVERTERS WITH SIMPLE INJECTIONS
C       
      if (idcsw .eq. 0) go to 400   
      do 390 jckt = 1,idckt 
         nec=nckt(jckt)-1  
         ntdc = nckt(jckt+1) - nckt(jckt)  
         do 380 i = 1,ntdc 
            kt=dcbus(1,i+nec)
            mt=dcbus(3,i+nec)
            if (mt .eq. 0) go to 380  
            theta = dcbus(12,i+nec)   
            pnetu(kt) = -dcbus(19,i+nec)/bmva
            qnetu(kt) = -abs(pnetu(kt))*tan(theta)  
  380    continue  
  390 continue  
  400 continue  
C       
C     Temporarily set pseudo slack buses constraints to identity rows:  
C       
C                         1 * dT = dT(error)
C                         1 * dV = dV(error)
C
      do 410 i = 1,nxslck
         kt = kxslck(i)
         iktemp(i) = ikk(1,kt+ntota)
         ikk(1,kt+ntota) = 2
  410 continue
C
      cutoff = 0.0
      itlim = iopton(2)
      itcnt = 0
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
      qsw = 0.90
      tsw = 0.10
  500 itcnt = itcnt+1   
      if (itcnt .gt. itlim) then
         if (dptot + dqtot + datot + ddtot .gt. cutoff) kerr = 1
         go to 1390 
      endif 
      ikec = 1  
      kownt = 0 
      kownta = 0
      dptot = 0.0   
      dqtot = 0.0   
      datot = 0.0   
      ddtot = 0.0   
C       
C     Define Jacobian elements for bus constraints  
C       
      do 1020 kt = 1,ntot   
      kta = kt + ntota  
      ikkind(1,kta) = ikec  
      call curjac (qsw,tsw) 
      dptk = dpt(1,kta) 
      dqtk = dpt(2,kta) 
      mel = 0   
  640 mel = korder(mel) 
      mt = kolum(mel)   
      if (mt-kta) 740,644,642   
C       
C     Eliminate column MT from working row  
C       
  740 ik = ikkind(2,mt) 
      ikstop = ikkind(1,mt+1)   
      xn = amtrx(ik)
      ik = ik + 1   
      rh = rowh(mel)
      rn = rown(mel)-rh*xn  
      rj = rowj(mel)
      rl = rowl(mel)-rj*xn  
      amtrx(ikec)=mt
      amtrx(ikec+1)=rh  
      amtrx(ikec+2)=rn  
      amtrx(ikec+3)=rj  
      amtrx(ikec+4)=rl  
      ikec=ikec+5   
      dptm = dpt(1,mt)  
      dqtm = dpt(2,mt)  
      dptk = dptk-rh*dptm-rn*dqtm   
      dqtk = dqtk-rj*dptm-rl*dqtm   
      krw = mel 
C       
C     Perform Row MT elimination
C       
  800 if (ik .ge. ikstop) go to 640 
      ml = amtrx(ik)
      if (ml .gt. max) go to 980
  960 if (kolum(krw)-ml) 970,1010,990
  970 ko = krw
      krw = korder(krw)
      go to 960
 
 1010 mlc = krw
      go to 1000
 
  980 max = ml
      ko = lp
      lp = mend
  990 korder(mend) = korder(ko)
      kolum(mend) = ml
      korder(ko) = mend
      mlc = mend
      ko = mend 
      mend = mend + 1   
      rowh(mlc) = 0 
      rown(mlc) = 0 
      rowj(mlc) = 0 
      rowl(mlc) = 0 
 1000 continue  
      xh = amtrx(ik+1)  
      xj = amtrx(ik+2)  
      rowh(mlc) = rowh(mlc) - rh*xh - rn*xj 
      rowj(mlc) = rowj(mlc) - rj*xh - rl*xj 
      xn = amtrx(ik+3)  
      xl = amtrx(ik+4)  
      rown(mlc) = rown(mlc) - rh*xn - rn*xl 
      rowl(mlc) = rowl(mlc) - rj*xn - rl*xl 
      ik = ik + 5   
      go to 800 
C       
C     Error - no residual diagonal element  
C       
  642 call erexit   
C       
C     Normalize row 
C       
  644 rh = rowh(mel)
      rhin = 1.0/rh 
      an = rown(mel)*rhin   
      rj = rowj(mel)
      rl = rowl(mel)
      rlin = 1.0/(rl-rj*an) 
      amtrx(ikec)=rhin  
      amtrx(ikec+1)=rj  
      amtrx(ikec+2)=rlin
      ikec=ikec+3   
      ikkind(2,kta)=ikec
      dpt(1,kta) = dptk*rhin
      dpt(2,kta) = (dqtk - rj*dpt(1,kta))*rlin  
      amtrx(ikec) = an  
      ikec = ikec + 1   
  690 mel = korder(mel) 
      if (mel .eq. 0) go to 1020
      mt = kolum(mel)   
      ah = rowh(mel)*rhin   
      an = rown(mel)*rhin   
      amtrx(ikec) = mt  
      amtrx(ikec+1) = ah
      amtrx(ikec+2) = (rowj(mel) - rj*ah)*rlin  
      amtrx(ikec+3) = an
      amtrx(ikec+4) = (rowl(mel) - rj*an)*rlin
      ikec = ikec + 5
      go to 690
 
 1020 continue
      ikkind(1,ntotx) = ikec
C       
C     END OF BUS LOOP   
C       
      write (outbuf,1040) itcnt,dptot,dqtot,datot,ddtot,
     1   kownt,kowntc,ikkind(1,ntotx)   
 1040 format(' Starting',i2,2f14.5,13x,2f13.5,i7,6x,i6,20x,i10 )
      call prtout(1)
      call curbak (0)   
        
      ko = kownta/1000  
      if ( ko .ne. 0 ) then 
         write (outbuf,1130) ko 
 1130    format('+',101x,i7)
         call prtout(1) 
      endif 
      if (cutoff .eq. 0) then   
         cutoff = dptot + dqtot + datot + ddtot 
      else if (dptot + dqtot + datot + ddtot .gt. cutoff) then  
         kerr = 1   
         go to 1390 
      endif 
      if (kownt+kowntc .gt. 0) go to 500
 1390 continue  
C       
C     Restore pseudo slack bus constraints  
C       
      do 1400 i = 1,nxslck  
         kt = kxslck(i)
         ikk(1,kt+ntota) = iktemp(i)   
 1400 continue
C       
C     RESTORE PHASE SHIFTERS
C       
      if (iph .eq. 0) go to 2660
      do 2650 i = 1,iph 
      kt = phtx(1,i)   
      mt = phtx(2,i)   
      jt = phtx(9,i)   
      pnetu(kt) = phtx(3,i)
      gkku(kt) = phtx(4,i) 
      bkku(kt) = phtx(5,i) 
      l = phtx(6,i)
C       
C     DETERMINE NEW PHASE SHIFT ANGLE FROM VOLTAGE TORQUE ANGLES
C     TO UPDATE PHASE SHIFTER ANGLE 
C       
      jx = iabs (jt)
      if (jt .gt. 0) then   
        tmax = tran(7,jx)   
        tmin = tran(8,jx)   
      else  
        tmax = -tran(8,jx)  
        tmin = -tran(7,jx)  
      endif 
      gkm = phtx(7,i)   
      bkm = phtx(8,i)   
      ysq = gkm**2 + bkm**2 
      rkm = -gkm/ysq
      xkm = bkm/ysq 
      vk = dsqrt (e(kt)**2 + f(kt)**2)   
      vm = dsqrt (e(mt)**2 + f(mt)**2)   
      va = sqrt (vk*vm) 
      pkm = 0.5*(tran(4,jx) + tran(5,jx))   
      if (jt .lt. 0) go to 2610 
      aij = pkm*e(kt)/vk**2 
      bij = pkm*f(kt)/vk**2 
      ei = e(kt) - rkm*aij + xkm*bij
      fi = f(kt) - rkm*bij - xkm*aij
      ej = e(mt)
      fj = f(mt)
      go to 2620
 
 2610 aij = pkm*e(mt)/vm**2
      bij = pkm*f(mt)/vm**2
      ei = e(mt) - rkm*aij + xkm*bij
      fi = f(mt) - rkm*bij - xkm*aij
      ej = e(kt)
      fj = f(kt)
 2620 ak = atan2 (fi,ei)
      am = atan2 (fj,ej)
      aold = ak - am
      if (jt .lt. 0) aold = -aold   
      da = ddim (aold,tmax) - ddim (tmin,aold)
      anew = aold - da  
      cs = cos (anew)   
      sn = sin (anew)   
      gkmu(l) = gkm*cs - bkm*sn
      bkmu(l) = bkm*cs + gkm*sn
      if (jt .gt. 0) go to 2630 
C       
C     SECOND PASS -- UPDATE VOLTAGE AND TAP ARRAYS  
C       
      vk = va/vk
      if (ikk(1,kt+ntota) .eq. 1) vk = 1.0  
      e(kt) = vk*e(kt)  
      f(kt) = vk*f(kt)  
      vm = va/vm
      if (ikk(1,mt+ntota) .eq. 1) vm = 1.0  
      e(mt) = vm*e(mt)  
      f(mt) = vm*f(mt)  
      tap(jx) = -anew   
 2630 if (idswb .ne. 0) write (dbug,2640) i,kt,mt,jt,aold,anew,da,ak,
     &                                    am,va,vk,vm
 2640 format(' LTC PHS-SFT ',i2,3i5,8e10.3) 
 2650 continue  
      do 2652 i=1,iph   
      if (phtx(9,i) .gt. 0) then   
         kt = phtx(1,i)
         mt = phtx(2,i)
         ikk(1,kt+ntota) = phtx(10,i)  
         ikk(1,mt+ntota) = phtx(11,i)  
      endif 
 2652 continue  
 2660 continue  
C       
C     COMPUTE PHASE SHIFTER SENSITIVITIES   
C       
      if (itsw .eq. 0 .or. itsw .eq. 3) go to 2700  
      do 2690 jt = 1,ntota  
      phse(jt) = 0.0
      if (ltran(10,jt) .ne. 3) go to 2690   
      if (ikk(1,jt) .ne. 1) go to 2690  
      kt = ltran(1,jt)  
      mt = ltran(9,jt)  
      do 2670 i = nbslck+ntota,ntotx
 2670 dpt(1,i) = 0.0
      dpt(1,kt+ntota) = 1.0 
      dpt(1,mt+ntota) = -1.0
      call curbak (1)   
      phse(jt) = -dpt(1,kt+ntota) + dpt(1,mt+ntota) 
      if (idswb .ne. 0) write (dbug,2680) jt,kt,mt,phse(jt) 
 2680 format (' LTC PHS-SFT ',3i5,' SENSITIVITY ',e10.3)
 2690 continue  
 2700 continue  
C       
C     COMPUTE TYPE X-BUS SENSITIVITIES  
C       
      do 2750 jt = 1,kxtot  
      xsen(jt) = 0.0
      do 2710 i = nbslck+ntota,ntotx
 2710 dpt(1,i) = 0.0
      kt = xdata(1,jt)
      if (ntypu(kt) .ne. 11) go to 2750
      kta = kt + ntota  
      if (ikk(1,kta) .eq. 2) then   
         dpt(1,kta) = 1.0   
         call curbak (1)
         xsen(jt) = dpt(1,kta)  
      else  
         qsw = 0.0  
         tsw = 0.0  
         call curjac (qsw,tsw)  
         do 2720 i = 1,lp   
            mt = kolum(i)  
            if (mt .ne. kta) dpt(1,mt) = rowh(i)   
 2720    continue   
         call curbak (1)
         kl = 0 
         do 2730 i = 1,lp   
            mt = kolum(i)  
            if (mt .eq. kta) then  
               kl = i  
            else   
               xsen(jt) = xsen(jt) - rowh(i)*dpt(1,mt) 
            endif  
 2730    continue   
         xsen(jt) = 1.0/(rowh(kl) - xsen(jt))   
      endif 
      if (idswb .ne. 0) write (dbug,2740) jt,kt,xsen(jt)
 2740 format (' X BUS ',2i5,' SENSITIVITY ',e10.3)  
 2750 continue  
C       
      lprtsw=lprtsv 
      fichsw=fichsx 
C       
      return
      end   
