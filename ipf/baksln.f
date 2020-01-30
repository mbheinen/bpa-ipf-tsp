C    @(#)baksln.f	20.9 10/13/99
      subroutine baksln(ind)
      dimension ind(2,*)  
 
C     This subroutine normally performs a downwards and back solution
C     using the factored A or C matrix. If NTOTZT < NTOTX, an unusual
C     solution sequence is performed in the A matrix:   
C       
C     1. Perform the elimination and normalization operations on
C        rows 1 to NTOTZT - 1.  
C     2. Perform reduction on rows NTOTZT to NTOTX - 1. 
C     3. Solve the reduced system NTOTZT to NTOTX - 1 using a LP
C        (DECPLP).  Its purpose is to optimize the LTC phase shifter
C        which is subject to inequality constraints.  The objective
C        function is real power flow penalty and phase shift angle
C        penalty.  The latter optimizes system losses by minimizing
C        circulating flow.
C     4. Using the computed values of phase shift angle, complete
C        the elimination and normalization on rows NTOTZT to NTOTX - 1.
C     5. Perform the back solution in the normal was.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		gkmu, bkmu, gkku, bkku, qnetu, pnetu, ineti, inetr
      include 'ipfinc/amtrx.inc'
c	Global variables changed:
c		dpt(r*8), amtrx (r*8)
      include 'ipfinc/area.inc'
c	Global variables used:
c		area(r*4), karea
      include 'ipfinc/beta.inc'
c	Global variables used:
c		gkm (r*8), bkm(r*8), qk(r*8), aik(r*8), bik(r*8), pk (r*8),
c		qk (r*8), kt, kta, msw
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e (r*8), f(r*8)
      include 'ipfinc/dc.inc'
c	Global variables used:
c		dcbus (r*8), dcline (r*8) 
      include 'ipfinc/dcsln.inc'
c	Global variables used:
c		akm (r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		dqtot(r*8), dptot(r*8), datot(r*8) 
      include 'ipfinc/errbus.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ntotzt.inc'
c	Global variables used:
c		blp(r*8), ntotzt
      include 'ipfinc/optim1.inc'
c	Global variables used:
c		txtie(r*8)
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
c	Global variables used:
c		tran(r*4), 
      include 'ipfinc/intbus.inc'
c

      common/bksln/icount 
c
c	Local Variables:
c
      double precision dt, tt, ek, rh 
c
      integer status
c
      real temp
c
      icount = 0            !  BEGIN DOWNWARD OPERATION 
      ierr=0
      kta=nbslck+ntota  
  100 kta=kta+1 
      if (kta.eq.ntotx) go to 382   
      if (ikk(1,kta).eq.0) go to 120
      if (msw.eq.0) go to 110
      if (ikk(1,kta).eq.1) go to 120
      go to 130
 
  110 jt=jckikk(kta,3)
c
c     Disable area interchange to improve initial convergence
c
c     if (jt.gt.0) go to 240
      go to 130
 
  120 dpt(1,kta)=0.0
      if (ind(1,kta).lt.ind(2,kta)) go to 360
      go to 100
 
  130 kt=kta-ntota
      if (kt.gt.ntot) go to 330
      ek=e(kt)
      fk=f(kt)
      dt=0.0
      dx = 0.0  
      aik=0.0   
      do 180 l = km(kt), km(kt)-1+kmlen(kt)   
      mt = ikmu(l)                                  
      em=e(mt)  
      fm=f(mt)  
      gkm = gkmu(l)                                 
      bkm = bkmu(l)                                 
      psi=fk-fm 
      if (msw.eq.0) goto 150
      dt = dt - em*(gkm*sin(psi) + bkm*(1.0-cos(psi)))  
      if (ikk(1,mt+ntota).eq.1) go to 140   
      aik = aik + em*bkm
      go to 180
 
  140 dt = dt + em*bkm
      go to 180
 
  150 if (mt.le.nbslck) go to 160
      aik=aik+em*psi*bkm
      dx = dx + akm(mt)*psi*bkm
      go to 170
 
  160 aik = aik + em*fk*bkm
      dx = dx + akm(mt)*fk*bkm
      dt = dt + em*fm*bkm
  170 dt=dt-em*(gkm*cos(psi)+bkm*(sin(psi)-psi))
  180 continue  
      rh = 0.0  
      ix = jckikk(kta,10)   
      if (ix.eq.0) go to 220
C       
C     A bus tie is connected to bus 
C       
  190 xh = 1.0  
      if (txtie(8,ix).lt.0) xh = -1.0  
      mt = dabs (txtie(8,ix)) - ntota  
      if (msw.ne.0) go to 200   
      xh = xh*e(mt) 
      go to 210 

  200 xh = xh*f(mt)/ek  
  210 rh = rh + xh  
      dt = dt + 0.001   
C                                     CHECK FOR ADDITIONAL BUS TIES 
      ix = ix + 1   
      if (ix.gt.ntxtie) go to 220   
      if (txtie(1,ix).eq.kt) go to 190 
C                                     ADD CONSTANT CURRENT CONTRIBUTIONS
  220 if (msw.eq.0) go to 230   
      dt = qnetu(kt)/ek +dt +ineti(kt)            
      dpt(1,kta) = dt   
      qk = qnetu(kt) -ek*(dt + aik - rh + ek*bkku(kt)) 
      dptm = abs(qk-qnetu(kt))                      
      if (abs(dptm) .gt. option(4)) then
         kownt = kownt + 1  
c***kln qnetu now double precision.
         call buserr (kta, 0.0,sngl(qk-qnetu(kt)))       
      endif 
      dqtot = dqtot + dptm  
      go to 360 

  230 continue  
      dt = ek * (dt-inetr(kt)) -gkku(kt) * ek**2      
      dpt(1,kta)= dt +pnetu(kt) +akm(kt)*dx -ek*aik 
      pk = ek*aik - dt + rh 
      dptm=abs(pk-sngl(pnetu(kt)))                        
      if (dptm .gt. option(4)) then 
         kownt = kownt + 1  
         call buserr (kta, sngl(pk-pnetu(kt)), 0.0)       
      endif 
      dptot=dptot+dptm  
      go to 360 
C                                          AREA SLACK BUS
  240 dt = 0.0  
      dx = 0.0  
      aik = 0.0 
      bik = 0.0 
      j1=karea(3,jt)
      j2=karea(4,jt)+j1-1   
      do 320 j=j1,j2
      ix=kaloc(j)   
      if (ix.lt.0) go to 320
      iax=iabs(ix)  
      k1=tie(1,iax)
      k2=tie(7,iax)
      ka1=tie(2,iax)   
      ka2=tie(8,iax)   
      kdc=tie(9,iax)   
      if (kdc.eq.0) go to 300   
      mdc = kdc 
  250 k1x = dmin1 (dcline(1,mdc),dcline(2,mdc)) 
      k2x = dmax1 (dcline(1,mdc),dcline(2,mdc)) 
      if (k1x.ne.min0(k1,k2)) go to 260 
      if (k2x.eq.max0(k1,k2)) go to 270 
  260 if (mdc.ne.kdc) call erexit   
      if (mtdcln.eq.0) call erexit  
      mdc = kdc + mtdcln
      go to 250
 
  270 if (dcline(1,mdc).eq.k1) then
         l1 = dcline(8,mdc)
         l2 = dcline(9,mdc)
      else
  280    l1 = dcline(9,mdc)
         l2 = dcline(8,mdc)
      endif
 
      v1=dcbus(20,l1)
      v2=dcbus(20,l2)
      rh = v1*(v1 - v2)/(dcline(4,mdc)*bmva)
      if (ka1.ne.jt) rh = -rh
      dt = dt - rh
      go to 320
 
  300 continue
C
C     Process phase shifter tie lines. (The following logic is
C     vulnerable if K1 and K2 both have two or more bus ties!)
C
      i1 = jckikk(k1+ntota,10)
      i2 = jckikk(k2+ntota,10)
      if (i1 .eq. 0 .or. i2 .eq. 0) then
         go to 306
      else if (txtie(1,i1) .eq. txtie(2,i2) .and.
     1         txtie(2,i1) .eq. txtie(1,i2)) then
      else
         do 302 i1 = 1, ntxtie
         if (txtie(1,i1) .eq. k1 .and. txtie(2,i1) .eq. k2)
     1      go to 304
  302    continue
         go to 306
 
  304    continue
      endif
 
      kx = dabs (txtie(8,i1))
      rh = 1.0
      if (txtie(8,i1) .lt. 0) rh = -rh
      if (ka1.ne.jt) rh = -rh
      bik = bik + rh * e(kx-ntota)
      go to 320
 
  306 ek1=e(k1)
      fk1=f(k1)
      ek2=e(k2)
      fk2=f(k2)
      gk11=tie(3,iax)
      gk12=tie(5,iax)
      bk12=tie(6,iax)
      psi=fk1-fk2
      rh=ek1*(ek1*gk11+ek2*(gk12*cos(psi)+bk12*(sin(psi)-psi)))
      rl=ek1*ek2*bk12*psi
      rj = akm(k1)*akm(k2)*bk12*psi
C
C     CHECK FOR SLACK BUS TERMS
C
      if (k1 .le. nbslck) then
         rn = ek1*ek2*bk12*fk1
         rh = rh - rn
         rl = rl - rn
         rj = rj - akm(k1)*akm(k2)*bk12*fk1
      endif
      if (k2 .le. nbslck) then
         rn = ek1*ek2*bk12*fk2
         rh = rh + rn
         rl = rl + rn
         rj = rj + akm(k1)*akm(k2)*bk12*fk2
      endif
      if (ka1 .ne. jt) then
         rh=-rh
         rl=-rl
         rj = -rj
      endif
      dt=dt-rh
      aik=aik+rl
      dx = dx + rj
  320 continue
 
      dt=dt+area(2,jt)
      dpt(1,kta)=dt+dx-aik
      pk = aik + bik - dt + area(2,jt)
      dptm=abs(pk-area(2,jt))
      if (dptm .gt. option(5)) then
         kowntc = kowntc + 1
         call buserr (kta, sngl(dpt(1,kta)), 0.0) 
      endif 
      datot=datot+dptm  
      go to 360 
C       
C     Evaluate bus tie constraints  
C       
  330 ix = ikk(5,kta)   
      if (ix.le.0.or.ix.gt.ntxtie) call erexit  
      if (txtie(8,ix).ne.kta) call erexit  
      k1 = txtie(1,ix) 
      k2 = txtie(2,ix) 
      if (msw.ne.0) go to 340   
C ***   
C *** Compute flow Pkm if phase shifters.   
C ***   
      v1 = e(k1)
      v2 = e(k2)
      gk = txtie(9,ix)  
      bk = txtie(10,ix) 
      angle = f(k1) - f(k2) - txtie(4,ix)   
      gkm = txtie(5,ix) 
      bkm = txtie(6,ix) 
      psi = f(k1) - f(k2)   
      pk1 = v1 * (v1 * gk + v2 * (gkm * cos (psi) + bkm * sin (psi)))   
      pk2 = e(kta-ntota)
C ***   
C *** The following statement is crucial: If the current tie K1-K2  
C *** is an LTC phase shifter, the phase shift was computed optimally   
C *** in DECPLP, updated in TXTIE(4,IX), and now can be added to the
C *** RHS. Otherwise, the phase shift is fixed and already is in the
C *** RHS.  
C ***   
      if (kta .ge. ntotzt) then 
         dt = 0.0   
      else  
         dt = txtie(4,ix)   
         if (k1 .le. nbslck) dt = dt - f(k1)
         if (k2 .le. nbslck) dt = dt + f(k2)
      endif 
        
      dt = dt + v1 * gk / (v2 * bk) - gk / bk * cos (angle) 
     1        - sin (angle) + angle 
        
      dpt(1,kta)=dt 

      if ( idswa .ne. 0 ) then
         write (dbug,332) k1, k2, pk1, pk2, txtie(4,ix), angle 
  332    format(' PHASE SHIFTER ',2i6,' PK ',2e12.5,' ANGLE ',2e12.5)  
      endif
      ltc = txtie(7,ix)
      if (ltc .ne. 0) then  
         d1 = dim (sngl(pk),tran(4,ltc)) - dim (tran(5,ltc),sngl(pk))   
         d2 = dim (sngl(txtie(4,ix)),tran(7,ltc)) 
     &      - dim (tran(8,ltc),sngl(txtie(4,ix))) 
         if ( idswa .ne. 0 ) then
            write (dbug,334) ltc, pk, d1, tran(4,ltc), tran(5,ltc),
     &                    txtie(4,ix), d2, tran(7,ltc), tran(8,ltc)
  334       format(' LTC ',i4,' PK ',4e12.5,' PHASE SHIFT ',4e12.5)
         endif 
      endif 
      dptm = abs (angle)
      if (dptm.gt.option(4)) kowntb = kowntb + 1
      go to 350 
        
  340 dt = 0
      if (ikk(1,k1+ntota).eq.1) dt = -e(k1) 
      if (ikk(1,k2+ntota).eq.1) dt = dt + txtie(3,ix)*e(k2) 
      dpt(1,kta) = dt   
      dptm = abs (e(k1) - txtie(3,ix) * e(k2))  
      if (dptm.gt.0.01) kowntb = kowntb + 1 
  350 dttot = dttot + dptm  
C       
C     If NTOTZT < NTOTX, perform reduction. 
C       
  360 dt = dpt(1,kta)   
      ik=ind(1,kta) 
      ikstop=ind(2,kta)-1   
      if (kta .lt. ntotzt .or. msw .eq. 1) then 
  370    if (ik .lt. ikstop) then   
            mt=amtrx(ik) 
            dt=dt-dpt(1,mt)*amtrx(ik+1) 
            ik=ik+2 
            go to 370   
         endif  
         dpt(1,kta)=dt*amtrx(ikstop)
      else  
  380    if (ik .lt. ikstop) then   
            mt=amtrx(ik) 
            if (mt .lt. ntotzt) then
               dt=dt-dpt(1,mt)*amtrx(ik+1)  
            endif   
            ik=ik+2 
            go to 380   
         endif  
         dpt(1,kta) = dt
         klp = kta - ntotzt + 1 
         blp(klp) = dpt(1,kta)  
      endif 
      go to 100 
C       
C     If NTOTZT < NTOTX, perform LP solution, append new, optimally 
C     computed angle from LP output (this was deliberately omitted  
C     when the RHS was first computed), and complete
C     elimination/normalization.
C       
  382 if (ntotzt .lt. ntotx .and. msw .eq. 0) then  
         call decplp (status,z) 
        
         nlp = ntotx - ntotzt   
         do 386 klp = 1, nlp
         kta = ntotzt + klp - 1 
         ix = ikk(5,kta)
         if (txtie(8,ix).ne.kta) call erexit   
         dt = dpt(1,kta) + txtie(4,ix)  
        
         k1 = txtie(1,ix)  
         k2 = txtie(2,ix)  
         if (k1 .le. nbslck) dt = dt - f(k1)
         if (k2 .le. nbslck) dt = dt + f(k2)
        
         ik = ind(1,kta)
         ikstop = ind(2,kta)-1  
  384    if (ik .lt. ikstop) then   
            mt=amtrx(ik) 
            if (mt .ge. ntotzt) then
               dt = dt - dpt(1,mt) * amtrx(ik+1)
            endif   
            ik = ik + 2 
            go to 384   
         endif  
         dpt(1,kta) = dt * amtrx(ikstop)
  386    continue   
      endif 
C       
C     BACK SUBSTITUTION 
C       
      if (ierr .lt. 8) then 
         do 387 i = ierr+1,8
  387    ibuser(1,i,ittot)=0
      endif 
      do 388 i = 1,8
         buser(5,i,ittot) = 0
  388 continue
      ierr = 0
 
  390 kta=kta-1
      if (kta.le.nbslck+ntota) go to 500
      ik=ind(2,kta)
      ikstop=ind(1,kta+1)-1
      if (ik.lt.ikstop) go to 400
      if (ikk(1,kta).eq.0) go to 390
      dt=dpt(1,kta)
      go to 430
 
  400 dt=dpt(1,kta)
  410 if (ik.ge.ikstop) go to 420
      mt=amtrx(ik)
      dt=dt-dpt(1,mt)*amtrx(ik+1)
      ik=ik+2
      go to 410
 
  420 dpt(1,kta)=dt
  430 kt=kta-ntota
      if (msw.eq.0) go to 470
      if (ikk(1,kta).eq.1) go to 390
      if (kt.le.ntot) go to 440 
      f(kt) = dt
      go to 390
 
  440 continue
      dt = dt - e(kt)
      if (abs(dt) .gt. option(4)) call buscor(kta, 0.0, sngl(dt))
      if (abs(dt) .lt. 0.200) go to 460
 
      icount = icount+1
C
C     Truncation formula is
C
C     DT = 2*0.200 - 0.200**2/ABS(DT)
C
      tt = 0.400 - 0.040/dabs(dt)
      dt = dsign(tt,dt)  
      if (iopton(13).eq.0) go to 460
      dx = dpt(1,kta) - e(kt)   
      write (dbug,450) kt,dx,dt 
  450 format(' TRUNCATED DC ADJUSTMENT - BUS ',i5,' ADJUSTMENT ',
     1  2e10.3)
  460 e(kt) = e(kt) + dt
      go to 390
 
  470 if (kt.le.ntot) go to 480
      e(kt) = dt
      go to 390
 
  480 continue
      dt = dt - f(kt)
      if (dabs(dt) .gt. option(4)) call buscor(kta, sngl(dt), 0.0)
      if (dabs(dt) .lt. 1.500) go to 490
 
      icount = icount+1
C
C     Truncation formula is
C
C     DT = 2*1.500 - 1.500**2/DABS(DT)
C
      tt = 3.00 - 2.25/dabs(dt)
      dt = dsign (tt,dt)
      if (iopton(13).eq.0) go to 490
      dx = dpt(1,kta) - f(kt)   
      write (dbug,450) kt,dx,dt 
  490 f(kt) = f(kt) + dt
      go to 390 
        
  500 if (ierr .lt. 8) then 
         do 510 i = ierr+1,8
            ibuser(1,i,ittot)=0
  510    continue
      endif
      return
      end   
