C    @(#)dcstrt.f	20.14 5/3/00
      subroutine dcstrt (kerr)

c     NOTE: Variable var sensitivities do not correctly compute   
C           dVi/dXij for nodes i,j which are PV nodes.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/addtbx.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/dcsln.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gendrp.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ntotzt.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/ycomp.inc'
      include 'ipfinc/ycpsen.inc'
      include 'ipfinc/tbxsrt.inc'
 
      common /bksln/ icount
C
      common /scratch/ lphtx(2,MAXPHS), ikk2(2,MAXBUS+2*MAXLTC+1),
     &                 ikktemp(MAXBUS+2*MAXLTC+1)
c
c***kln Local double precision variables
c
      double precision theta, rh, aim, bim, em, ek, fk, ratio, vm
      double precision a1, a2, angle, pkm, qkm, dpqlo
c
      complex y1(2,2), y2(2,2), y(2,2), v(2)
c 
      integer fichsx, kerr, k1, k2
c
      real  pk1, pk2, dpk1, dpk2, qk1, qk2, dqk1, dqk2, vk1, vk2
c
      character id * 1  
c        
      kerr=0
      ntbxad = 0

C     Insert pseudo buses on all branches with 95-105% compensation 

      call cnvtrx   

C     Temporarily convert bus ties into ideal bus ties   

      ntotzt = 0
      if (ntxtie.eq.0) go to 310
      jt = ntotx + ntxtie/2 
      lt = MAXBUS + MAXLTC + 1  
      if (jt.le.lt) go to 220   
      write (errbuf(1),210) ntxtie/2, jt, lt
  210 format('0',i3,' IDEAL BUS TIES YIELD A MAXIMUM OF ',i4,' ',
     1       ' CONSTRAINTS (',i4,' IS LIMIT). ')
      call prterx ('W',1)
      ntxtie=0
      go to 310
C
C     Remove 99% of bus tie admittance from network; NTOTZT + 1 is
C     pointer to first LTC phase shifter.  If NTOTZT = 0, no LTC
C     phase shifters are present.   
C       
C     Store terminal busses types   
C       
  220 continue

      do it = 1, ntxtie 
        k1 = txtie(1,it) 
        k2 = txtie(2,it) 
        if (txtie(7,it) .gt. 0) then 
          lphtx(1,it) = ikk(1,k1+ntota)  
          lphtx(2,it) = ikk(1,k2+ntota)  
        endif 
      enddo
        
      do it = 1,ntxtie  
        k1 = txtie(1,it) 
        k2 = txtie(2,it) 
        ls = kmlen(k1)  
        kmluu = km(k1)-1
        do l = 1,ls   
          if (ikmu(l+kmluu) .eq.k2) go to 240          
        enddo
        call erexit   
  240   gkm=txtie(5,it)   
        bkm=txtie(6,it)   
C       
C       Add residual admittance Y_res = 0.01 - j0.05 to prevent   
C       singularity.  
C       
        gkmu(l+kmluu) = gkmu(l+kmluu) - gkm - 0.01d0
        bkmu(l+kmluu) = bkmu(l+kmluu) - bkm + 0.05d0
        gmk=txtie(9,it)   
        bmk=txtie(10,it)  
        gkku(k1) = gkku(k1) - gmk + 0.01d0
        bkku(k1) = bkku(k1) - bmk - 0.05d0
        ix = jckikk(k1+ntota,10)  
        if (ix .eq. it) then  
        else if (ix .gt. 0) then  
          do i = ikk(5,k1+ntota)+1, njndxx   
            if (jndx(1,i) .eq. k1+ntota) then  
              if (jndx(2,i) .eq. 10) then 
                if (jndx(3,i) .eq. it) go to 246 
              endif   
            else   
              go to 244   
            endif  
          enddo
  244     call erexit
  246     continue   
        else  
          call erexit
        endif 

C       Set pointers for bus tie

        if (txtie(8,it) .ne. 0) go to 280
        if (txtie(7,it) .lt. 0) call erexit  
        txtie(8,it) = ntotx  
        if (txtie(7,it) .gt. 0) then 
          if (ntotzt .eq. 0) ntotzt = ntotx  
        endif 
C       
C       Temporarily convert terminal busses to PV 
C       
        if (txtie(7,it) .gt. 0) then 
          ikk(1,k1+ntota) = 1
          ikk(1,k2+ntota) = 1
        endif 
        
        j = 2 
        if (ikk(1,k1+ntota).eq.1.and.ikk(1,k2+ntota).eq.1) j = 1  
        ikk(1,ntotx)=j
        ikk(2,ntotx)=0
        ikk(3,ntotx)=0
        ikk(4,ntotx)=7
        ikk(5,ntotx)=it   
        ikkind(1,ntotx)=1 
        ikkind(2,ntotx)=1 
        mt = ntotx - ntota
C       
C       Compute initial flow in branch
C       
c       aim = gmk*e(k1) - bmk*f(k1) + gkm*e(k2) - bkm*f(k2)   
c       bim = gmk*f(k1) + bmk*e(k1) + gkm*f(k2) + bkm*e(k2)   
c       e(mt) = e(k1)*aim + f(k1)*bim 
c       f(mt) = -e(k1)*bim + f(k1)*aim
        e(mt) = 0.0
        f(mt) = 0.0
        ntotx = ntotx + 1
        do j = 1,ntxtie
          if (k2 .eq. txtie(1,j) .and. k1 .eq. txtie(2,j)) go to 270
        enddo
        call erexit
  270   txtie(8,j) = -txtie(8,it)
  280   if (idswb .gt. 0) then
          ii = dabs (txtie(8,it))
          write (dbug,290) it,(ifix(sngl(txtie(j,it))),j=1,2),
     &         (txtie(j,it),j=3,6),
     &         (ifix(sngl(txtie(j,it))),j=7,8),
     &         (txtie(j,it),j=9,10),e(ii),f(ii)
  290     format (' TXTIE ', 3i5, 2f8.3, 2f10.3, 2i6, 2f10.3, 2f8.3)
        endif
      enddo

  310 if (ntotzt .eq. 0) ntotzt = ntotx 
C       
C     Model d-c converters with simple injections
C       
      if (idcsw.eq.0) go to 400 
      do jckt = 1,idckt 
        nec=nckt(jckt)-1  
        ntdc = nckt(jckt+1) - nckt(jckt)  
        do i = 1,ntdc 
          kt=dcbus(1,i+nec)
          mt=dcbus(3,i+nec)
          if (mt .gt. 0) then
            theta = dcbus(12,i+nec)   
            pnetu(kt) = -dcbus(19,i+nec)/bmva
            qnetu(kt) = -dabs(pnetu(kt))*dtan(theta)  
          endif
        enddo
      enddo
  400 continue  
C       
C     Convert voltages to polar form
C
      do kt = 1,ntot
        ek = e(kt)
        fk = f(kt)
        vk = dsqrt(ek**2+fk**2)
        e(kt) = vk
        akm(kt) = vk
        if (vk .gt. 0.0d0) f(kt) = datan2(fk,ek)
      enddo
      msw = 0
  420 continue
C
C     Begin factorization.
C
C     NTOTLP is a reduction switch. It is initialized to 0 to flag
C     reduction to rank NTOTZT - 1. The first call to BAKSLN with
C     MSW = 0 will complement the reduced subsystem with a small
C     Lp, solve it, fill in the subsystem, and reduce it.   
C       
      msw=0 
      ikec=1
      call dcfact(ikkind)   
      msw=1 
      call dcfact(ikk2) 
      ittot=0   
      dpqlo=0.0d0
      mswnxt = 0
      msw = 1   
  430 ittot=ittot+1 
      if (ittot.gt.iopton(1)) go to 450 
      dptot = 0.0d0
      dqtot = 0.0d0
      datot = 0.0d0
      kownt = 0 
      kowntc = 0
      dttot = 0 
      kowntb = 0
      mswlst = msw  
      msw = mswnxt  
C       
C     MSWNXT is the next value of MSW.  Normally, it is set to alternate
C     P - Q - P.  The sequence is interrupted if a limit it hit to force
C     consecutive P - P's.  
C       
      if (msw .eq. 0) then  
        mswnxt = 1 
      else  
        mswnxt = 0 
      endif 
      if(msw.eq.0) then 
C       
C       NTOTLP is a reduction switch. It is initialized to 0 to flag   

C       reduction to rank NTOTZT - 1. The first call to BAKSLN with
C       MSW = 0 will complement the reduced subsystem with a small 
C       Lp, solve it, fill in the subsystem, and reduce it.
C       
        call baksln(ikkind)
        ikecl=ikkind(1,ntotx)  
        if (icount.gt.0) then  
          if (mswlst .eq. 1) mswnxt = 0   
          if (ittot+1.ge.iopton(1)) then  
            if (iopton(1).le.10) then
              iopton(1) = iopton(1) + 2 
            endif
          endif   
        endif  
      else  
        call baksln(ikk2)  
        ikecl=ikk2(1,ntotx) - ikkind(1,ntotx) + 1  
      endif 
C       
      lprtsv=lprtsw   
      fichsx=fichsw   
C       
      lprtsw=1
      if(kspare(16).ge.0) fichsw=1
C       
      write (outbuf,440) ittot,dptot,dqtot,dttot,datot,kownt
     1                  ,kowntb,kowntc,icount,ikecl 
  440 format('  INITIAL',i2,2f14.5,2f13.5,i20,2i6,i12,i18)  
      call prtout(1)
C       
      if (ittot.le.2) dpqlo = dpqlo + dptot + dqtot + datot 
      if (msw.eq.0) then
        ktot = kownt + kowntc   
      else  
        ktot = ktot + kownt + kowntc
      endif
      if (ktot.gt.0) go to 430
      go to 460

  450 if (dptot+dqtot+datot.gt.dpqlo) go to 480
  460 do kt=1,ntot
C
C       Restore rectangular form of voltages
C
        ek=e(kt)
        fk=f(kt)
        e(kt)=ek*dcos(fk)
        f(kt)=ek*dsin(fk)
      enddo
      call savsln
      go to 490

  480 call retsln
  490 continue
C
C     Remove pseudo buses from branches with 95-105% compensation
C
      call restrx
C
C     Restore bus tie admittances in Y-matrix.
C
      if (ntxtie.eq.0) go to 590
      do i = 1,ntxtie   
        k1 = txtie(1,i)  
        k2 = txtie(2,i)  
        ls = kmlen(k1)  
        kmluu = km(k1)-1
        do l = 1,ls   
          if (ikmu(l+kmluu) .eq. k2) go to 560         
        enddo
        call erexit   
C       
C       Restore or update Y-matrix.   
C       
  560   gkm = txtie(5,i)  
        bkm = txtie(6,i)  

C       Remove residual admittance Y_res = 0.01 - j0.05.

        gkmu(l+kmluu) = gkmu(l+kmluu) + gkm + 0.01d0
        bkmu(l+kmluu) =  bkmu(l+kmluu) + bkm - 0.05d0
        gkm = txtie(9,i)  
        bkm = txtie(10,i) 
        gkku(k1) = gkku(k1) + gkm - 0.01d0
        bkku(k1) = bkku(k1) + bkm + 0.05d0
        ltc = txtie(7,i) 
        if (ltc .gt. 0) then  
C ***   
C ***     Phase shifter sensitivities are explicit in the reduced
C ***     matrix ALP.
C ***   
          tap(ltc) = txtie(4,i)  
          nlp = txtie(8,i) - ntotzt + 1 
          if (nlp .lt. 0) call erexit
          phse(ltc) = alp(nlp,nlp)   
        endif 
        
        vk = dsqrt (e(k1)**2 + f(k1)**2)   
        vm = dsqrt (e(k2)**2 + f(k2)**2)   
        ratio = vk / vm   
        a1 = datan2 (f(k1),e(k1))  
        a2 = datan2 (f(k2),e(k2))  
        angle = a1 - a2   
        x = dsign (1d0, txtie(8,i)) 
        mt = dabs(txtie(8,i)) - ntota
        pkm = x * e(mt)   
        qkm = x * f(mt)   
        
        if ( idswa .ne. 0 ) then
          write (dbug,570) i,k1,k2,txtie(3,i), ratio, txtie(4,i),   
     &                       angle, pkm, qkm
  570     format(' TIE RESULTS ',3i5,' RATIOS ',2e10.3,' ANGLES ',
     &              2e10.3,' FLOWS ',2e10.3)
          if (ltc .gt. 0) then
             write (dbug,572) ltc, tap(ltc), phse(ltc)
  572        format(' LTC ',i3,' TAP ',e10.3,' SENSITIVITY ',e10.3)
          endif
        endif
C
C       Check results
C
      enddo
      ntotx = ntota + ntot + 1
  590 continue
C
C     Temporarily convert all X-buses to type PQ and compute type X 
c     bus sensitivities
C
      do jt = 1, kxtot
        kt = xdata(1,jt)
        kta = kt + ntota
        ikktemp(kta) = ikk(1,kta)
        if (ntypu(kt) .eq. 11) ikk(1,kta) = 2
      enddo
      msw=1 
      ntotzt = 0
      call dcfact(ikk2) 
      do jt = 1,kxtot
        kt = xdata(1,jt)
        if (ntypu(kt) .eq. 11) then
          do i = nbslck+ntota,ntotx
            dpt(1,i) = 0.0d0
          enddo
          vk = dsqrt (e(kt)**2 + f(kt)**2)
          kta = kt + ntota
          if (ikk(1,kta) .eq. 2) then
            dpt(1,kta) = 1.0d0
            call dwnbak (ikk2)
            xsen(jt) = sngl( dpt(1,kta) / vk )
          else
            call dcjacb
            do i = 1,lp
              mt = kolum(i)
              if (mt .ne. kta) dpt(1,mt) = rowh(i)
            enddo
            call dwnbak (ikk2)
            xsen(jt) = 0.0
            do i = 1,lp
              mt = kolum(i)
              if (mt .eq. kta) then
                xsen(jt) = xsen(jt) + real( rowh(i) )
              else
                xsen(jt) = xsen(jt) - sngl( rowh(i)*dpt(1,mt) )
              endif
            enddo
            xsen(jt) = 1.0 / ( xsen(jt) * real(vk) )
          endif
          if (idswb.ne.0) write (dbug,740) jt,kt,xsen(jt)
  740     format (' X BUS ',2i5,' SENSITIVITY ',e10.3)
        endif
      enddo
c
c     Restore state of X-buses
c
      do jt = 1, kxtot
        kt = xdata(1,jt)
        kta = kt + ntota
        ikk(1,kta) = ikktemp(kta)
      enddo
 
      if (nycomp .eq. 0) go to 900
C
C     Compute the sensitivities dPij/dXkl, DVi/dXkl, and
C     dVi/dBks in the following steps:
C
      do i = 1,nycomp
        do j = 1,nycomp
          ycpsen(j,i) = 0.0
        enddo
      enddo
C
C     Step 1. Compute (empirically) the sensitivity dY/dX.
C
C     Y1 = The perturbed 2-port Y-matrix [Y(B+dB)].
C
      do jt = 1,nycomp
 
         k1 = kycomp(1,jt)
         k2 = kycomp(2,jt)
 
         kt = inp2opt(k1)
         mt = inp2opt(k2)
         id  = char(kycomp(3,jt))  
         ksect = kycomp(4,jt)  
        
         itype = 1 
         deltax = 0.001
         call deltay (k1,k2,id,ksect,itype,deltax,y1,dydx(1,1,jt)) 
C       
C        Y2 = The perturbed 2-port Y-matrix [Y(B-dB)].  
C       
         itype = 1 
         deltax = -0.001   
         call deltay (k1,k2,id,ksect,itype,deltax,y2,dydx(1,1,jt)) 
C       
C        DYDX = [dY/dX] 
C             = {[Y(b + db/2)] - [Y(B - dB/2)]} / dB
C       
         deltax = 0.002
         do i = 1,2
           do j = 1,2
             dydx(i,j,jt) = (y1(i,j) - y2(i,j)) / deltax   
           enddo
         enddo
        
         write (dbug,756) intbus(kt), intbas(kt), intbus(mt),  
     1                    intbas(mt), id, ksect  
  756    format ('0 dY/dX sensitivity of branch ',a8,f6.1,1x,a8,f6.1,  

     1            1x,a1,1x,i1)
        
         do i = 1, 2   
           do j = 1, 2   
             write (dbug,758) i, j, dydx(i,j,jt)   
  758        format ('  [dY/dX] ',2i3,' (',e12.5,',',e12.5,')')
           enddo
         enddo
        
      enddo
        
      do jt = 1,nycomp  
        
         k1 = kycomp(1,jt) 
         k2 = kycomp(2,jt) 
        
         kt = inp2opt(k1)
         mt = inp2opt(k2)
         id  = char(kycomp(3,jt))  
         ksect = kycomp(4,jt)  
C       
C        Step 2. Compute the "objective function" dPij/dx  
C       
         itype = 1 
         deltax = 0.0  
         call deltay (k1,k2,id,ksect,itype,deltax,y,y1)
        
         ek = dsqrt (e(kt)**2 + f(kt)**2)   
         em = dsqrt (e(mt)**2 + f(mt)**2)   
        
         do i = nbslck+ntota,ntotx 
           dpt(1,i) = 0.0d0
         enddo
        
         bkm = dble(aimag (y(1,2)))
         rh = ek*em*bkm
         if (mt .gt. nbslck) dpt(1,mt+ntota) = rh  
         if (kt .gt. nbslck) dpt(1,kt+ntota) = -rh 
C       
C        Step 3. Compute the Lagrange multipliers Lambda   
C       
         call dwnbkt (ikkind)  
        
         if (idswb .gt. 1) then
           write (dbug,765) kt+ntota,mt+ntota,intbus(kt),intbas(kt),  
     1                       intbus(mt),intbas(mt),id,ksect  
  765      format (' Lagrangian multipliers for Pij ',2i5,' (',
     1              a8,f7.1,1x,a8,f7.1,1x,a1,i2,')')
           do i = 1,ntotx-1,4 
             j = min0 (i+3,ntotx-1) 
             write (dbug,766) (k,dpt(1,k),dpt(2,k),k=i,j)   
  766        format (4(1x,i4,1x,2e12.5))
           enddo
         endif 
C       
C        Step 4. Compute all sensitivities dPij/dXkl where Xkl 
C        pertains to other variable compensated lines: 
C       
C          dPij/dBkl = dPij/dbkl + Lambda * [dG/dBkl]t  
C          {dH/dU = dH/dU + Lambda * [dG/dU]t}  
        
         do j = 1,nycomp   
        
            k1 = kycomp(1,j)  
            k2 = kycomp(2,j)  
          
            k = inp2opt(k1) 
            m = inp2opt(k2) 
        
            v(1) = cmplx (e(k),f(k))  
            v(2) = cmplx (e(m),f(m))  
        
            if (min0 (k,m) .eq. min0 (kt,mt) .and.
     1          max0 (k,m) .eq. max0 (kt,mt)) then
               dgdu = real(v(1)*conjg(v(1)) * dydx(1,1,j))
     1              +real(v(2)*conjg(v(1)) * dydx(1,2,j))
        
            else  
               dgdu = 0.0 
            endif 
        
            do i = 1,2
              if (i .eq. 1) then
                k = inp2opt(k1)  
                m = inp2opt(k2)  
              else  
                k = inp2opt(k2)  
                m = inp2opt(k1)  
              endif 
              v(1) = cmplx (e(k),f(k))  
              v(2) = cmplx (e(m),f(m))  
C       
C             Compute dG/dU = ... + dPk/dXkl * Lambda Pk
C                                 + dPl/dXkl * Lambda Pl
C       
              if (i .eq. 1) then
                ix = jckikk(k+ntota,3) 
                if (ix .gt. 0) then
                else if (kycomp(7,j) .ne. 0) then  
                  ix = kycomp(7,j)
                  if (tie(1,ix) .eq. k) then 
                    dpdb = real (v(1)*conjg(v(1)) * dydx(1,1,j)) +   
     1                     real (v(2)*conjg(v(1)) * dydx(1,2,j)) 
                    jx = tie(2,ix)  
                    jx = karea(1,jx) 
                    dgdu = dgdu + dpdb * sngl(dpt(1,jx+ntota))
                  else
                    dpdb = real (v(1)*conjg(v(1)) * dydx(2,2,j)) +   
     1                     real (v(2)*conjg(v(1)) * dydx(2,1,j)) 
                    jx = tie(8,ix)  
                    jx = karea(1,jx) 
                    dgdu = dgdu + dpdb * sngl(dpt(1,jx+ntota))
                  endif   
                else   
                  dpdb = real (v(1)*conjg(v(1)) * dydx(1,1,j)) +  
     1                   real (v(2)*conjg(v(1)) * dydx(1,2,j))
                  dgdu = dgdu + dpdb * sngl(dpt(1,k+ntota))
                endif  
              else  
                ix = jckikk(k+ntota,3) 
                if (ix .gt. 0) then
                else if (kycomp(7,j) .ne. 0) then  
                  ix = kycomp(7,j)
                  if (tie(1,ix) .eq. k) then 
                    dpdb = real (v(1)*conjg(v(1)) * dydx(1,1,j)) +   
     1                     real (v(2)*conjg(v(1)) * dydx(1,2,j)) 
                    jx = tie(2,ix)  
                    jx = karea(1,jx) 
                    dgdu = dgdu + dpdb * sngl(dpt(1,jx+ntota))
                  else
                    dpdb = real (v(1)*conjg(v(1)) * dydx(2,2,j)) +   
     1                     real (v(2)*conjg(v(1)) * dydx(2,1,j)) 
                    jx = tie(8,ix)  
                    jx = karea(1,jx) 
                    dgdu = dgdu + dpdb * sngl(dpt(1,jx+ntota))
                  endif   
                else   
                  dpdb = real (v(1)*conjg(v(1)) * dydx(2,2,j)) +  
     1                   real (v(2)*conjg(v(1)) * dydx(2,1,j))
                  dgdu = dgdu + dpdb * sngl(dpt(1,k+ntota))
                endif  
              endif
        
            enddo
        
            ycpsen(jt,j) = dgdu 
        
         enddo
        
         ycomp(42,jt) = ycpsen(jt,jt)  
C       
C        Step 5. Compute the sensitivity dVk/dBs   
C       
         k1 = kycomp(1,jt) 
         k2 = kycomp(2,jt) 
           
         if (ycomp(38,jt) .lt. ycomp(39,jt)) then  
        
            do i = nbslck+ntota,ntotx  
              dpt(1,i) = 0.0d0
            enddo
        
            kt = inp2opt(k1) 
            vk = dsqrt (e(kt)**2 + f(kt)**2)
            kta = kt + ntota   
        
            if (ikk(1,kta) .eq. 2) then
              dpt(1,kta) = 1.0d0
              call dwnbak (ikk2)
              ycomp(48,jt) = sngl(dpt(1,kta) / vk)
            else
              call dcjacb
              do i = 1,lp
                mt = kolum(i)
                if (mt .ne. kta) dpt(1,mt) = rowh(i)
              enddo
              call dwnbak (ikk2)
              ycomp(48,jt) = 0.0
              do i = 1,lp
                mt = kolum(i)
                if (mt .eq. kta) then
                  ycomp(48,jt) = ycomp(48,jt) + real(rowh(i))
                else
                  ycomp(48,jt) = ycomp(48,jt) - 
     +                         ( real(rowh(i)) * sngl(dpt(1,mt)) )
                endif
              enddo
              ycomp(48,jt) = 1.0 / ( ycomp(48,jt) * sngl(vk) )  
            endif  
C       
C           Step 6. Compute the sensitivity dX/dU = [dVk/dXkl] 
C                                           = -INV [dG/dX] * dG/dU  
C       
            do i = nbslck+ntota,ntotx  
              dpt(1,i) = 0.0d0
            enddo
        
            v(1) = cmplx (e(kt),f(kt)) 
            v(2) = cmplx (e(mt),f(mt)) 
        
            kt = inp2opt(k1) 
            mt = inp2opt(k2) 
            v(1) = cmplx (e(kt),f(kt)) 
            v(2) = cmplx (e(mt),f(mt)) 
C       
C           Compute dG/dU = dQk/dXkl , dQl/dXkl
C       
            dpt(1,kt+ntota) = -aimag (v(1)*conjg(v(1)) * dydx(1,1,jt))
     1                        -aimag (v(2)*conjg(v(1)) * dydx(1,2,jt))
            dpt(1,mt+ntota) = -aimag (v(2)*conjg(v(2)) * dydx(2,2,jt))
     1                        -aimag (v(1)*conjg(v(2)) * dydx(2,1,jt))
            call dwnbak (ikk2) 
        
            ycomp(43,jt) = sngl(dpt(1,kta) / vk)
        
         endif 
        
         if (idswb .ne. 0) then
            write (dbug,782) jt,intbus(kt),intbas(kt),intbus(mt),  
     1            intbas(mt),id,ksect,ycomp(42,jt),ycomp(43,jt),  
     2            ycomp(48,jt)
  782       format (' YCOMP ',i3,' Branch ',a8,f6.1,1x,a8,f6.1,1x,a1,
     1              i2,t52,' Sensitivity dPij/dXij ',e12.5/
     2              t52,' Sensitivity dVi/dXij  ',e12.5/
     3              t52,' Sensitivity dVi/dBi   ',e12.5)
         endif 
        
      enddo
        
      if (idswb .ne. 0) then
         do i = 1,nycomp
            write (dbug,802) i,(j,ycpsen(i,j),j=1,nycomp)  
  802       format ('0 YCPSEN row ',i3,1p,5(1x,i3,1x,e10.3)/   
     1             (17x,5(1x,i3,1x,e10.3)))
            write (*,802) i,(j,ycpsen(i,j),j=1,nycomp) 
         enddo
      endif 
        
  900 continue  
C       
C     Restore temporarily converted terminal busses types.  
C       
      do it = 1,ntxtie  
         if (txtie(7,it) .gt. 0) then  
            k1 = txtie(1,it)   
            k2 = txtie(2,it)   
            ltc = txtie(7,it)  
            call nrpqv (k1, pk1, dpk1, qk1, dqk1, vk1)  
            call nrpqv (k2, pk2, dpk2, qk2, dqk2, vk2)  
            dv1 = dqk1 / (real(txtie(6,it)) * vk1)
            dv2 = dqk2 / (real(txtie(10,it)) * vk2)   
C       
C           If the estimated voltage correction through the phase   
C           shifter is greater than 0.05, temporary convert types B,
C           BC, and BT bus types to type BF.
C       
            if (abs (dv1) + abs (dv2) .gt. 0.05) then   
               if (ntypu(k1) .eq. 1 .or. ntypu(k1) .eq. 4 
     +                               .or. ntypu(k1) .eq. 10) then 
                  ntbxad = ntbxad + 1   
                  ltbxad(ntbxad) = ntypu(k1) 
                  ntotb = ntotb + 1 
                  if (ntotb .gt. MAXTBX) then   
                     write (errbuf(1), 902) MAXTBX, intbus(k1), 
     1                  intbas(k1)  
  902                format (' More than ',i4,
     &                       ' special buses (BV, BQ, BG, BO, BX, BF).',
     &                       ' Overflow encountered at bus ',a8,f6.1)
                     call prterx ('W', 1)   
                     ntotb = 1  
                     ntbxad = 1 
                  endif 
                  tbx(1,ntotb) = 6 
                  tbx(2,ntotb) = k1
                  tbx(3,ntotb) = vlimx(k1)        
                  tbx(4,ntotb) = vlimn(k1)        
                  tbx(5,ntotb) = qnetu(k1)         
                  tbx(6,ntotb) = 0.0
                  tbx(7,ntotb) = 1 
                  tbx(8,ntotb) = 0 
                  ntypu(k1) = 13   
                  lphtx(1,it) = 1   
                  if (idswb .ne. 0) then
                     write (dbug, 904) ltc, intbus(k1), intbas(k1)
  904                format (' LTC ',i3,' phase shifter terminal bus ',

     1                  a8,f6.1,' is temporarily converted to type BF')

                  endif
               endif
               if (ntypu(k2) .eq. 1 .or. ntypu(k2) .eq. 4
     +                               .or. ntypu(k2) .eq. 10) then 
                  ntbxad = ntbxad + 1   
                  ltbxad(ntbxad) = ntypu(k2) 
                  ntotb = ntotb + 1 
                  if (ntotb .gt. MAXTBX) then   
                     write (errbuf(1), 902) MAXTBX, intbus(k2), 
     1                  intbas(k2)  
                     call prterx ('W', 1)   
                     ntotb = 1  
                     ntbxad = 1 
                  endif 
                  tbx(1,ntotb) = 6 
                  tbx(2,ntotb) = k2
                  tbx(3,ntotb) = vlimx(k2)        
                  tbx(4,ntotb) = vlimn(k2)        
                  tbx(5,ntotb) = qnetu(k2)         
                  tbx(6,ntotb) = 0.0
                  tbx(7,ntotb) = 1 
                  tbx(8,ntotb) = 0 
                  ntypu(k2) = 13   
                  lphtx(2,it) = 1   
                  if (idswb .ne. 0) then
                     write (dbug, 904) ltc, intbus(k2), intbas(k2)  
                  endif 
               endif
            endif   
            ikk(1,k1+ntota) = lphtx(1,it)   
            ikk(1,k2+ntota) = lphtx(2,it)   
            if (idswb .ne. 0) then  
               write (dbug, 910) it, ltc, intbus(k1), intbas(k1),   
     1            intbus(k2), intbas(k2), dpk1, dqk1, dpk2, dqk2,   
     2            vk1, vk2  
  910          format (' TXTIE ',i2, ' LTC ', i3, a8, f6.1, 1x,
     1            a8, f6.1, 4e12.5, 2f6.3)  
            endif   
         endif  
      enddo
        
      lprtsw=lprtsv 
      fichsw=fichsx 
C       
      return
      end   
