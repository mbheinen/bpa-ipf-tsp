C    @(#)jacbus.f	20.6 7/18/96
      subroutine jacbus (kt,ksw)
 
C     Compute Jacobian elements for bus constraints. KSW determines 
C     the type.
C
C        0 - Normal Jacobian row for node
C        1 - Jacobian for Q-contraint only, regardless of bus type
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/errbus.inc'
      include 'ipfinc/factor.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/pctvr2.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
C
C     Double precision
C
      double precision ek, fk, em, fm, ikr, iki, imr,
     &                 imi, rh, rn, rj, rl, tsqr, dpk,
     &		       dqk, g12, b12, vksq, pct, v1, v2,
     &		       pin, xatot, dvk
      integer ptr
C
      lp=0
      kl=0
      kp = 0
      isw=0 
      i1 = iflag(kt)
      i2 = iflag(kt+1) - 1  
      kta=kt+ntota  
      if (ksw.eq.0) then

C        Check for slack bus

         do 90 i = i1,i2
         if (jflag(1,i).eq.3) then  
           isw = jflag(2,i) 
           if (kvolt(kt).eq.0) then 
              go to 100 
           else 

C             Area slack constraint only 

              call nrpqv (kt,pk,dp,qk,dq,vk)
              pnetu(kt) = pk   
              qnetu(kt) = qk   
              lp=1  
              kl = lp   
              korder(0)=1   
              kolum(0)=0
              korder(1)=0   
              kolum(kl) = kta   
              rowh(kl) = 0.0
              rown(kl) = 0.0
              rowj(kl) = 0.0
              rowl(kl) = 1.0
              max = kta 
              mend=2
              abdq = 0.0
              go to 294 
           endif
         endif  
   90 continue  
      endif 
  100 ek=e(kt)  
      fk=f(kt)  
      vksq=ek*ek+fk*fk  
      ikr=0.0   
      iki=0.0   

C     PROCESS LTC'S CONNECTED TO BUS "KT"  

      do 200 i = i1,i2  
         if (jflag(1,i).ne.2) go to 200
         jt=jflag(2,i) 
         k1=ltran(1,jt)
         m1=ltran(9,jt)
         la1=ltran(3,jt)   
         la2=ltran(12,jt)  
         ityp = mod(ltran(10,jt),100)  
         if (kt.eq.k1) then
            em=e(m1)   
            fm=f(m1)   
            g12 = gkmu(la1)                           
            b12 = bkmu(la1)                           
         else  
            em=e(k1)   
            fm=f(k1)   
            g12 = gkmu(la2)                           
            b12 = bkmu(la2)                           
         endif 
         lp=lp+1   
         kolum(lp)=jt  
         korder(lp)=lp+1   
         imr=em*g12-fm*b12 
         imi=em*b12+fm*g12 
         rh=imr*fk-imi*ek  
         rn=imr*ek+imi*fk  
         if (ityp.eq.3) then   
            if (kt.eq.k1) then 
               rowh(lp)=rh  
               rowj(lp)=-rn 
            else   
               rowh(lp)=-rh 
               rowj(lp)=rn  
            endif  
         else  
         if (kt.eq.m1) then 
            tsqr=2.0*tap(jt)*vksq
            rn=rn-tsqr*g12   
            rh=rh+tsqr*b12   
         endif  
         rowh(lp)=rn
         rowj(lp)=rh
      endif 
      rown(lp) = 0.0
      rowl(lp) = 0.0
  200 continue  

C     Process branches

  210 do 290 l=km(kt),km(kt)-1+kmlen(kt)      
         mt=ikmu(l)                                   
         mta=mt+ntota  
         lp=lp+1   
         if (kp.eq.0 .and. mt.gt.kt)   then
            kp = kt
            kl = lp
            kolum(lp) = kta
            korder(lp) = lp +1 
            lp = lp +1 
         end if
         em=e(mt)  
         fm=f(mt)  
         g12=gkmu(l)                                  
         b12=bkmu(l)                                  
         imr=em*g12-fm*b12 
         imi=em*b12+fm*g12 
         ikr=ikr+imr   
         iki=iki+imi   
         kolum(lp)=mta 
         korder(lp)=lp+1   
         rh=imr*fk-imi*ek  
         rn=imr*ek+imi*fk  
         rj=-rn
         rl=rh 
         rowh(lp)=rh   
         rowj(lp)=rj   
         rown(lp)=rn   
         rowl(lp)=rl   
  290 continue  
      if (kp.eq.0) then   
         kp = kt
         lp = lp +1 
         kl = lp
         kolum(lp) = kta
         korder(lp) = lp +1 
         mta = kta  
      end if
      em=e(kt)  
      fm=f(kt)  
      g12=gkku(kt) 
      b12=bkku(kt) 
      imr=em*g12-fm*b12 
      imi=em*b12+fm*g12 
      ikr=ikr+imr   
      iki=iki+imi   
C       
C     Add identity row for passive BM nodes 
C       
      if (lp.eq.1) then 
         rowh(kl) = 1.0 
         rown(kl) = 0.0 
         rowj(kl) = 0.0 
         rowl(kl) = 1.0 
         dpt(1,kta) = 0.0   
         dpt(2,kta) = 0.0   
      else  
         dvk=dsqrt(vksq)
         imr=inetr(kt)*dvk                        
         imi=ineti(kt)*dvk                        
         dpk=ikr*ek+iki*fk+imr  
         dqk=ikr*fk-iki*ek-imi  
         pk = dpk   
         qk = dqk   
         g12 = gkku(kt)*vksq                       
         b12 = bkku(kt)*vksq                       
         rh=-dqk-b12-imi
         rn=dpk+g12 
         rj=dpk-g12-imr 
         rl=dqk-b12 
         rowh(kl)=rh
         rown(kl)=rn
         rowj(kl)=rj
         rowl(kl)=rl
         dpt(1,kta)=pnetu(kt)-dpk                  
         dpt(2,kta)=qnetu(kt)-dqk                  
      endif 
      korder(lp)=0  
      max=mta   
      mend=lp+1 
      ko=lp 
      korder(0)=1   
      if (ksw.ne.0) return  
  294 mt = kvolt(kt)
C       
C     Set up contraints for PV node 
C       
      if (mt.ne.0) then 
          qnetu(kt) = qk   
          do 300 i = 1,mend-1   
             rowj(i) = 0.0 
             rowl(i) = 0.0 
  300     continue  
          if (mt.eq.kt) then
C       
C            PV node
C       
             rowl(kl) = 1.0 
             dpt(2,kta) = volt(kt)  
          else  
C       
C            PV node controlling MT 
C       
             mta = mt + ntota   
             ip = 0 
             ko = korder(ip)
  302        if (kolum(ko)-mta) 304,308,306 
  304        ip=ko  
             ko=korder(ko)  
             if (ko.ne.0) go to 302 
             max = mta  
             lp=mend
  306        korder(mend)=korder(ip)
             korder(ip)=mend
             ko=mend
             mend=mend+1
             kolum(ko)=mta  
             rowh(ko)=0.0   
             rowj(ko)=0.0   
             rown(ko)=0.0   
             rowl(ko)=0.0   
  308        rowl(ko)=1.0   
             rowl(kl) = 0.00001  
             dpt(2,kta) = volt(mt)  
          endif 
          if (isw.eq.0) go to 480   
          lp = 1
          ko = 0
          go to 500 
      endif 
C       
C     Implement % VAR control scheme
C       
      do 310 i = i1,i2  
         if (jflag(1,i).eq.4) go to 320
  310 continue  
      go to 460 

  320 ji = jflag(2,i)   
C       
C     Test for activity 
C       
      ptr = kpctvr(2,ji)
      if (pctvr(3,ptr).eq.0) go to 460  
      kl=0  
      do while (ptr .gt. 0)
         mt=pctvr(1,ptr)   
         if (mt .eq. kt) then
            jtbx = pctvr(2,ptr)   
            kode = tbx(7,jtbx) 
            if (kode .ne. 4) go to 460
            kl = ptr
            ix = pctvr(3,ptr)   
            rj = xmtrx(ix+2)  
         endif
         ptr = pctvr(8,ptr)
      enddo
        
      if (kl.eq.0) call erexit  
      if (rj.eq.1.0) go to 460  
      pct = rj/(rj - 1.0)   
      ptr = kpctvr(2,ji)
      do while (ptr .gt. 0)
         if (ptr .ne. kl) then
            ix = pctvr(3,ptr)   
            if (ix.eq.0) go to 410
            iax=pctvr(4,ptr)
            dqk = dqk + xmtrx(ix+1)*pct   
            do lmx=ix+3,iax,3   
               ip=0  
               ko=korder(0)  
               jt=xmtrx(lmx)   
               do while (ko .gt. 0)
                  if (kolum(ko) .eq. jt) goto 390
                  if (kolum(ko) .gt. jt) goto 380
                  ip=ko 
                  ko=korder(ko) 
               enddo
               max = jt  
               lp=mend   
  380          korder(mend)=korder(ip)   
               korder(ip)=mend   
               ko=mend   
               mend=mend+1   
               kolum(ko)=xmtrx(lmx)
               rowh(ko)=0.0  
               rowj(ko)=0.0  
               rown(ko)=0.0  
               rowl(ko)=0.0  
  390          rowj(ko)=rowj(ko)+pct*xmtrx(lmx+1)  
               rowl(ko)=rowl(ko)+pct*xmtrx(lmx+2)  
            enddo
         endif
  410    continue  
         ptr = pctvr(8,ptr)
      enddo
      if (kolum(lp).ne.max) call erexit 
  440 if(idswb.ne.0) write (dbug,450)kt,qnetu(kt),dqk,rj   
  450 format(' %VAR-JACBUS-',i5,3e11.3) 
      dpt(2,kta) = -dqk 
  460 continue  
  480 continue  
C       
C     Test for AGC scheme.  
C       
      if (isw .ne. 0) go to 10500   
      do 10481 i = i1,i2
         if (jflag(1,i) .eq. 13) go to 10482
10481 continue  
      go to 10500   
C       
C     Implement AGC scheme. 
C       
10482 ji = jflag(2,i)   
C       
C     Test for activity 
C       
      if (kagc(10,ji) .eq. 1 .and. kagc(11,ji) .eq. 0) then 
C       
C        Normal AGC.
C       
         ix = kagc(2,ji)
         rj = agcmtx(ix+2)  
         if (rj .eq. 1.0) go to 10500   
         pct = rj / (1.0 - rj)  
         dpk = agcmtx(ix+1) - agc(7,ji) 
         do 10489 j = 1, numagc 
            if (j .ne. ji .and. kagc(10,j) .eq. 1) then 
               ix = kagc(2,j)   
               if (ix .gt. 0) then  
                  iax = kagc(3,j)   
                  dpart = agcmtx(ix+1) - agc(7,j)   
                  dpk = dpk - dpart * pct   
                  do 10488 l=ix+3,iax,3 
                     ip = 0 
                     ko = korder(0) 
                     jt = agcmtx(l) 
10484                if (kolum(ko)-jt) 10485, 10487, 10486  
10485                ip = ko
                     ko = korder(ko)
                     if (ko .ne. 0) go to 10484 
                     max = jt   
                     lp = mend  
10486                korder(mend) = korder(ip)  
                     korder(ip) = mend  
                     ko = mend  
                     mend = mend + 1
                     kolum(ko) = agcmtx(l)  
                     rowh(ko) = 0.0 
                     rowj(ko) = 0.0 
                     rown(ko) = 0.0 
                     rowl(ko) = 0.0 
10487                rowh(ko) = rowh(ko) - pct * agcmtx(l+1)
                     rown(ko) = rown(ko) - pct * agcmtx(l+2)
10488             continue  
               endif
            endif   
10489    continue   
         if (kolum(lp) .ne. max) call erexit
         if (idswb .ne.0) then  
            write (dbug, 10490) kt, ji, agc(13,ji), agc(7,ji), -dpk, rj 
10490       format(' JACBUS/AGC-1  ', 2i5, 4e12.4)  
         endif  
         dpt(1,kta) = -dpk  
      else if (kagc(10,ji) .eq. 1 .and. kagc(11,ji) .gt. 0) then
C       
C        This AGC unit is a temporary slack bus. Reformulate
C        P-constraint.  
C       
         do 10491 i = 1,mend-1  
            rowh(i) = 0.0   
            rown(i) = 0.0   
10491    continue   
        
         jx = kagc(11,ji)   
         ix = kagc(2,jx)
         iax = kagc(3,jx)   
         mt = inp2opt(kagc(1,jx)) 
         dpk = pnetu(mt) -agc(13,jx) + agc(8,jx)   
         do 10498 l=ix+3,iax,3  
            ip = 0  
            ko = korder(0)  
            jt = agcmtx(l)  
10494       if (kolum(ko)-jt) 10495, 10497, 10496   
10495       ip = ko 
            ko = korder(ko) 
            if (ko .ne. 0) go to 10494  
            max = jt
            lp = mend   
10496       korder(mend) = korder(ip)   
            korder(ip) = mend   
            ko = mend   
            mend = mend + 1 
            kolum(ko) = agcmtx(l)   
            rowh(ko) = 0.0  
            rowj(ko) = 0.0  
            rown(ko) = 0.0  
            rowl(ko) = 0.0  
10497       rowh(ko) = rowh(ko) - agcmtx(l+1)   
            rown(ko) = rown(ko) - agcmtx(l+2)   
10498    continue   
         if (kolum(lp) .ne. max) call erexit
         if (idswb .ne.0) then  
            write (dbug, 10499) kt, kt, ji, pnetu(kt), mt, jx, 
     1         agc(13,jx)-agc(8,jx), pnetu(mt)     
10499       format(' JACBUS/AGC-2  ', 2i5, e12.4, 2i5, 2e12.4)  
         endif  
         dpt(1,kta) = -dpk  
      endif 
10500 continue  
        
      if (isw.eq.0) then
        
         abdp=sngl(dpt(1,kta))  
         abdp=abs(abdp) 
         abdq=sngl(dpt(2,kta))  
         abdq=abs(abdq)
         if (kvolt(kt) .ne. 0) abdq = 0.0 
         dptot=dptot+abdp   
         dqtot=dqtot+abdq   
         if (amax1(abdp,abdq).gt.option(4)) then
            kownt=kownt+1   
            call buserr (kta, sngl(dpt(1,kta)), sngl(dpt(2,kta)))   
         endif  
         go to 780  
      endif 
C       
C     Nullify P constraint in Jacobian. It will be replaced with
C     an Area Interchange constraint.   
C       
      do 482 i = 1,mend-1   
         rowh(i) = 0.0  
         rown(i) = 0.0  
  482 continue  
      dpt(1,kta) = 0.0  
      abdp=0.0  
      abdq=sngl(dpt(2,kta)) 
      abdq=abs(abdq)
      if (kvolt(kt) .ne. 0) abdq = 0.0
      dqtot=dqtot+abdq  
      if (abdq.gt.option(4)) kownt=kownt+1  
      pnetu(kt) = pk   
      lp=korder(0)  
      ko=0  
C       
C     Define area interchange constraints   
C       
  500 xatot=0.0 
      jt=isw
      if(jt.eq.0) call erexit   
      j1=karea(3,jt)
      js=karea(4,jt)+j1-1   
      do 700 j=j1,js
      ix=kaloc(j)   
      iax=iabs(ix)  
      k1=tie(1,iax)
      k2=tie(7,iax)
      mt=k1 
      if (ix.lt.0) mt=k2
      ka1=tie(2,iax)   
      kdc=tie(9,iax)   
      if (kdc.eq.0) go to 580   
      if (ix.lt.0) go to 700
      kd = kdc  
  530 k1x = dmin1 (dcline(1,kd),dcline(2,kd))   
      k2x = dmax1 (dcline(1,kd),dcline(2,kd))   
      if (k1x.ne.min0(k1,k2)) then  
         if (kd.ne.kdc) call erexit 
         if (mtdcln.eq.0) call erexit   
         kd = kdc + mtdcln  
         go to 530  
      else if (k2x.ne.max0(k1,k2)) then 
         call erexit
      endif 
      if (k1.eq.dcline(1,kd)) then  
         l1 = dcline(8,kd)  
         l2 = dcline(9,kd)  
      else  
         l1 = dcline(9,kd)  
         l2 = dcline(8,kd)  
      endif 
      v1=dcbus(20,l1)   
      v2=dcbus(20,l2)   
      pin = v1*(v1 - v2)/(dcline(4,kd)*bmva)
      if (jt.ne.ka1) pin = -pin 
      xatot = xatot + pin   
      if (idswb.ne.0) write (dbug,570) kt,pin   
  570 format(' DC export from area slack node ',i4,' is ',f7.3) 
      go to 700 
C       
C     Process LTC phase shifter tie lines   
C       
  580 if (ix .lt. 0) go to 594  
      k = ifix(sngl(tie(10,ix)))/100000
      if (k .eq. 0) go to 594   
      ityp = mod(ltran(10,k),100)   
      if (ityp .eq. 3) then 
         m1 = ltran(1,k)
         m2 = ltran(9,k)
         la1 = ltran(3,k)   
         la2 = ltran(12,k)  
        
         ek = e(m1) 
         fk = f(m1) 
         em = e(m2) 
         fm = f(m2) 
         g12 = gkmu(la1)                           
         b12 = bkmu(la1)                           
         ko = 0 
         lp = korder(0) 
         go to 586  
  582    if (kolum(lp) - k) 584,590,588 
  584    ko=lp  
         lp=korder(lp)  
  586    if (lp.ne.0) go to 582 
         max=k  
  588    korder(mend)=korder(ko)
         korder(ko)=mend
         kolum(mend)=k  
         ko=mend
         lp=mend
         mend=mend+1
         rowh(lp)=0.0   
         rown(lp)=0.0   
         rowj(lp)=0.0   
         rowl(lp)=0.0   
        
  590    continue   
        
         imr=em*g12-fm*b12  
         imi=em*b12+fm*g12  
         rh=imr*fk-imi*ek   
         rn=imr*ek+imi*fk   
        
         if (k1 .ne. m1) then   
            rh = -rh
         endif  
         if (ka1 .ne. jt) then  
            rh = -rh
         endif  
         rowh(lp) = rowh(lp) + rh   
      endif 
  594 mta=mt+ntota  
  596 if (kolum(lp)-mta) 600,620,610
  620 ko=lp 
      go to 630 
  600 ko=lp 
      lp=korder(lp) 
      if (lp.ne.0) go to 596
      max=mta   
  610 korder(mend)=korder(ko)   
      korder(ko)=mend   
      kolum(mend)=mta   
      ko=mend   
      lp=mend   
      mend=mend+1   
      rowh(lp)=0.0  
      rown(lp)=0.0  
      rowj(lp)=0.0  
      rowl(lp)=0.0  
  630 ek=e(k1)  
      fk=f(k1)  
      em=e(k2)  
      fm=f(k2)  
      vksq=ek*ek+fk*fk  
      g12=tie(5,iax)
      b12=tie(6,iax)
      ikr=g12*em-b12*fm 
      iki=b12*em+g12*fm 
      rh=-ek*iki+fk*ikr 
      rn=ek*ikr+fk*iki  
      pin=rn+vksq*tie(3,iax)
      if (ix.gt.0) then 
         rh=-rh 
         rn=rn+2.0*vksq*tie(3,iax)  
      endif 
      if (ka1.ne.jt) then   
         pin=-pin   
         rh=-rh 
         rn=-rn 
      endif 
      rowh(lp)=rowh(lp)+rh  
      rown(lp)=rown(lp)+rn  
  690 if (ix.gt.0) xatot=xatot+pin  
  700 continue  
  710 if (kolum(lp)-max) 720,740,730
  720 lp=korder(lp) 
      go to 710 
  730 call erexit   
  740 continue  
      dpt(1,kta)=area(2,jt)-xatot   
      abdp=sngl(dpt(1,kta)) 
      abdp=abs(abdp)
      datot=datot+abdp  
      if (kvolt(kt) .ne. 0) abdq = 0.0
      if (abdp.gt.option(5)) kowntc=kowntc+1
      if (abdp.gt.option(5).or.abdq.gt.option(4)) then  
         call buserr (kta, sngl(dpt(1,kta)), sngl(dpt(2,kta)))  
      endif 
C              The following logic projects a target PNET for   
c              the area slack bus such that the ensuing convergence 
c              test in SWITCH is applicable.
      pnetu(kt) = pnetu(kt) + dpt(1,kta)  
      if (idswc.gt.0) then  
         write (dbug,760) jt,kt,area(2,jt),xatot,dpt(1,kta),dpt(2,kta)  
  760    format ('0 Area interchange ',2i6,4e20.6)  
         l = 0  
         write (dbug,770) l,kolum(l),korder(l)  
  770    format ('  Debug of area Jacobian '/(3i8,4e20.6))  
         ix=mend-1  
         write (dbug,772) (l,kolum(l),korder(l),rowh(l),rown(l),rowj(l),
     1    rowl(l),l=1,ix)   
  772    format (3i8,4e20.6)
      endif 
  780 continue  
      return
      end   
