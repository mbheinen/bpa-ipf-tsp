C    @(#)trnjac.f	20.3 2/13/96
      subroutine trnjac (kt)
C
C     Compute Jacobian elements Hkm for bus
C     constraints.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/factor.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/slnopt.inc'
 
C
      real ikr,iki,imr,imi
 
      lp=0
      kl=0
      kp = 0
      isw=0 
      i1 = iflag(kt)
      i2 = iflag(kt+1) - 1  
      if (kspare(20) .eq. 1) then   
C                 Check for slack bus (conditional upon AI_CONTROL option   
C                 on > SENSITIVITY < not enabled).  
         do 90 i = i1,i2
         if (jflag(1,i).eq.3) then  
           isw = jflag(2,i) 
C                    Area slack constraint only 
           lp = 1   
           kl = lp  
           korder(0)=1  
           kolum(0)=0   
           korder(1)=0  
           kolum(kl) = kt   
           rowh(kl) = 0.0   
           max = kt 
           mend=2   
           go to 480
         endif  
   90 continue  
      endif 
  100 vk = e(kt)
      ak = f(kt)
      vksq = vk ** 2
      ek = vk * cos(ak) 
      fk = vk * sin(ak) 
      ikr=0.0   
      iki=0.0   
C                                   Process branches
  210 do 290 l=km(kt), km(kt)-1+kmlen(kt)     
      mt=ikmu(l)                                   
      lp=lp+1   
      vm = e(mt)
      am = f(mt)
      em = vm * cos(am) 
      fm = vm * sin(am) 
      if (kl .eq. 0  .and.  mt .ge. kt)   then      
         kolum(lp) = kt                             
         korder(lp) = lp +1                         
         lp = lp +1                                 
         kl = lp                                    
      endif                                         
      g12 = gkmu(l)                                
      b12 = bkmu(l)                                
      imr=em*g12-fm*b12 
      imi=em*b12+fm*g12 
      ikr=ikr+imr   
      iki=iki+imi   
      kolum(lp)=mt  
      korder(lp)=lp+1   
      rh=imr*fk-imi*ek  
      rowh(lp)=rh   
  290 continue  
C                                PROCESS DIAGONAL   
      if (kl.eq.0) then 
         kl=lp+1
         lp=kl  
         kolum(lp)=kt   
         mt=kt  
      endif 
C                        Add identity row for passive BM nodes  
      if (lp.eq.1) then 
         rowh(kl) = 1.0 
      else  
         imr = inetr(kt) * vk                     
         imi = ineti(kt) * vk                     
c>>>     PK = IKR*EK + IKI*FK + IMR 
         qk = ikr*fk - iki*ek - imi 
         g12 = gkku(kt)*vksq                       
         b12 = bkku(kt)*vksq                       
         rh = -qk - b12 - imi   
         rowh(kl) = rh  
      endif 
      korder(lp)=0  
      max=mt
      mend=lp+1 
      ko=lp 
      korder(0)=1   
      return
  480 if (isw .eq. 0) go to 780 
C       
C     Nullify P constraint in Jacobian. It will be replaced with
C     an Area Interchange constraint.   
C       
      do 482 i = 1,mend-1   
      rowh(i) = 0.0 
  482 continue  
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
      if (idswb.ne.0) write (dbug,570)kt,pin
  570 format(' DC export from area slack node ',i4,' is ',f7.3)
      go to 700 

  580 continue  
  590 if (kolum(lp)-mt) 600,620,610 
  620 ko=lp 
      go to 630 

  600 ko=lp 
      lp=korder(lp) 
      if (lp.ne.0) go to 590
      max=mt
  610 korder(mend)=korder(ko)   
      korder(ko)=mend   
      kolum(mend)=mt
      ko=mend   
      lp=mend   
      mend=mend+1   
      rowh(lp)=0.0  
  630 vk = e(k1)
      ak = f(k1)
      ek = vk * cos (ak)
      fk = vk * sin (ak)
      vm = e(k2)
      am = f(k2)
      em = vm * cos (am)
      fm = vm * sin (am)
      vksq = vk ** 2
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
  690 if (ix.gt.0) xatot=xatot+pin  
  700 continue  
  710 if (kolum(lp)-max) 720,740,730
  720 lp=korder(lp) 
      go to 710 

  730 call erexit   
  740 continue  
C       
C     The following logic projects a target PNET for
c     the area slack bus such that the ensuing convergence  
c     test in SWITCH is applicable. 
C       
      if (idswc.gt.0) then  
         write (dbug,760) jt,kt,area(2,jt),xatot
  760    format ('0 Area interchange ',2i6,4e20.6)  
         l = 0  
         write (dbug,770) l,kolum(l),korder(l)  
  770    format ('  Debug of area Jacobian '/(3i8,4e20.6))  
         ix=mend-1  
         write (dbug,772) (l,kolum(l),korder(l),rowh(l),l=1,ix) 
  772    format (3i8,4e20.6)
      endif 
  780 continue  
      return
      end   
