C    @(#)senjac.f	20.6 7/18/96
      subroutine senjac (kt,ksw)
 
C      This subroutine computes Jacobian elements for bus
C      constraints. KSW determines the type.
 
C        0 - Normal Jacobian row for node
C        1 - Jacobian for Q-contraint only, regardless of bus type
 
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
      include 'ipfinc/pctvr2.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xtran.inc'
C
      real ikr,iki,imr,imi
      integer senchk, ptr
 
      lp=0
      kl=0
      kp = 0
      isw=0 
      i1 = iflag(kt)
      i2 = iflag(kt+1) - 1  
      kta=kt+ntota  
      if (ksw .eq. 0 .and. kspare(20) .eq. 0) then  

C        Check for slack bus (conditional upon AI_CONTROL option   
C        on > SENSITIVITY < not enabled).  

         do 90 i = i1,i2
         if (jflag(1,i).eq.3) then  
           isw = jflag(2,i) 
           if (senchk(kt).eq.0) then
              go to 100 
           else 

C             Area slack constraint only  

              call nrpqv (kt,pk,dp,qk,dq,vk)
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
      if (xtrflg .eq. 1) then   
         la1 = ltran(3,jt)  
         la2 = ltran(12,jt) 
      else  
         la1 = xtran(1,jt)  
         la2 = xtran(2,jt)  
      endif 
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

      kp = 0                                        
  210 do 290 l=km(kt), km(kt)-1+kmlen(kt)     
      mt=ikmu(l)                                   
      mta=mt+ntota  
      lp=lp+1   
      if (kp .eq. 0)   then                         
        if (mt .ge. kt)   then                      
           kp = l                                   
           kl = lp                                  
           kolum(lp) = ntota +kt                    
           korder(lp) = lp +1                       
           lp = lp +1                               
        endif                                       
      endif                                         
      em=e(mt)  
      fm=f(mt)  
      g12 = gkmu(l)                                
      b12 = bkmu(l)                                
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

C     PROCESS DIAGONAL 

      if (kp.eq.0) then                             
         kl=lp+1
         lp=kl  
         kolum(lp)=kta  
         mta=kta
      endif 

C     Add identity row for passive BM nodes

      if (lp.eq.1) then 
         rowh(kl) = 1.0 
         rown(kl) = 0.0 
         rowj(kl) = 0.0 
         rowl(kl) = 1.0 
      else 
         g12 = gkku(kt)
         b12 = bkku(kt)
         imr = ek*g12 - fk*b12 
         imi = ek*b12 + fk*g12
         ikr = ikr + imr
         iki = iki + imi
 
         dvk=sqrt(vksq) 
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
      endif
      korder(lp)=0
      max=mta
      mend=lp+1
      ko=lp
      korder(0)=1
      if (ksw.ne.0) return
  294 mt = senchk(kt)

C     Set up contraints for PV node

      if (mt.ne.0) then
          do 300 i = 1,mend-1
            rowj(i) = 0.0
            rowl(i) = 0.0
  300     continue
          if (mt.eq.kt) then

C            PV node

             rowl(kl) = 1.0 
          else  

C            PV node controlling MT 

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
             rowl(kl) = 0.0001  
          endif 
          if (isw.eq.0) go to 480   
          lp = 1
          ko = 0
          go to 500 
      endif 

C     Implement % VAR control scheme  

      do 310 i = i1,i2  
      if (jflag(1,i).eq.4) go to 320
  310 continue  
      go to 460 
  320 ji = jflag(2,i)   

C     Test for activity

      ptr = kpctvr(2,ji)
      if (pctvr(3,ptr).eq.0) go to 460  
      kl=0  
      do while (ptr .gt. 0)
         mt=pctvr(1,ptr)   
         if (mt .eq. kt) then
            kl=ptr
            ix = pctvr(3,ptr)   
            rj=xmtrx(ix+2)
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
            if (ix .gt. 0) then
               iax=pctvr(4,ptr)
               dqk = dqk + xmtrx(ix+1)*pct   
               do l=ix+3,iax,3   
                  ip=0  
                  ko=korder(0)  
                  jt=xmtrx(l)   
                  do while (ko .gt. 0)
                     if (kolum(ko) .eq. jt) goto 390
                     if (kolum(ko) .gt. jt) goto 380
                     ip=ko 
                     ko=korder(ko) 
                  enddo
                  max = jt  
                  lp=mend   
  380             korder(mend)=korder(ip)   
                  korder(ip)=mend   
                  ko=mend   
                  mend=mend+1   
                  kolum(ko)=xmtrx(l)
                  rowh(ko)=0.0
                  rowj(ko)=0.0
                  rown(ko)=0.0
                  rowl(ko)=0.0
  390             rowj(ko)=rowj(ko)+pct*xmtrx(l+1)
                  rowl(ko)=rowl(ko)+pct*xmtrx(l+2)
               enddo
            endif
         endif
         ptr = pctvr(8,ptr)
      enddo
      if (kolum(lp).ne.max) call erexit
  440 continue
  460 continue
  480 if (isw .eq. 0) go to 780
C
C     Nullify P constraint in Jacobian. It will be replaced with
C     an Area Interchange constraint.
C
      do 482 i = 1,mend-1
         rowh(i) = 0.0
         rown(i) = 0.0
  482 continue
      lp=korder(0)
      ko=0
C                                  Define area interchange constraints
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

  580 mta=mt+ntota
  590 if (kolum(lp)-mta) 600,620,610
  620 ko=lp
      go to 630

  600 ko=lp
      lp=korder(lp)
      if (lp.ne.0) go to 590
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
         write (dbug,772) (l,kolum(l),korder(l),rowh(l),rown(l),rowj(l),
     1    rowl(l),l=1,ix)   
  772    format (3i8,4e20.6)
      endif 
  780 continue  
      return
      end   
