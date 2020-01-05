C    @(#)dldx.f	20.6 7/18/96
      subroutine dldx   

C     Computes the Jacobian elements dLoss/dX where only the 
C     differential portions of Loss are used:   
c
C              Losses = Sum (Pi injections) + Sum (Ai injections)   
C                     - Sum (Di injections) 
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
      include 'ipfinc/tran.inc'
c
c***KLN Conversion from single to double precision.  
c	Local variables 
c
      double precision ikr, iki, imr, imi, b12, g12, rh, rn
      double precision fk, ek, vksq, em, fm, tsqr 
C
 
      do 100 i = 1,ntotx-1
         dpt(1,i) = 0.0d0
         dpt(2,i) = 0.0d0
  100 continue
C
C     Incremental Losses Dloss is evaluated by one of the two equations
C     below:
C
C     1. Dloss = Sum P(area slack buses) + Sum (Vk*Conjg(Idk)) + Constan
C     2. Dloss = Sum P(system slack buses) + Sum (Vk*Conjg(Idk)) + Const

C     Eq. (1) is used when area interchange is constrained. 

      if (ntotc .gt. 0 .and. kspare(20) .eq. 0) then
         do 120 jx = 1,ntotc
            kt = karea(1,jx)
            assign 120 to isw
            go to 190
  120    continue

C        Include any slack busses which are not area interchange slack 
C        busses.  

         do 126 kt = 1,nbslck
            do 122 jx = 1,ntotc
               if (kt .eq. karea(1,jx)) go to 126
  122       continue
            assign 124 to isw   
            go to 190   
  124    continue   
  126    continue   
      else  
         do 130 kt = 1,nbslck   
         assign 130 to isw  
         go to 190  
  130    continue   
      endif 
C       
C     Compute dI*/dX for all nodes  
C       
      do 140 kt = 1,ntot
      vk = dsqrt(e(kt)**2 + f(kt)**2)
      imr = inetr(kt) * vk                        
      dpt(1,kt+ntota) = dpt(1,kt+ntota) - imr   
  140 continue  
      return
C       
C     Compute dP/dX 
C       
  190 ek=e(kt)  
      fk=f(kt)  
      vksq=ek*ek+fk*fk  
      ikr=0.0   
      iki=0.0   
C       
C     Process LTC'S connected to bus "KT"   
C       
      i1 = iflag(kt)
      i2 = iflag(kt+1) - 1  
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
      imr=em*g12-fm*b12 
      imi=em*b12+fm*g12 
      rh=imr*fk-imi*ek  
      rn=imr*ek+imi*fk  
      if (ityp.eq.3) then   
         if (kt.eq.k1) then 
            dpt(1,jt) = dpt(1,jt) - rh  
         else   
            dpt(1,jt) = dpt(1,jt) + rh  
         endif  
      else  
         if (kt.eq.m1) then 
           tsqr=2.0d0*tap(jt)*vksq
           rn=rn-tsqr*g12   
           rh=rh+tsqr*b12   
         endif  
         dpt(1,jt) = dpt(1,jt) - rn 
      endif 
  200 continue  
C       
C     Process branches  
C       
  210 do 290 l=km(kt), km(kt)-1+kmlen(kt)     
      mt=ikmu(l)                                   
      mta=mt+ntota  
      em=e(mt)  
      fm=f(mt)  
      g12=gkmu(l)                                  
      b12=bkmu(l)                                  
      imr=em*g12-fm*b12 
      imi=em*b12+fm*g12 
      ikr=ikr+imr   
      iki=iki+imi   
      if (mt .eq. kt) then  
         kp = l 
      else  
         rh=imr*fk-imi*ek   
         rn=imr*ek+imi*fk   
         dpt(1,mta) = dpt(1,mta) - rh   
         dpt(2,mta) = dpt(2,mta) - rn   
      endif 
  290 continue  

C     Add identity row for passive BM nodes   

      if (kmlen(kt) .ge. 0)   then                
         vk = sqrt(vksq)
         imr=inetr(kt) *vk                        
         imi=ineti(kt) *vk                        
         pk=ikr*ek+iki*fk+imr   
         qk=ikr*fk-iki*ek-imi   
         g12 = gkku(kt) *vksq                      
         b12 = bkku(kt) *vksq                      
         rh=-qk-b12-imi 
         rn= pk+g12 
         kta = kt + ntota   
         dpt(1,kta) = dpt(1,kta) - rh   
         dpt(2,kta) = dpt(2,kta) - rn   
      endif 
      go to isw 
      end   
