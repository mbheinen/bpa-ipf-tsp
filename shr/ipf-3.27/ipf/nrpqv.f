C    @(#)nrpqv.f	20.3 2/13/96
        subroutine nrpqv (kt,pk,dpk,qk,dqk,vk)  
C                computes the nodal injections, injection errors,   
C                the nodal voltage, and V-limits for node KT.  It   
C                does not modify any labeled commons.   
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
c	Global variables used:
c		gkmu(r*8), bkmu(r*8), gkku(r*8), bkku(r*8),
c		ineti(r*8), inetr(r*8), pnetu(r*8), qnetu(r*8),
c		km, kmlen, ikmu
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8)
c
      double precision   aik, bik, ek, fk, fm, em, gkm, bkm
C       
        ek = e(kt)  
        fk = f(kt)  
        aik = 0.0d0
        bik = 0.0d0 
        do 100 l = km(kt), km(kt)-1+kmlen(kt) 
           mt = ikmu(l)                       
           fm = f(mt)  
           em = e(mt)  
           gkm = gkmu(l)                      
           bkm = bkmu(l)                      
           aik = aik + em*gkm - fm*bkm 
           bik = bik + em*bkm + fm*gkm 
  100   continue
        aik = aik +ek*gkku(kt) -fk*bkku(kt)   
        bik = bik +ek*bkku(kt) +fk*gkku(kt)   
        vk = dsqrt(ek**2 + fk**2)
        pk = aik*ek + bik*fk + inetr(kt)*vk   
        qk = aik*fk - bik*ek - ineti(kt)*vk   
        dpk = pnetu(kt) - pk                  
        dqk = qnetu(kt) - qk                  
        return  
        end 
