C    @(#)pkqk1.f	20.3 2/13/96
      subroutine pkqk1 (kt, ls, ybr, ai)

      integer kt, ls, mt
      double precision ybr, ai

      dimension ybr(3,*), ai(*)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		inetr(r*8), ineti(r*8), pnetu(r*8), qnetu(r*8)
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8)
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		option
 
      common /pkqkxx/ pk,qk
C
      double precision ek, fk, em, fm, gkm, bkm, aik, bik, 
     &                 vk, ak, bk, pkd, qkd, dp, dq

c
c      integer
c
c      real
c
      ek = e(kt)
      fk = f(kt)
      aik = 0.0d0
      bik = 0.0d0
      do 100 l = 1,ls
         mt = ybr(1,l)
         if (mt.lt.0) go to 100
         em = e(mt)
         fm = f(mt)
         gkm = ybr(2,l)
         bkm = ybr(3,l)
         aik = aik + em*gkm - fm*bkm   
         bik = bik + em*bkm + fm*gkm   
  100 continue  
      pk = sngl(ek*aik + fk*bik)
      qk = sngl(-ek*bik + fk*aik)
      vk = dsqrt (ek**2 + fk**2) 
      ak = vk*inetr(kt)                           
      bk = -vk*ineti(kt)                          
      aik = ai(1) + ai(3) + ai(5)   
      bik = ai(2) + ai(4) + ai(6)   
      pkd = aik*ek + bik*fk 
      qkd = aik*fk - bik*ek 
      dp = pk + ak - pnetu(kt) - pkd               
      dq = qk + bk - qnetu(kt) - qkd               
      if (dp**2 + dq**2 .gt. option(4)**2) then
         write (errbuf(1),110) intbus(kt), intbas(kt)
  110    format(' Y-matrix injection error - bus ',a8,f7.1)
         write (errbuf(2),120) pnetu(kt), qnetu(kt), pkd, qkd
  120    format(' -- Original    =', 2e11.3, ' Distributed =',2e11.3)
         write (errbuf(3),130) ak, bk, pk, qk
  130    format(' -- Injected    =', 2e11.3, ' Calculated  =',2e11.3)
         write (errbuf(4),140) dp,dq
  140    format(' -- ERROR       =',2e11.3)
         call prterx ('W',4)
      endif
      return
      end   
