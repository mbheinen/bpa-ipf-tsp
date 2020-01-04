C    @(#)baktrn.f	20.3 2/13/96
        subroutine baktrn (ntr)   ! performs the operation  
C                                   INV [dG/dX] * [dH/dX] if NTR = 0 and
C                                   INV [dG/dX]t * [dH/dX] if NTR = 1.  
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		jndex
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		amtrx(r*8), dpt(r*8)
      include 'ipfinc/blank.inc'
c	Global variables used:
c		ntot
      include 'ipfinc/bus.inc'
c	Global variables used:
c		None
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		None
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None
      include 'ipfinc/prt.inc'
c	Global variables used:
c		None
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		None
      include 'ipfinc/tran.inc'
c	Global variables used:
c		None
c
c	Local variables
c
      double precision rh, rhin, xh, dt, dp
      integer mt, ik, kt
c 
      if (ntr .eq. 1) go to 200
C                     NTR = 0 --> Perform operation INV [dG/dX] * [dH/dX]   
C
C                     Perform Downwards Solution
      do 120 kt = 1,ntot
C
C                        Eliminate column MT from working row
         do 110 ik = jndex(1,kt),jndex(2,kt)-2,2
            mt = amtrx(ik)
            rh = amtrx(ik+1)
            dpt(1,kt)=dpt(1,kt)-rh*dpt(1,mt)
  110    continue
C                        Normalize row
         ik = jndex(2,kt) - 1
         rhin = amtrx(ik)
         dpt(1,kt) = dpt(1,kt)*rhin
  120 continue
C                        Begin back solution
      do 140 kt = ntot,1,-1
         dt = dpt(1,kt)
         do 130 ik = jndex(2,kt),jndex(1,kt+1)-1,2
            mt=amtrx(ik)
            dp=dpt(1,mt)
            xh = amtrx(ik+1)
            dt = dt - dp*xh
  130    continue
         dpt(1,kt)=dt
  140 continue
      return

C               NTR = 1 --> Perform operation INV [dG/dX]t * [dH/dX]
C               Perform U(n)t * U(n-1)t * ... * U(1)t operation 
  200 do 220 kt = 1,ntot
         dp=dpt(1,kt)
         do 210 ik = jndex(2,kt),jndex(1,kt+1)-1,2
            mt=amtrx(ik)
            xh = amtrx(ik+1)
            dpt(1,mt) = dpt(1,mt) - dp*xh
  210    continue
  220 continue
C            Perform D(n)t * L(n)t * D(n-1)t * ... * L(2)t * D(1)t operation
      do 240 kt = ntot,1,-1
         ik = jndex(2,kt) - 1
         rhin = amtrx(ik)
         dp = dpt(1,kt) * rhin
         dpt(1,kt) = dp
         do 230 ik = jndex(1,kt),jndex(2,kt)-2,2
            mt = amtrx(ik)
            rh = amtrx(ik+1)
            dpt(1,mt) = dpt(1,mt) - rh*dp
  230    continue
  240 continue
      return
      end   
