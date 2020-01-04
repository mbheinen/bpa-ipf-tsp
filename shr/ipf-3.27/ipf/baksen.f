C    @(#)baksen.f	20.6 7/18/96
        subroutine baksen (ntr)   ! performs the operation  
C                                   INV [dG/dX]  * [dH/dX] if NTR = 0 and
C                                   INV [dG/dX]t * [dH/dX] if NTR = 1.  
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		jndex
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		dpt(r*8), amtrx(r*8)
      include 'ipfinc/blank.inc'
c	Global variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global variables used:
c		None
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		ntotx
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
c	Local variables:
c
	double precision rh, rn, rj, rl, dt, dv, dp, dq, rhin, rlin
	double precision dxn, xh, xn, xj, xl
c
	integer mt, kt, ik

      if (ntr .eq. 1) go to 200 
C                  NTR = 0 --> Perform operation INV [dG/dX] * [dH/dX]  
C                  Perform Downwards Solution   
      do 120 kt = 1,ntotx-1 
C                  Eliminate column MT from working row 
      do 110 ik = jndex(1,kt),jndex(2,kt)-4,5   
         mt = amtrx(ik)
         rh = amtrx(ik+1)  
         rn = amtrx(ik+2)  
         rj = amtrx(ik+3)  
         rl = amtrx(ik+4)  
         dpt(1,kt)=dpt(1,kt)-rh*dpt(1,mt)-rn*dpt(2,mt) 
         dpt(2,kt)=dpt(2,kt)-rj*dpt(1,mt)-rl*dpt(2,mt) 
  110 continue  
C                   Normalize row   
      ik = jndex(2,kt) - 3  
      rhin = amtrx(ik)  
      rj = amtrx(ik+1)  
      rlin = amtrx(ik+2)
      dpt(1,kt) = dpt(1,kt)*rhin
      dpt(2,kt) = (dpt(2,kt) - rj*dpt(1,kt))*rlin   
  120 continue  
C                   Begin back solution 
      do 140 kt = ntotx-1,1,-1
         ik=jndex(2,kt)
         dxn = amtrx(ik)
         dt = dpt(1,kt)
         dv = dpt(2,kt)
         do 130 ik = jndex(2,kt)+1,jndex(1,kt+1)-1,5
            mt=amtrx(ik)
            dp=dpt(1,mt)
            dq=dpt(2,mt)
            xh = amtrx(ik+1)
            xn = amtrx(ik+3)
            xj = amtrx(ik+2)
            xl = amtrx(ik+4)
            dt = dt - dp*xh - dq*xn
            dv = dv - dp*xj - dq*xl
  130    continue
         dpt(2,kt)=dv
         dt=dt-dxn*dv
         dpt(1,kt)=dt
  140 continue
      return

C                NTR = 1 --> Perform operation INV [dG/dX]t * [dH/dX]   
C                Perform U(n)t * U(n-1)t * ... * U(1)t operation
  200 do 220 kt = 1,ntotx-1
         ik = jndex(2,kt)
         xn = amtrx(ik)
         dp=dpt(1,kt)
         dq=dpt(2,kt) - xn * dp
         dpt(2,kt) = dq
         do 210 ik = jndex(2,kt)+1,jndex(1,kt+1)-1,5
            mt=amtrx(ik)
            xh = amtrx(ik+1)
            xn = amtrx(ik+3)
            xj = amtrx(ik+2)
            xl = amtrx(ik+4)
            dpt(1,mt) = dpt(1,mt) - dp*xh - dq*xj
            dpt(2,mt) = dpt(2,mt) - dp*xn - dq*xl
  210    continue
  220 continue

C         Perform D(n)t * L(n)t * D(n-1)t * ... * L(2)t * D(1)t operation   
      do 240 kt = ntotx-1,1,-1
         ik = jndex(2,kt) - 3
         rhin = amtrx(ik)
         rj = amtrx(ik+1)
         rlin = amtrx(ik+2)
         dq = dpt(2,kt) * rlin
         dpt(2,kt) = dq
         dp = (dpt(1,kt) - rj*dpt(2,kt)) * rhin
         dpt(1,kt) = dp
         do 230 ik = jndex(1,kt),jndex(2,kt)-4,5
            mt = amtrx(ik)
            rh = amtrx(ik+1)
            rn = amtrx(ik+2)
            rj = amtrx(ik+3)
            rl = amtrx(ik+4)
            dpt(1,mt) = dpt(1,mt) - rh*dp - rj*dq
            dpt(2,mt) = dpt(2,mt) - rn*dp - rl*dq
  230    continue
  240 continue
      return
      end   
