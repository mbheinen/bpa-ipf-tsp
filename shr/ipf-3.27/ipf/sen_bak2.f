C    @(#)sen_bak2.f	20.1 10/10/96
      subroutine sen_bak2 (ntr, num_rows)
C
C     Subroutine sen_bak2 performs the operation:
C
C        INV [dG/dX]  * [dH/dX] if NTR = 0 and
C        INV [dG/dX]t * [dH/dX] if NTR = 1.  
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'

c     Local variables:
c
      double precision rh, rn, rj, rl, dt, dv, dp, dq, rhin, rlin
      double precision dxn, xh, xn, xj, xl
c
      integer mt, kt, ik

      if (ntr .eq. 1) go to 200 

C     NTR = 0 --> Perform operation INV [dG/dX] * [dH/dX]  
C                 Perform Downwards Solution   

      do kt = 1,num_rows

C        Eliminate column MT from working row 

         do ik = jndex(1,kt),jndex(2,kt)-4,5   
            mt = amtrx(ik)
            rh = amtrx(ik+1)  
            rn = amtrx(ik+2)  
            rj = amtrx(ik+3)  
            rl = amtrx(ik+4)  
            dpt(1,kt)=dpt(1,kt)-rh*dpt(1,mt)-rn*dpt(2,mt) 
            dpt(2,kt)=dpt(2,kt)-rj*dpt(1,mt)-rl*dpt(2,mt) 
         enddo

C        Normalize row   

         ik = jndex(2,kt) - 3  
         rhin = amtrx(ik)  
         rj = amtrx(ik+1)  
         rlin = amtrx(ik+2)
         dpt(1,kt) = dpt(1,kt)*rhin
         dpt(2,kt) = (dpt(2,kt) - rj*dpt(1,kt))*rlin   
      enddo

C     Begin back solution 

      do kt = num_rows,1,-1
         ik=jndex(2,kt)
         dxn = amtrx(ik)
         dt = dpt(1,kt)
         dv = dpt(2,kt)
         do ik = jndex(2,kt)+1,jndex(1,kt+1)-1,5
            mt=amtrx(ik)
            dp=dpt(1,mt)
            dq=dpt(2,mt)
            xh = amtrx(ik+1)
            xn = amtrx(ik+3)
            xj = amtrx(ik+2)
            xl = amtrx(ik+4)
            dt = dt - dp*xh - dq*xn
            dv = dv - dp*xj - dq*xl
         enddo
         dpt(2,kt)=dv
         dt=dt-dxn*dv
         dpt(1,kt)=dt
      enddo
      return

C     NTR = 1 --> Perform operation INV [dG/dX]t * [dH/dX]   
C     Perform U(n)t * U(n-1)t * ... * U(1)t operation

  200 continue
      do kt = 1,num_rows
         ik = jndex(2,kt)
         xn = amtrx(ik)
         dp=dpt(1,kt)
         dq=dpt(2,kt) - xn * dp
         dpt(2,kt) = dq
         do ik = jndex(2,kt)+1,jndex(1,kt+1)-1,5
            mt=amtrx(ik)
            xh = amtrx(ik+1)
            xn = amtrx(ik+3)
            xj = amtrx(ik+2)
            xl = amtrx(ik+4)
            dpt(1,mt) = dpt(1,mt) - dp*xh - dq*xj
            dpt(2,mt) = dpt(2,mt) - dp*xn - dq*xl
         enddo
      enddo

C     Perform D(n)t * L(n)t * D(n-1)t * ... * L(2)t * D(1)t operation   

      do kt = num_rows,1,-1
         ik = jndex(2,kt) - 3
         rhin = amtrx(ik)
         rj = amtrx(ik+1)
         rlin = amtrx(ik+2)
         dq = dpt(2,kt) * rlin
         dpt(2,kt) = dq
         dp = (dpt(1,kt) - rj*dpt(2,kt)) * rhin
         dpt(1,kt) = dp
         do ik = jndex(1,kt),jndex(2,kt)-4,5
            mt = amtrx(ik)
            rh = amtrx(ik+1)
            rn = amtrx(ik+2)
            rj = amtrx(ik+3)
            rl = amtrx(ik+4)
            dpt(1,mt) = dpt(1,mt) - rh*dp - rj*dq
            dpt(2,mt) = dpt(2,mt) - rn*dp - rl*dq
         enddo
      enddo
      return
      end   
