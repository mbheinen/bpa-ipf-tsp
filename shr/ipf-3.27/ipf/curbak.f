C    @(#)curbak.f	20.6 7/18/96
        subroutine curbak (ksw)
C                             KSW determines type of correction:
C                             0 - Normal solution
C                             1 - sensitivity calculation (no truncation)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		dpt(r*8), amtrx(r*8)
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		kownta, idswa, ntotx
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tran.inc'
c
c***KLN Local variables used:
c
	double precision rh, rn, rj, rl
	double precision rhin, rlin, tt, vksq, angle
	double precision xn, dt, dv, dp, dq, de, ek 
	double precision ef, s, cx, c, dtmax, dvmax

        dtmax = 0.0
        dvmax = 0.0

C                    KSW = 1 --> Perform operation INV [dG/dX] * [dH/dX]
        if (ksw .eq. 0) go to 120
C                    Perform Downwards Solution
        do 110 kt = ntota+1,ntotx-1
C                    Eliminate column MT from working row
        do 100 ik = ikkind(1,kt),ikkind(2,kt)-4,5
           mt = amtrx(ik)
           rh = amtrx(ik+1)
           rn = amtrx(ik+2)
           rj = amtrx(ik+3)
           rl = amtrx(ik+4)
           dpt(1,kt)=dpt(1,kt)-rh*dpt(1,mt)-rn*dpt(2,mt)
           dpt(2,kt)=dpt(2,kt)-rj*dpt(1,mt)-rl*dpt(2,mt)
  100   continue
C                  Normalize row
        ik = ikkind(2,kt) - 3   
        rhin = amtrx(ik)
        rj = amtrx(ik+1)
        rlin = amtrx(ik+2)  
        dpt(1,kt) = dpt(1,kt)*rhin  
        dpt(2,kt) = (dpt(2,kt) - rj*dpt(1,kt))*rlin 
  110   continue
  120   do 210 kt = ntotx-1,ntota+1,-1  
        ik=ikkind(2,kt) 
        ikstop=ikkind(1,kt+1)-1 
        xn = amtrx(ik)  
        ik=ik+1 
        dt = dpt(1,kt)  
        dv = dpt(2,kt)  
  130   if (ik.ge.ikstop) go to 150 
        mt=amtrx(ik)
        dp=dpt(1,mt)
        dq=dpt(2,mt)
        dt=dt-dp*amtrx(ik+1)-dq*amtrx(ik+3)
        dv=dv-dp*amtrx(ik+2)-dq*amtrx(ik+4)
        ik=ik+5
        go to 130
 
  150   dpt(2,kt)=dv
        dt=dt-xn*dv 
        dpt(1,kt)=dt
        dtmax = dmax1 (dtmax,dabs(dt))   
        dvmax = dmax1 (dvmax,dabs(dv))   
  210   continue
C            Skip nodal voltage adjustments for sensitivity calculations.   
        if (ksw .eq. 1) return  
        cx = 1.0
        if (dtmax .gt. 3.00) cx = dmin1 (cx, 3.00/dtmax)
        if (dvmax .gt. 0.40) cx = dmin1 (cx, 0.40/dvmax)
        if (cx.lt.1.0) then 
           write (outbuf,220) dtmax,dvmax,cx
  220      format('0Relaxation',f14.5,' D THETA',f19.5,' D V MAX',  
     1            f18.5,' Relaxation factor' )
           call prtout(1)   
           call space(1)
           cx = dmax1 (cx,0.25d0) 
        endif   
        do 300 kt = 1,ntot  
        kta = kt + ntota
        dt = cx * dpt(1,kta)
        if (dabs(dt) .gt. 1.500) then
           if (dabs(dt) .gt. 2.00d0) dt = dsign (2.00d0,dt)   
C                      Truncation formula is
C                      TT = 2*1.500 - 1.500**2/DABS(DT)  
           tt = 3.00 - 2.25/dabs(dt) 
           dt = dsign(tt,dt) 
           if (idswa.ne.0) write (dbug,260) kt,cx*dpt(1,kta),dt 
  260      format (' Truncated Bus angle adjustment   ',i4,2e12.5)  
           kownta=kownta+1000   
        endif   
        dv = cx * dpt(2,kta)
        if (dabs(dv) .gt. 0.200) then
           if (dabs(dv) .gt. 0.300d0) dv = dsign (0.30d0,dv)  
C                    Truncation formula is  
C                    DV = 2*0.200 - 0.200**2/DABS(DV)
           tt = 0.400 - 0.040/dabs(dv)   
           dv = dsign(tt,dv) 
           if (idswa.ne.0) write (dbug,262) kt,cx*dpt(2,kta),dv 
  262      format (' Truncated Bus voltage adjustment ',i4,2e12.5)  
           kownta=kownta+1000   
        endif   
        de = 1.0 + dv   
        s = sin(dt) 
        c = cos(dt) 
        ek = e(kt)  
        fk = f(kt)  
        e(kt) = de * (ek*c - fk*s)  
        f(kt) = de * (fk*c + ek*s)  
  300   continue
        return  
        end 
