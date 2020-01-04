C    @(#)gettrf.f	20.3 2/13/96
        subroutine gettrf (jt, lt, nt, senstl, senstt, pout, dpovld,
     1                     comp, tx)
C
C       This subroutine computes compensation COMP and transfer TX in
C       three modes:
C
C       1. JT = 0: No outage occurs, i.e., compute the base case transfe
C       2. LT = 0: No overload occurs, i.e., compute the compensation
C                  COMP
C       3.         Normal: compute the compensation COMP and the transfe
C                  TX to alleviate overload DPOVLD.
C
C       Input parameters:
C
C         JT - outage index
C         LT - overload index
C         NT - transfer index
C         SENSTL(2,*) - the Sensitivities G(x)**-1 * G(u) for outages
C         SENSTF(*) - the Sensitivities G(x)**-1 * G(t) for transfer
C         POUT - the base line flow of the outaged line
C         DPOVLD - the required power excursion in the monitored line
C
C       Output parameters:
C
C         COMP - the outage compensation to simulate outage Pout
C         TX - the transfer to alleviate the overload dPovld
C
C       The general form is
C
C        | L(l) - L(x)*SENSTL    -L(X)*SENSTT    || dl |   | -L(0)  |
C        |                                       ||    | = |        |
C        |      - F(x)*SENSTL    -F(x)*SENSTT    || dt |   | -dF(0) |
C
C       where dL = COMP and dt = TX.
C
 
 
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		idswb
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/transf.inc'
c	Global variables used:
c		fdata, ldata
 
        double precision senstl(2,*)
c
        real senstt(*)
c 
        if (jt .eq. 0) then
C
C          Determine transfer without outage (base overload)
C
           comp = 0.0
           if (lt .eq. 0) then
              tx = 0.0
           else
              k1 = kfdata(1,lt)
              k2 = kfdata(2,lt)
              x = -fdata(11,lt)*senstt(k1) - fdata(13,lt)*senstt(k2)
              if (x .eq. 0.0) then
                 tx = sign (1.0e10,-dpovld)
              else
                 tx = -dpovld / x
              endif
           endif
 
        else if (nt .eq. 0) then
C
C          Determine compensation without transfer (outage without
C          corrective action)
C
           tx = 0.0
           if (jt .eq. 0) then
              comp = 0.0
           else
              k1 = kldata(1,jt)
              k2 = kldata(2,jt)
              x = 1.0 - ldata(11,jt) * senstl(1,k1) -
     1                  ldata(13,jt) * senstl(1,k2)
              if (x .eq. 0.0) then
                 comp = sign (1.0e10,-pout)
              else
                 comp = -pout / x
              endif
           endif
        else if (lt .eq. 0) then
           comp = 0.0
           tx = 0.0
        else
C
C          Determine compensation and transfer simultaneously
C
           k1 = kldata(1,jt)
           k2 = kldata(2,jt)
           a11 = 1.0 - ldata(11,jt) * senstl(1,k1)
     1               - ldata(13,jt) * senstl(1,k2)
           a12 = -ldata(11,jt) * senstt(k1) - ldata(13,jt) * senstt(k2)
 
           k1 = kfdata(1,lt)
           k2 = kfdata(2,lt)
           a21 = -fdata(11,lt) * senstl(1,k1)
     1          - fdata(13,lt) * senstl(1,k2)
           a22 = -fdata(11,lt) * senstt(k1) - fdata(13,lt) * senstt(k2)
 
           denom = a11 * a22 - a21 * a12
           if (abs (denom) .le. 1.0e-6) then
              comp = sign (1.0e10,-pout)
              tx = sign (1.0e10,-dpovld)
           else
              comp = (a22 * (-pout) - a12 * (-dpovld)) / denom
              tx = (-a21 * (-pout) + a11 * (-dpovld)) / denom
           endif
        endif
 
        if (idswb .gt. 0) then
            write (dbug,100) jt, lt, nt, pout, dpovld, comp, tx
  100       format (' GETTRF/ JT,LT,NT,POUT,DPOVLD,COMP,TX ',
     1         3i5,4e12.5)
        endif
 
        return
        end
