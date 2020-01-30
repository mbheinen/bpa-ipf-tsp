C    @(#)linsen.f	20.3 2/13/96
      subroutine linsen
C
C     compute the line sensitivities
C
C     1. dPij/dBkl
C     2. dVi/dBkl
C     3. dLoss/dBkl
C     4. dVi/dBs
C
C     where Bkl is a pi-equivalent susceptance and Bs is a shunt
C     susceptance.
C
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/alpha.inc'
c	Global variables used:
c		pnetu(r*8), qnetu(r*8), ineti(r*8), inetr(r*8)
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		dpt(r*8)
      include 'ipfinc/blank.inc'
c	Global variables used:
c		kspare, bmva, ntota, ntot
      include 'ipfinc/branch.inc'
c	Global variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8), f(r*8), inp2opt
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		None
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbus, intbas
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/lnsen.inc'
c	Global variables used:
c		ldsen, ysen, yznew, yzold, ndsen, nlsen
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf

 
      character*1 tag(3), id1, id2
      complex dydbx(2,2), y1(2,2), y2(2,2), yold(2,2), ynew(2,2),
     1        dydx(2,2),  y(2,2), dy
C
        call forbtm
        call fortop
        write (outbuf,100)
  100   format(' LINE_SENSITIVITIES COMPUTED WITH THE ',
     &         'FOLLOWING CONTROLS:')
        call prtout (1)
        call space(1)
        if (kspare(19) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
           tag(3) = ' '
        else if (kspare(19) .eq. 1) then
           tag(1) = ' ' 
           tag(2) = ' ' 
           tag(3) = 'X' 
        else
           tag(1) = ' ' 
           tag(2) = 'X' 
           tag(3) = ' ' 
        endif   
        write (outbuf,102) tag(1)   
102     format(t18,'LTC CONTROL',t43,'(',a,')',3x,'OFF')
        call prtout (1)
 
        write (outbuf,104) tag(2)
104     format(t18,'--- -------',t43,'(',a,')',3x,
     1         'ON (FULL CONTROL)')
        call prtout (1)
 
        write (outbuf,106) tag(3)
106     format(t43,'(',a,')',3x,'ON (NO VOLTAGE CONTROL)')
        call prtout (1)
 
        call space (1)
 
        if (kspare(20) .eq. 0) then
           tag(1) = 'X'
           tag(2) = ' '
           tag(3) = ' '
        else if (kspare(20) .eq. 1) then
           tag(1) = ' ' 
           tag(2) = 'X' 
           tag(3) = ' ' 
        else
           tag(1) = ' '
           tag(2) = ' '
           tag(3) = 'X'
        endif
        write (outbuf,108) tag(1)
108     format(t18,'AI CONTROL',t43,'(',a,')',3x,'OFF')
        call prtout (1)
 
        write (outbuf,110) tag(2)
110     format(t18,'-- -------',t43,'(',a,')',3x,
     1         'CONTROL (DEFAULT)')
        call prtout (1)
 
        write (outbuf,112) tag(3)
112     format(t43,'(',a,')',3x,'MONITOR')
        call prtout (1)
 
        call space(2)
C
C     Recompute the Jacobian matrix. The previous Jacobian matrix
C     cannot be reused because
C
C         1.  only the upper-diagonal portion is stored and
C         2.  common /AMTRX/ is not physically large enough to
C             accomodate both upper and lower factors in double
C             precision.
C       
C     To circumvert the second obstacle, the Jacobian is refactored in  
C     single precision, which reduces the physical storage requirements 
C     by 50%.   
C       
      call senfac   
C       
C     Check sensitivity (debug only)
C
C
C     Begin line_sensitivity loop
C
      do 200 n = 1,nlsen
        
      kode = lsen(1,n)  
      k1 = lsen(2,n)
      m1 = lsen(3,n)
      id1 = char(lsen(4,n)) 
      ksect1 = lsen(5,n)
C       
C     Four 2-port Y-matrices must be computed:  
C       
C     1. The original 2-port Y-matrix [Y(B)].  YZOLD refers to the  
C        specific branch element K1-M1-ID1-KSECT1.  The call to DELTAY  
C        uses two dummy arguments: ITYPE = 1 and DELTAX = 0.0.  
C       
      call deltay (k1,m1,id1,ksect1,1,0.0,yold,yzold(1,n))  
C       
C     2. The perturbed 2-port Y-matrix [Y(B+dB)].  YZNEW refers to the  
C        specific branch element K1-M1-ID1-KSECT1.  
C       
C        The type of Y-matrix displacement is determined in the 
C        following tests.   
C       
C        a. Bs in section KSECT1 is modified (ITYPE = 2) to YZNEW(4,N)  
C        b. Xt in section KSECT1 is modified (ITYPE = 1) to YZNEW(2,N)  
C        c. Xt is compensated 100% (ITYPE = 1 and YZNEW(2,N) = 0.0) 
C       
      if (ysen(7,n) .eq. 0.0 .and. ysen(8,n) .ne. 0.0) then 
         itype = 2  
         deltax = ysen(8,n) - yzold(4,n)
         if (deltax .eq. 0.0) deltax = 0.001
         ysen(8,n) = deltax 
      else if (ysen(7,n) .eq. 0.0 .and. ysen(8,n) .eq. 0.0) then
C       
C        Compute YSEN(7,N) for 100% compensation of YNEW(1,2).  
C       
         itype = 1  
         if (id1 .ne. '*') then 
            call dydb (k1,m1,id1,ksect1,dydbx)  
            r = real (cmplx(-1.0,0.0)/yold(1,2))
            x = aimag (cmplx(-1.0,0.0)/yold(1,2))   
            xnew = sign (1.0e-6,x)  
            ynew(1,2) = cmplx(-1.0,0.0) / cmplx(r,xnew) 
            dy = (ynew(1,2) - yold(1,2)) / dydbx(1,2)   
            deltax = cmplx (-1.0,0.0) / dy  
            if (deltax .eq. 0.0) deltax = 0.001 
            ysen(7,n) = deltax  
         else   
            r = real (cmplx(-1.0,0.0)/yold(1,2))
            x = aimag (cmplx(-1.0,0.0)/yold(1,2))   
            xnew = sign (1.0e-6,x)  
            deltax = xnew - x   
            if (deltax .eq. 0.0) deltax = 0.001 
            ysen(7,n) = deltax  
         endif  
      else  
         itype = 1  
         deltax = ysen(7,n) - yzold(2,n)
         if (deltax .eq. 0.0) deltax = 0.001
         ysen(7,n) = deltax 
      endif 
      call deltay (k1,m1,id1,ksect1,itype,deltax,ynew,yznew(1,n))   
C       
C     3. The perturbed 2-port Y-matrix [Y(B+dB/2)], which is used   
C        to compute [dY/dB] using forward differences. Note that
C        YNEW(1,N+1) is used as a scratch write area.   
C       
      call deltay (k1,m1,id1,ksect1,itype,0.75*deltax,y2,yznew(1,n+1))  
C       
C     4. The perturbed 2-port Y-matrix [Y(B-dB/2)], which is used   
C        to compute [dY/dB] using central differences. Note that
C        YNEW(1,N+1) is used as a scratch write area.   
C       
      call deltay (k1,m1,id1,ksect1,itype,-0.25*deltax,y1,yznew(1,n+1)) 
C       
C     5. The partial [dY/dB] evaluated from an interpolated formula 
C        using central differences: 
C       
C         [dY/dB] = {[Y(b + 0.75*db)] - [Y(B - 0.25*dB)]} / dB  
C       
      do 177 i = 1,2
      do 177 j = 1,2
  177 dydx(i,j) = (y2(i,j) - y1(i,j)) / deltax  
        
      kt1 = inp2opt(k1)   
      mt1 = inp2opt(m1)   
        
      write (dbug,178) intbus(kt1),intbas(kt1),intbus(mt1),intbas(mt1), 
     1     id1,ksect1,deltax
  178 format ('0 Y-matrix sensitivity of branch ',a8,f6.1,1x,a8,f6.1,   
     1     1x,a1,1x,i1,' Delta X ',e12.5,/) 
      do 180 i = 1,2
      do 180 j = 1,2
      write (dbug,179) i,j,dydx(i,j)
  179 format ('  dY/dX ',2i3,' (',e12.5,',',e12.5,')')  
  180 continue  
        
      if (kode .eq. 1) then 
C       
C     Compute dPij/dBkl sensitivity in the following steps: 
C       
C       1. Compute the "objective function" dH/dX (actually dPij/dX)
C       2. Compute the Lagrange multipliers Lambda  
C       3. Compute the sensitivity  
C       
C          dPij/dBkl = dPij/dbkl + Lambda * [dG/dBkl]t  
C          {dH/dU = dH/dU + Lambda * [dG/dU]t}  
C       
         write (outbuf,181) 
  181    format('0Sensitivity type',
     1         t23,'Branch immitance sensitivity',  
     2         t60,'-- Original immitance (p.u.) ---',  
     3         t96,'- Compensated immitance (p.u.) -')  
         call prtout (1)
         write (outbuf,182) 
  182    format(t60,'---  R + jX ---  ---  G + jB ---', 
     1          t96,'---  R + jX ---  ---  G + jB ---') 
         call prtout (1)
         call space (1) 
         write (outbuf,184) intbus(kt1),intbas(kt1),intbus(mt1),
     1      intbas(mt1),id1,ksect1,(yzold(i,n),i=1,4),  
     2      (yznew(i,n),i=1,4)  
  184    format (t2,'dPij/dX or dPij/dB',   
     1           t23,a8,f7.1,1x,a8,f7.1,1x,a1,i2,   
     2           t60,2f8.5,1x,2f8.5,
     3           t96,2f8.5,1x,2f8.5)
         call prtout (1)
         call space (1) 
         if (itype .eq. 1) then 
            write (outbuf,186) 'dPij/dX','dX'   
  186       format(t23,'Monitored line flow ',
     1             t60,a,   
     2             t75,a,   
     3             t90,'Existing  Pij', 
     4             t105,'Pertubed Pij ',
     5             t120,' New Pij') 
            call prtout (1) 
         else   
            write (outbuf,186) 'dPij/dB','dB'   
            call prtout (1) 
         endif  
         write (outbuf,188) 
  188    format(
     1          t60,'(Mw/p.u.) ',   
     2          t75,'  (p.u.) ',
     3          t90,'    (Mw)     ',
     4          t105,'   (Mw)     ',
     5          t120,'    (Mw)     ')   
         call prtout (1)
         call space (1) 
        
         jt = lsen(6,n) 
C       
C        Loop through Pij (M1-M2)   
C       
         do 191 jdummy = 1,ndsen
         if (jt .eq. 0) go to 200   
        
         k2 = ldsen(1,jt)   
         m2 = ldsen(2,jt)   
         id2 = char(ldsen(3,jt))
         ksect2 = ldsen(4,jt)   
C       
C        Determine the 2-port Y-matrix for branch K2-M2-ID2.
C       
         call deltay (k2,m2,id2,0,1,0.0,y,yznew(1,n+1)) 
        
         kt2 = inp2opt(k2)
         mt2 = inp2opt(m2)
C       
C        Compute the "objective function" dH/dX (actually dPij/dX)  
C       
         call dpdx (kt2,mt2,y)  
C       
C        Compute the LaGrange multipliers   
C       
         call baksen (1)
         write (dbug,10900) kt2+ntota,mt2+ntota,intbus(kt2),intbas(kt2),
     1      intbus(mt2),intbas(mt2) 
10900    format (' Lagrangian multipliers for Pij (',2i5,1x,a8,f7.1,
     1      1x,a8,f7.1,')') 
         do 10920 i = 1,ntotx-1,4   
         j = min0 (i+3,ntotx-1) 
         write (dbug,10910) (k,dpt(1,k),dpt(2,k),k=i,j) 
10910    format (4(1x,i4,1x,2e12.5))
10920    continue   
C       
C        Compute the sensitivity:   
C       
C        dH/dU = dH/dU + Lambda * [dG/dU]t  
C       
         call dpdu (kt2,mt2,y,kt1,mt1,dydx,deltax,pij,dpdb) 
         if (itype .eq. 1) then 
            deltax = ysen(7,n)  
         else   
            deltax = ysen(8,n)  
         endif  
         deltap = dpdb * deltax 
         pnew = pij + deltap
         write (outbuf,190) intbus(kt2),intbas(kt2),intbus(mt2),
     1      intbas(mt2),id2,dpdb,deltax,pij,deltap,pnew 
  190    format (t23,a8,f7.1,1x,a8,f7.1,1x,a1,  
     1           t60,e12.5, 
     2           t75,f12.5, 
     3           t90,f10.2, 
     4           t105,f10.2,
     5           t120,f10.2)
         call prtout (1)
        
  191    jt = ldsen(5,jt)   
        
      else if (kode .eq. 2) then
C       
C     Compute dVi/dBkl sensitivity in the following steps:  
C       
C       1. Compute the partials dG/dU where U is Bkl.   
C       2. Compute the sensitivity  
C       
C       
         call dvdu (kt1,mt1,dydx,deltax)
         call baksen (0)
         call vsen (n,kt1,mt1,dydx,deltax)  
        
      else if (kode .eq. 3) then
C       
C     Compute dLoss/dBkl sensitivity in the following steps:
C       
C       1. Compute the "objective function" dH/dX (actually dLoss/dX)   
C       2. Compute the LaGrange multipliers 
C       3. Compute the sensitivity  
C       
C          dLoss/dBkl = dH/dU + Lambda * [dG/dU]t   
C       
         call dldx  
         call baksen (1)
         write (dbug,10930) 
10930    format (' Lagrangian multipliers for Losses')  
         do 10950 i = 1,ntotx-1,4   
         j = min0 (i+3,ntotx-1) 
         write (dbug,10940) (k,dpt(1,k),dpt(2,k),k=i,j) 
10940    format (4(1x,i4,1x,2e12.5))
10950    continue   
         call dldu (kt1,mt1,y1,dydx,deltax,dldb)
C       
C     Compute total system losses   
C       
         ploss = 0.0
         qloss = 0.0
        
         do 1090 kt = 1,ntot
         vk = dsqrt (e(kt)**2 + f(kt)**2)
         ploss = ploss + pnetu(kt) -inetr(kt) * vk   
         qloss = qloss + qnetu(kt) +ineti(kt) * vk   
 1090    continue   
         ploss = ploss * bmva   
         qloss = qloss * bmva   
        
         write (outbuf,192) 
  192    format('0Sensitivity type',
     1         t23,'Branch immitance sensitivity',  
     2         t60,'-- Original immitance (p.u.) ---',  
     3         t96,'- Compensated immitance (p.u.) -')  
         call prtout (1)
         write (outbuf,193) 
  193    format(t60,'---  R + jX ---  ---  G + jB ---', 
     1          t96,'---  R + jX ---  ---  G + jB ---') 
         call prtout (1)
         call space (1) 
         write (outbuf,194) intbus(kt1),intbas(kt1),intbus(mt1),
     1      intbas(mt1),id1,ksect1,(yzold(i,n),i=1,4),  
     2      (yznew(i,n),i=1,4)  
  194    format (t2,'dLoss/dX or dLoss/dB', 
     1           t23,a8,f7.1,1x,a8,f7.1,1x,a1,i2,   
     2           t60,2f8.5,1x,2f8.5,
     3           t96,2f8.5,1x,2f8.5)
         call prtout (1)
         call space (1) 
         if (itype .eq. 1) then 
            write (outbuf,195) 'dLoss/dX','dX'  
  195       format( 
     1             t60,a,   
     2             t75,a,   
     3             t90,'Existing Losses',   
     4             t105,'Pertubed Losses',  
     5             t120,' New Losses')  
            call prtout (1) 
         else   
            write (outbuf,195) 'dLoss/dB','dB'  
            call prtout (1) 
         endif  
         write (outbuf,196) 
  196    format(
     1          t60,'(Mw/p.u.) ',   
     2          t75,'  (p.u.) ',
     3          t90,'    (Mw)     ',
     4          t105,'    (Mw)     ',   
     5          t120,'    (Mw)     ')   
         call prtout (1)
         call space (1) 
         if (itype .eq. 1) then 
            deltax = ysen(7,n)  
         else   
            deltax = ysen(8,n)  
         endif  
         deltap = dldb * deltax 
         pnew = ploss + deltap  
         write (outbuf,197) dldb,deltax,ploss,deltap,pnew   
  197    format (   
     1           t60,e12.5, 
     2           t75,f12.5, 
     3           t90,f10.2, 
     4           t105,f10.2,
     5           t120,f10.2)
         call prtout (1)
        
      endif 
        
  200 continue  
      return
      end   
