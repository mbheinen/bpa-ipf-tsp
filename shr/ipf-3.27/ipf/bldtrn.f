C    @(#)bldtrn.f	20.3 2/13/96
      subroutine bldtrn
 
C        This subroutine preprocess TRANSFER_SENSITIVITES:
C
C           Types: "T" = Intertie transfer
C                  "L" = Outage
C                  "F" = Monitored overload
C
C        1. Preprocess (>TRANSFER) Area Intertie Transfers (T).
C           a. Obtain area slack bus numbers.
C           b. Compute and store dG/dT.
C
C        2. Preprocess (>OVERLOAD) Overload Monitor branches (F).
C           a. Compute 2-port Y-matrix and F(o) for outage branches.
C           b. Compute dF/dX.   
C        3. Process (>OUTAGE) Forced Outage Branches (L).   
C           a. Compute 2-port Y-matrix and L(o) for overload monitored  
C              branches.
C           b. Compute dL/dX.
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
c	Global variables used:
c		None
      include 'ipfinc/amtrx.inc'
c	Global variables used:
c		dpt(r*8)
      include 'ipfinc/arcntl.inc'
c	Global variables used:
c		None
      include 'ipfinc/area.inc'
c	Global variables used:
c		karea, tie
      include 'ipfinc/blank.inc'
c	Global variables used:
c		nbslck, jtie, ntot, ltot, kspare, bmva
      include 'ipfinc/branch.inc'
c	Global variables used:
c		kbrnch, brnch
      include 'ipfinc/bus.inc'
c	Global variables used:
c		inp2opt, f(r*8), e(r*8), kbsdta
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		idswb
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		intbas, intbus
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf,errbuf
      include 'ipfinc/transf.inc'
c	Global variables used:
c		ldata, numl, fdata, fymtrx, numf, tdata, numt
c
      double precision lagran(MAXBUS)
c
      real ikr, iki, imr, imi, ibase
c
      complex yeq(2,2), y1(2,2), y2(2,2), yxy(2,2), v(2), a(2),
     2        s(2),     ax1,     bx1,     cx1,      dx1,  z
c
      logical debug, jdebug
c
      integer error
c
      character id * 1, tag(3) * 1
C                                   Initialize counters/switches
      error = 0
      kbsdta(16,ntot+1) = ltot + 1
      if (idswb .eq. 0) then
         debug = .false.
      else
         debug = .true.
      endif
      jdebug = debug
 
      call forbtm
      call fortop
      write (outbuf,92)
   92 format(' transfer_sensitivities ',
     &       'COMPUTED WITH THE FOLLOWING CONTROLS:')
      call prtout (1)
      call space(1)
      if (kspare(19) .eq. 0) then
         tag(1) = 'x'
         tag(2) = ' '
         tag(3) = ' '
      else if (kspare(19) .eq. 1) then
         tag(1) = ' '
         tag(2) = ' '
         tag(3) = 'x'
      else
         tag(1) = ' '
         tag(2) = 'x'
         tag(3) = ' '
      endif
      write (outbuf,94) tag(1)
   94 format(t18,'ltc CONTROL',t43,'(',a,')',3x,'off')
      call prtout (1)
 
      write (outbuf,96) tag(2)
   96 format(t18,'--- -------',t43,'(',a,')',3x,'on (FULL CONTROL)')
      call prtout (1)   
        
      write (outbuf,98) tag(3)  
   98 format(t43,'(',a,')',3x,'on (NO VOLTAGE CONTROL)')
      call prtout (1)   
      call space (1)
c                         fORCE ai CONTROL  
      kspare(20) = 1
      if (kspare(20) .eq. 0) then   
         tag(1) = 'x'   
         tag(2) = ' '   
         tag(3) = ' '   
      else if (kspare(20) .eq. 1) then  
         tag(1) = ' '   
         tag(2) = 'x'   
         tag(3) = ' '   
      else  
         tag(1) = ' '   
         tag(2) = ' '   
         tag(3) = 'x'   
      endif 
      write (outbuf,100) tag(1) 
  100 format(t18,'AI CONTROL',t43,'(',a,')',3x,'OFF')   
      call prtout (1)   
        
      write (outbuf,102) tag(2) 
  102 format(t18,'-- -------',t43,'(',a,')',3x,'control (default)') 
      call prtout (1)   
        
      write (outbuf,104) tag(3) 
  104 format(t43,'(',a,')',3x,'MONITOR')
      call prtout (1)   
      call space(2) 
C                   Convert voltages to polar form  
      do 106 kt = 1,ntot
      ek = real(e(kt))
      fk = real(f(kt))
      vk = sqrt(ek**2 + fk**2)  
      e(kt) = vk
      if (vk .gt.0.0) f(kt) = atan2 (fk,ek) 
  106 continue  
C        Recompute the Jacobian matrix. The previous Jacobian matrix
C        is not used because
C       
C            1.  only the upper-diagonal portion is stored, whereas 
C                both upper and lower elements are required,
C            2.  common /AMTRX/ is not physically large enough to   
C                accomodate both upper and lower factors in double  
C                precision, and 
C            3.  real-only constraints are considered.  
C        To circumvert the second obstacle, the Jacobian is refactored
C        in single precision, which reduces the physical storage 
C        requirement by 50%.
C
      call trnfac
C
C        Check sensitivity
      if( jdebug ) call trnchk

  110 close (unit=11, err = 111)
  111 open (unit   = 11,
     &      access = 'DIRECT',
     &      form   = 'UNFORMATTED',
     &      status = 'SCRATCH',
     &      recl   =  MAXBUS)
C
C             Process >TRANSFER_SENSITIVITY type "T" (Intertie Transfer)
      do 120 jt = 1, numt
         do 114 i = 1,ntot
            dpt(1,i) = 0.0
  114    continue
      ka1 = ktdata(1,jt)
      ks1 = karea(1,ka1)
      ktdata(3,jt) = ks1
      ka2 = ktdata(2,jt)
      ks2 = karea(1,ka2)
      ktdata(4,jt) = ks2
C                 Partials dG/dT are trivial:   
      tdata(5,jt) = -1.0
      tdata(6,jt) = 1.0
      tdata(7,jt) = 0.0
 
      do 112 j = 1,jtie
         k1 = tie(1,j)
         k2 = tie(7,j)
         if (ka1 .eq. tie(2,j) .and. ka2 .eq. tie(8,j)) then
            factor = 1.0
         else if (ka1 .eq. tie(8,j) .and. ka2 .eq. tie(2,j))
     1   then
            factor = -1.0
         else
            go to 112
         endif
         pij = tieflo (j,0.0,dpt,0.0,dpt)
         tdata(7,jt) = tdata(7,jt) + factor * pij
  112 continue
C                                Compute and store -g(x)**-1*g(t)
      if (ks1 .gt. nbslck) dpt(1,ks1) = -1.0
      if (ks2 .gt. nbslck) dpt(1,ks2) = 1.0
      call baktrn (0)
 
      do 115 i = 1,ntot
         lagran(i) = dpt(1,i)
  115 continue
      write (11,rec=jt) (lagran(i),i=1,ntot)
      if (debug) then   
         write (dbug,116) jt,(i,ktdata(i,jt),i=1,4),
     1      (i,tdata(i,jt),i=5,7)   
         write (dbug,116) jt,(i,ktdata(i,jt),i=1,4) 
  116    format (' BLDTRN/TDATA - row ',i3,4(i3,i12) /  
     1           (23x,5(i3,e12.5))) 
      endif 
  120 continue  
C             Process >TRANSFER_SENSITIVITY type "F" (Overload Monitor) 
      do 200 jt = 1, numf   
      k1 = kfdata(1,jt) 
      kt = inp2opt(k1)
      kfdata(1,jt) = kt 
      k2 = kfdata(2,jt) 
      mt = inp2opt(k2)
      kfdata(2,jt) = mt 
      id = char(kfdata(3,jt))   
      ksect = kfdata(4,jt)  
      nbr = kfdata(5,jt)
      fdata(8,jt) = brnch(4,nbr)
      if (kbrnch(1,nbr) .ne. 5 .and. kbrnch(1,nbr) .ne. 6) then 
         fdata(8,jt) = -fdata(8,jt) 
      endif 
      ibase = 1000.0 * bmva / (sqrt (3.0) * intbas(kt)) 
      fdata(10,jt) = ibase  
C       
C        Compute following 2-port Y-matrices:   
C       
C        YEQ - Equivalent parallel 2-port   
C        Y1  - 2-port left of section KSECT 
C        YXY - 2-port of section KSECT  
C        Y2  - 2-port right of section KSECT
C       
      call getyeq (k1,k2,id,ksect,yeq,y1,yxy,y2)
        
      fymtrx(1,jt) = yeq(1,1)   
      fymtrx(2,jt) = yeq(1,2)   
        
      z = -cmplx (1.0,0.0) / yeq(1,2)   
      r = real (z)  
      x = aimag (z) 
      if (abs (r) .gt. abs (x)) then
         write (errbuf(1),180) intbus(kt), intbas(kt), intbus(mt),  
     1      intbas(mt), id, ksect, r, x 
  180    format (' >OVERLOAD branch ',a8,f6.1,1x,a8,f6.1,1x,a1,i2,  
     1      ' has R (',f8.5,') > X (',f8.5,').')
         call prterx ('W',1)
      endif 
C       
C        Compute the section-to-branch conversion factors.  
C       
C        | Vx |   | Ax1   Bx1 || V1 |   
C        |    | = |           ||    |   
C        | Ix |   | Cx1   Dx1 || I1 |   
C       
C       
      if (cabs(y1(1,1)) .eq. 0.0) then  
         ax1 = cmplx (1.0,0.0)  
         bx1 = cmplx (0.0,0.0)  
         cx1 = cmplx (0.0,0.0)  
         dx1 = cmplx (1.0,0.0)  
      else  
         ax1 = -y1(1,1) / y1(1,2)   
         bx1 = cmplx (1.0,0.0) / y1(1,2)
         cx1 = -(y1(2,1) - y1(2,2) * y1(1,1) / y1(1,2)) 
         dx1 = -y1(2,2) / y1(1,2)   
      endif 
      fdata(15,jt) = real (ax1) 
      fdata(16,jt) = aimag (ax1)
      fdata(17,jt) = real (bx1) 
      fdata(18,jt) = aimag (bx1)
      fdata(19,jt) = real (cx1) 
      fdata(20,jt) = aimag (cx1)
      fdata(21,jt) = real (dx1) 
      fdata(22,jt) = aimag (dx1)
C                                      Compute F(0), dF/dX
      vk = real(e(kt))
      ak = real(f(kt))
      ek = vk * cos (ak)
      fk = vk * sin (ak)
      vm = real(e(mt))
      am = real(f(mt))
      em = vm * cos (am)
      fm = vm * sin (am)
      ikr = 0.0
      iki = 0.0
      g12 = real (yeq(1,2))
      b12 = aimag (yeq(1,2))
      imr = em * g12 - fm * b12
      imi = em * b12 + fm * g12
      ikr = ikr + imr
      iki = iki + imi
      rh = imr * fk - imi * ek
      rn = imr * ek + imi * fk
      rj = -rn
      fdata(13,jt) = -rh
      fdata(14,jt) = -rj
        
      vksq = vk ** 2
      g12 = real (yeq(1,1)) * vksq  
      b12 = aimag (yeq(1,1)) * vksq 
      pk = ikr * ek + iki * fk + g12
      qk = ikr * fk - iki * ek - b12
      rh = -qk - b12
      rj = pk - g12
      fdata(11,jt) = -rh
      fdata(12,jt) = -rj
      fdata(6,jt) = pk
      fdata(7,jt) = qk
C
C        Compute F(0), dF/dX for KSECT also.
C       
      a(1) = cmplx (ikr,iki)
      v(1) = cmplx (ek,fk)  
      v(2) = cmplx (fdata(19,jt),fdata(20,jt)) * a(1) + 
     1       cmplx (fdata(21,jt),fdata(22,jt)) * v(1)   
      a(2) = cmplx (fdata(15,jt),fdata(16,jt)) * v(1) + 
     1       cmplx (fdata(17,jt),fdata(18,jt)) * a(1)   
      s(2) = v(2) * conjg(a(2)) 
      fdata(9,jt) = real (s(2)) 
        
      if (debug) then   
         write (dbug,190) jt,(i,kfdata(i,jt),i=1,5),
     1     (i,fdata(i,jt),i=6,24)   
  190    format (' BLDTRN/FDATA - row ',i3,5(i3,i12) /  
     1          (23x,5(i3,e12.5)))  
      endif 
        
  200 continue  
C       
C        Process >TRANSFER_SENSITIVITY type "L" (Outage)
C       
      do 300 jt = 1, numl   
        
      k1 = kldata(1,jt) 
      kt = inp2opt(k1)
      kldata(1,jt) = kt 
      k2 = kldata(2,jt) 
      mt = inp2opt(k2)
      kldata(2,jt) = mt 
      id = char(kldata(3,jt))   
      ksect = kldata(4,jt)  
      nbr = kldata(5,jt)
C       
C        Compute following 2-port Y-matrices:   
C       
C        YEQ - Equivalent parallel 2-port   
C        Y1  - 2-port left of section KSECT 
C        YXY - 2-port of section KSECT  
C        Y2  - 2-port right of section KSECT
C       
      call getyeq (k1,k2,id,ksect,yeq,y1,yxy,y2)
        
      lymtrx(1,jt) = yeq(1,1)   
      lymtrx(2,jt) = yeq(1,2)   
        
      z = -cmplx (1.0,0.0) / yeq(1,2)   
      r = real (z)  
      x = aimag (z) 
      if (abs (r) .gt. abs (x)) then
         write (errbuf(1),280) intbus(kt), intbas(kt), intbus(mt),  
     1      intbas(mt), id, ksect, r, x 
  280    format (' >OUTAGE branch ',a8,f6.1,1x,a8,f6.1,1x,a1,i2,
     1      ' has R (',f8.5,') > X (',f8.5,').')
         call prterx ('W',1)
      endif
C                                Compute dG/dL.  This is trivial:
      ldata(7,jt) = 1.0
      ldata(8,jt) = 1.0
      ldata(9,jt) = -1.0
      ldata(10,jt) = -1.0
C                                    Compute L(0), dL/dX
      vk = real(e(kt))
      ak = real(f(kt))
      ek = vk * cos (ak)
      fk = vk * sin (ak)
      vm = real(e(mt))
      am = real(f(mt))
      em = vm * cos (am)
      fm = vm * sin (am)
      ikr = 0.0 
      iki = 0.0 
        
      g12 = real (yeq(1,2)) 
      b12 = aimag (yeq(1,2))
      imr = em * g12 - fm * b12 
      imi = em * b12 + fm * g12 
      ikr = ikr + imr   
      iki = iki + imi   
      rh = imr * fk - imi * ek  
      rn = imr * ek + imi * fk  
      rj = -rn  
        
      ldata(13,jt) = -rh
      ldata(14,jt) = -rj
        
      vksq = vk ** 2
      g12 = real (yeq(1,1)) * vksq  
      b12 = aimag (yeq(1,1)) * vksq 
      pk = ikr * ek + iki * fk + g12
      qk = ikr * fk - iki * ek - b12
      rh = -qk - b12
      rj = pk - g12 
        
      ldata(11,jt) = -rh
      ldata(12,jt) = -rj
      ldata(6,jt) = pk  
        
      if (debug) then   
         write (dbug,290) jt,(i,kldata(i,jt),i=1,5),
     1     (i,ldata(i,jt),i=6,14)   
  290    format (' BLDTRN/LDATA - row ',i3,5(i3,i12) /  
     1          (23x,5(i3,e12.5)))  
      endif 
        
  300 continue  
      return
      end   
