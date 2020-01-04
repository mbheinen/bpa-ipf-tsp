C    %W% %G%
      subroutine angsol
C    
C     THIS SUBROUTINE SOLVES THE  SWING EQUATION USING THE
C     TRAPEZOIDAL RULE. IT IS CALLED BY CNTRL.
C    
      include 'tspinc/params.inc'
      include 'tspinc/lshed1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/param.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/comvar.inc'

      common /wscc_debug/ xdebug(20)

C     -  Local variables

      logical debug
      character gname * 16, name*8

      data twopi /6.2831853/
      data pi    /3.1415927/

      call genname (ispf, gname)                                        !dem
      if (gname .eq. 'MOT MII  500.0 1') then
        debug = .true.
      else 
        debug = .false.
      endif
C    
C      RMFAC IS THE FREQUENCY DEPENDENCE FACTOR
C    
      if (mgen .ne. 2) then
        rcmfac = 2.0*pi/(2.0*pi+angp2(ispf))
        if (mfdep .ne. 2 .and. mfdep .ne. 4) rcmfac = 1.0
        consm = consm*rcmfac
      endif
C    
C         SOLVE SWING EQN BY THE TRAPEZ RULE...CORRECT
C    
      if (lppwr .eq. 0) then
        if (idsw .eq. 3 .or. idsw .eq. 5) then
          angl1(ispf) = angl(ispf)
          angp1(ispf) = angp2(ispf)
        endif
C      
C       PRECESS PWR DATA
C      
        paccm1(ispf) = pacc(ispf)
        pacc(ispf) = paccex(ispf)
        if (idsw .eq. 1 .or. idsw .eq. 2 .or. idsw .eq. 4 .or. 
     &      idsw .eq. 6) then
C        
C          SOLVE THE SWING EQN BY THE TRAPZOIDAL RULE...PREDICT
C        
          d13 = ddt1 + ddt2
          recip1 = 1.0/(ddt1*d13)
          recip2 = 1.0/(-ddt2*ddt1)
          recip3 = 1.0/(d13*ddt2)
          st32 = tc3 + tc2
          st13 = tc1 + tc3
          st21 = tc2 + tc1
          pt23 = tc2*tc3
          pt31 = tc3*tc1
          pt12 = tc1*tc2
          cn1 = 4.0*edt*datat(5)*pacc(ispf)
          dtc1 = edt*dampp*datat(5)
          cn2 = 2.0 - dtc1
          cn3 =  - 2.0*dtc1
          cd1 = 2.0 + dtc1
          wold = angp1(ispf)
          wnow = angp2(ispf)
          wnew = (cn1+wold*cn2+wnow*cn3)/cd1
          angp1(ispf) = wnow
          angnow = angl(ispf)
          angl1(ispf) = angnow
        else
          slope = (paccex(ispf)-pacc(ispf))/edt
          pnow = pacc(ispf)
          pnew = paccex(ispf)
C        
C             SOLVE SWING EQN BY THE TRAPEZ RULE...CORRECT
C        
          cn1 = datat(5)*edt
          dtc1 = dampp*edt*datat(5)
          cn2 = 2.0 - dtc1
          cn3 = 2.0 + dtc1
          wnow = angp1(ispf)
          wnew = (cn1*(pnow+pnew)+wnow*cn2)/cn3
          angnow = angl1(ispf)
        endif
      else
        pnew = paccex(ispf)
        pnow = pacc(ispf)
        cn1 = datat(5)*edt
        dtc1 = dampp*edt*datat(5)
        cn2 = 2.0 - dtc1
        cn3 = 2.0 + dtc1
        wnow = angp1(ispf)
        wnew = (cn1*(pnow+pnew)+wnow*cn2)/cn3
        angnow = angl1(ispf)
      endif
      if (mgen .eq. 2) then
        slip0 = angp2(ispf)
 
C       TEST IF IND MTR TO BE SWITCHED TO BLOCKED ROTOR REP
 
        if (datat(26) .eq. -2.0) then
          wnew = wnow
          paccex(ispf) =  - 4000.0
        endif
      endif
      angp2(ispf) = wnew
      spdavg = (wnow+wnew)*0.5
      angnew = spdavg*edt + angnow
      pasta = angl(ispf)
      if (mgen .eq. 2) then
        if (abs(angnew) .ge. twopi) angnew = angnew - sign(twopi,
     &   angnew)
      endif
      angl(ispf) = angnew
      spdslp = (wnew-wnow)/edt
      if (keybrd(16) .ne. 0) then
        cm = 1.0/consm
        write (outbuf, 10000) ispf, cm
        call prtout(1)
10000   format (' ', ' MACH NO., CONSM = ', i5, e18.8)
      endif
      if (keybrd(3) .ne. 0) then
        write (outbuf, 10010) paccex(ispf), edt, dampp, wold, wnow,
     &   wnew, angnow, angnew
        call prtout(1)
10010   format ('01C,5002', 8e15.8)
      endif
      if (lppwr .ne. 0) then
        angdif = abs(angl(ispf)-pasta)
C      
C            CK FOR ANGLAR DIFF
C      
        if (angdif .gt. delang) then
          itol = 2
C        
C         UPDATE FOR LARGEST ANGLE DEVIATION
C        
          if (angdim .ge. angdif) then
            angdim = angdif
            igmx = igbn
          endif
        endif
      endif
      if (mgen .eq. 2) then
        slip = angp2(ispf)/6.2831852
        omega = 1. + slip
        if (abs(slip) .gt. 1.) then
          i1 = igentn(1, ispf)
          name = bname(i1)
          bkv = buskv(i1)
          write (outbuf, 10020) name, bkv
10020     format ('0', 5x, ' SLIP AT INDUCTION MACHINE ', a8, 2x, f5.1,
     &     ' EXCEEDS 1.0. SWING WILL BE ABORTED. ')
          call prtout(1)
          jexit = 1
        else
          govpwr(ispf) = all*omega*omega + bl*omega + cl
          if (keybrd(3) .ne. 0) then
            i = 1526
            write (outbuf, 10030) i, slip, omega, govpwr(ispf)
            call prtout(1)
10030       format ('01C', i5, 3e14.6)
          endif
          if (debug) then          
            xdebug(4) = govpwr(ispf)
          endif
        endif
      endif
      return
      end
