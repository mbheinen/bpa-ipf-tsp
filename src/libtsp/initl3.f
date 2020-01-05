C    %W% %G%
      subroutine initl3

C     This subroutine uses solution data from the power flow solution
C     history file to initialize all load and machine tables.
C     It is called by SWINGM.

C     Revs:
C     Sep/16/92 - DEM:  Undid logic to change sign of PSS input gains if
C     gen is motoring.

C     This version calls MIINT instead of inline code to initialize
C     induction motors

      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc' 
      include 'tspinc/cntrl2.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/matrow.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/shdlod.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/workc.inc'
      include 'tspinc/packtn.inc'
      include 'tspinc/lnet.inc'
      include 'tspinc/ldndxp.inc'
      include 'tspinc/ldidxn.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/busdta.inc'
      include 'tspinc/busnum.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'

      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/gentblc.inc'
      include 'tspinc/buslod.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/nsavdc.inc'
      include 'tspinc/citer.inc'
      include 'tspinc/int3.inc'
      include 'tspinc/gov.inc'
      include 'tspinc/turb.inc'
      include 'tspinc/spare1.inc'

      common /ecion/ecsn(6)
      dimension iecsn(6)
      equivalence (ecsn, iecsn)
      dimension bsbr(200)
      dimension jbus(200), gij(199), bij(198)
      equivalence (bsbr, jbus, pnet), (bsbr(2), gij, qnet), (bsbr(3), 
     &            bij, gii), (bsbr(4), bii), (bsbr(9), pload), (bsbr
     &            (10), qload), (bsbr(11), ainetr), (bsbr(12), aineti)
      common /temp/tempn(2, 5), itempn(4, 5)

      equivalence (kptbl1, igecse)
      equivalence (creg(1), vrmax), (creg(2), vrmin), (creg(3), vref), 
     &            (creg(4), efdmax), (creg(5), cki), (creg(6), hbr), 
     &            (creg(7), cckp, ckpr), (creg(8), cka), (creg(9), dr), 
     &            (creg(10), ckc), (creg(11), v1max), (creg(12), v1min)
     &            , (creg(13), va), (creg(15), hcb), (creg(16), ckj), 
     &            (creg(17), db), (creg(18), dc), (creg(19), vgmax), 
     &            (creg(20), xl), (creg(21), rcex), (creg(22), vr), 
     &            (creg(23), xcex), (creg(24), ckg), (creg(25), da), 
     &            (creg(26), hba), (creg(27), vco), (creg(28), theta, 
     &            ckpi), (creg(29), efd), (creg(30), vi)
      equivalence (ivgt1, vgt1), (asat, iasat), (istgt1, cstgt1), 
     &            (bsat, ibsat)
      equivalence (aeps, ar), (aes, brs, af1), (bes, bf1), (ba1s, br), 
     &            (baps, a1), (bfs, a2)
      equivalence (ae, vfdo), (aep, bfx), (be, ba1x), (bf1, bapx)
      equivalence (creg(27), vrmlt)
      character*8 namesr, nameln
      character*2 lnx
      character ch90*90, gname*16
      logical qerr, found, finished

      data slipl, sliph/0.003, 0.05/
      data lnx/'LN'/

      call mpost('INITL3')
      nim = 0
      itslkt = 0
      wnow = 0.0     ! Initialize value in common /comvar/

C     Define timestep in seconds
      sdt = dt/frqbse
      isg = isgg

      call initmat()

C     Add Newton option
C     Iniialize admittance tables for Newton option
      ii = 0
      do i = 1, nmx
        busp(i) = 0.0
        busq(i) = 0.0
        yreal(i) = 0.0
        yimag(i) = 0.0
        gadmit(i) = 0.0
        badmit(i) = 0.0
        gnewt(2*i-1) = 0.0
        gnewt(2*i) = 0.0
        bnewt(2*i-1) = 0.0
        bnewt(2*i) = 0.0

C       Initialize getmat/putmat

        call putmat(i, ii)
      enddo
      nc = 0
      if (ndc .gt. 1) nc = 1
      mts = 27
      inet = 0
      i182 = 1
      isgg1 = isgg + 1
      ivtyp3 = 1
      inmx = 0
      incrm = 0
      imlv = 0
      lsknt = ls

C     This subroutine is one big loop to initialize bus quantities,
C     machine data tables, and y matrix tables.  All tables have
C     now been converted to internal order; however, some bus
C     numbers in the machine data tables are still in external order
C     and must be converted.

      do i = 1, nmx
        ctrr = 0.0
        ctri = 0.0
        call redecs(bsbr, kkp(1, i)+kecst, kkp(2, i))
        if (nmx .eq. i) then
          if (nmx .ne. kbsknt .and. ifltsw .eq. 1) goto 280
        endif
        vsq = eyr(i)**2 + eyi(i)**2
        emag = 1.0/sqrt(vsq)
        eang = atan2(eyi(i), eyr(i))
        emagrn(1, i) = emag
        emagrn(2, i) = eang

        if (capcor(1, i) .ne. 0.0) then
          cap = capcor(1, i)
          bint = capcor(2, i)
          bii = bii - cap
          qnet = qnet + vsq*cap
        endif
        if (ainetr .ne. 0.0 .or. aineti .ne. 0.0) then
          vmagx = sqrt(vsq)
          pload = pload + ainetr*vmagx
          qload = qload - aineti*vmagx
          pnet = pnet - ainetr*vmagx
          qnet = qnet + aineti*vmagx
          ainetr = 0.0
          aineti = 0.0
          call ritecs(bsbr(1), kkp(1, i)+kecst, 2)
          call ritecs(bsbr(9), kkp(1, i)+kecst+8, 4)
        endif
        pgen = pload + pnet
        qgen = qload + qnet
        if (abs(pgen) .lt. 0.000001) pgen = 0.0
        if (abs(qgen) .lt. 0.000001) qgen = 0.0
        if (ipactn(3, i) .ne. 0) i182 = 2
        if (ipactn(2, i) .gt. 0) then

C         Device is a machine
C         Start of generator initialization
C         J is the generator number

          j = ipactn(2, i)
          vhr = eyr(i)
          vhi = eyi(i)

C         Assign initial mech & elec powers

          govpwr(j) = pgen
          genp(j) = pgen
          pld = pgen - pnet
          qld = qgen - qnet

C         MGNKNT is the number of generators on bus I

          mgnknt = ipactn(1, i)
          jhg = 0
          jlg = 0
          igrh = 0

C         IMFLAG = 0 (default) means only induction motors are connected
C         to this bus. if so, load netting should be properly done.
C         IMFLAG = 1 means synchronous generators are connected to this bus.

          imflag = 0

C         PGENIM, QGENIM will be the sum of PGEN1,QGEN1 of all induction
C         motors on a bus. They will be adjusted with the PLD, QLD at that
C         bus after all machines on that bus have been processed.

          pgenim = 0.0
          qgenim = 0.0
        else

C         --  Device is a load, or a generator netted into a negative load
          if (pgen .eq. 0.0) then
            pld =  - pnet
            qld =  - qnet
          else

C           --     Search LNET table for bus I
            name = exnamc(i)
            namesr = name
            found = .false.
            i1 = 1
            i2 = ln
            do while (i1 .le. i2 .and. .not. found)
              iname = (i1+i2)/2
              nameln = lnetc(iname)
              komp = kompr(nameln, namesr, kdum)
              if (komp .lt. 0) then
                i1 = iname + 1
              elseif (komp .gt. 0) then
                i2 = iname - 1
              else
                kbsr = ixnamn(i)
                kbln = lnetn(iname)
                if (kbln .lt. kbsr) then
                  i1 = iname + 1
                elseif (kbln .gt. kbsr) then
                  i2 = iname - 1
                else
                  pld =  - pnet
                  qld =  - qnet
                  found = .true.
                endif
              endif
            enddo
            if (.not. found) then
              ibase = ixnamn(i)
              base = basekv(ibase)
              write (errbuf(1), 10000) name, base
10000         format ('0 ', a8, 2x, f5.1, 
     &         'Load netting was not specified for this bus.')
              call prterr('E', 1)
              inet = inet + 1
              iabort = 1
              jabort = 1
              write (work80(inet), 10010) lnx, name, base
10010         format (a2, 1x, a8, f5.1)
              pld =  - pnet
              qld =  - qnet
            endif
          endif
          goto 250
        endif
        do while (.true.)
          vsqr = vhr*vhr + vhi*vhi
          lgnth = igentn(2, j)

C         The DATAT array corresponds to the CGEN (or ICGEN) array in
C         subroutine INPUT3.

          read (l2, rec = j) (igndta(iii, j), iii = 1, 8), (datat(iii), 
     &     iii = 1, lgnth)
          jabort = 0
          mgen = 0
          mex = 0
          mex1 = 0
          msupp = 0
          igr = 0
csw
c         igv = 0

          iex = 0
          isup = 0

C         MGEN is machine type:
C         1 for classical
C         2 for an induction motor
C         3 for d-q with const E'd & E'q
C         4 for d-q with const E_fd,
C         5 for d-q with exciter (i.e., variable E_fd),
C         6 for d-q with const E'd & E'q and damper windings
C         7 for d-q with const E_fd and damper windings
C         8 for d-q with exciter and damper windings
C         9 for static var

          mgen = igndta(1, j)
          if (mgen .ne. 2) imflag = 1
          name = exnamc(i)
          ibase = ixnamn(i)
          base = basekv(ibase)
          id = igentc(j)
          if (id .eq. 'L') jlg = j

          if (mgen .eq. 1) then

C           For classical machines

            pgen1 = pgen*datat(10)
            qgen1 = qgen*datat(11)

            govpwr(j) = pgen1
            genp(j) = pgen1
            pgn = pgen1
            igr = 11

          elseif (mgen .eq. 2) then

C           For induction motors

            pgen1 =  - pld*datat(15)
            qgen1 =  - qld*datat(16)
            call miint(j, angnow, nim, pgenim, qgenim, pld, qld, sqz, 
     &                 xc, vhi, vhr, sina, cosa, vsqr, qerr)
            if (qerr) goto 200
            goto 100
          elseif (mgen .ge. 3 .and. mgen .le. 8) then

C           For full rep machines

            pgen1 = pgen*datat(26)
            qgen1 = qgen*datat(27)
            igr = 27
            if (mgen .gt. 5) igr = 39

          elseif (mgen .eq. 9) then

C           For static var source

            call svsint()
            goto 200
          endif

C         For all generators or motors except induction

          if (mgen .ne. 2) then
            agenr = (pgen1*vhr+qgen1*vhi)/vsqr
            ageni = (pgen1*vhi-qgen1*vhr)/vsqr

            if (keybrd(24) .eq. 1) then
              write (outbuf, 10020)
10020         format ('0', 10x, 'PGEN1', 5x, 'QGEN1', 7x, 'VHR', 7x, 
     &         'VHI', 6x, 'VSQR', 5x, 'AGENR', 5x, 'AGENI')
              call prtout(1)
              write (outbuf, 10030) pgen1, qgen1, vhr, vhi, vsqr, 
     &         agenr, ageni
10030         format (5x, 7f10.5)
              call prtout(1)
            endif

C           Apply proper damping whether gen or motor

            dmpfac = abs(dmpfac*pgen1)
            if (mgen .ne. 1) then
              if (.not. (mgen .lt. 5 .or. mgen .eq. 6 .or. mgen .eq. 7)
     &         ) then

C               Here if MGEN = 5 (2-axis, dynamic E_fd)

                mex = igndta(5, j)
                if (mex .eq. 4 .or. igndta(2, j) .eq. 0) then

C                 Here for all generators (except inducs or SVC's)
C                 R_t & X_t used with "MZ" model

                  if (xt .gt. 0.0 .and. rt .gt. 0.0) then
                    vlr = vhr + rt*agenr - xt*ageni
                    vli = vhi + rt*ageni + xt*agenr
                    vsqr = vlr*vlr + vli*vli
                  endif
                elseif (mex .eq. 4) then

C                 Here for all generators (except inducs or SVC's)
C                 R_t & X_t used with "MZ" model

                  lvref = igndta(2, j)
                  if (xt .gt. 0.0 .and. rt .gt. 0.0) then
                    vlr = vhr + rt*agenr - xt*ageni
                    vli = vhi + rt*ageni + xt*agenr
                    vsqr = vlr*vlr + vli*vli
                  endif
                elseif (igndta(2, j) .ne. 0) then
                  lvref = igndta(2, j)
                  lvref = indx2n(lvref)
                  igndta(2, j) = lvref
                  if (lvref .ne. i) vsqr = eyr(lvref)**2 + eyi(lvref)*
     &             *2

C                 Here for all generators (except inducs or SVC's)
C                 R_t & X_t used with "MZ" model

                elseif (xt .gt. 0.0 .and. rt .gt. 0.0) then
                  vlr = vhr + rt*agenr - xt*ageni
                  vli = vhi + rt*ageni + xt*agenr
                  vsqr = vlr*vlr + vli*vli
                endif
              endif
            endif
          endif

          vmag = sqrt(vsqr)
          efdo = 0.0

          if (mgen .eq. 1) then

C           Classical machine: voltage behind X'_d

            eg = vhr - ageni*xdp
            fg = vhi + agenr*xdp
            vmag = sqrt(eg*eg+fg*fg)
            fd(j) = vmag
            angnow = atan2(fg, eg)
            bii = bii - 1.0/xdp
            sina = sin(angnow)
            cosa = cos(angnow)
            ctrr = (vmag/xdp)*sina
            ctri =  - (vmag/xdp)*cosa

C           Terminal voltage in d,q coords

            eyri(j) = vhr*sina - vhi*cosa
            eyii(j) = vhr*cosa + vhi*sina
            epq(j) = 0.0
            vfldtn(1, j) = 0.0
            vfldtn(2, j) = 0.0
            goto 110
          else

C           Full-represented machine

            xc = 0.5*(xdp+xqp)

C           Check if sataturation represented and get SD, SQ

            if (esat .eq. 0.0) then
              xp = 0.0

C             Voltage behind armature resistance

              vpr = vhr + ra*agenr
              vpi = vhi + ra*ageni
              satd = 0.0
              satq = 0.0
            else
              if (xp .eq. 0.0) then
                xp = 0.95*xdp
                write (ch90, '(a,1x,f7.4,1x,a,1x,a)') name, base, id, 
     &           ': has X_l or X_p = 0.0'
                call puterr(1, ch90)
                write (ch90, '(a,a,f8.5)') 
     &           '  It is changed to 95% of X''d.', 'New value is ', xp
                call puterr(2, ch90)
                call prterr('W', 2)
              endif

C             Voltage behind Potier reactance -> E_p

              vpr = vhr + ra*agenr - xp*ageni
              vpi = vhi + ra*ageni + xp*agenr
              vpm = sqrt(vpr**2+vpi**2)

C             Saturation is based on V_p

              if (vpm .gt. esat) then
                satd = (csat*(vpm-esat)**2)/vpm
                satq = (xq/xd)*satd
              else
                satd = 0.0
                satq = 0.0
              endif
            endif
            xqsat = (xq-xp)/(1.0+satq)

C           Calculate voltage behind X_q

            vqr = vpr - ageni*xqsat
            vqi = vpi + agenr*xqsat

C           Rotor angle is based on V_q above

            angnow = atan2(vqi, vqr)

C           P_elec = V_p * conj (I_t)

            pgn = vpr*agenr + vpi*ageni

            govpwr(j) = pgn
            genp(j) = pgn

C           Determine d- and q-axis voltages & currents

            sina = sin(angnow)
            cosa = cos(angnow)

C           Terminal current & voltage in d,q coords

            agend = agenr*sina - ageni*cosa
            agenq = agenr*cosa + ageni*sina
            vhd = vhr*sina - vhi*cosa
            vhq = vhr*cosa + vhi*sina
            xqps = (xqp-xp)/(1.0+satq) + xp

C           voltage behind X'q (sat) -> E'd

            fd(j) = vhd + ra*agend - xqps*agenq
            xdps = (xdp-xp)/(1.0+satd) + xp

C           voltage behind X'd sat  -> E'q

            epq(j) = vhq + ra*agenq + xdps*agend

C           voltage behind X_d) * sat -> E_fd

            vfldtn(1, j) = epq(j)*(1.0+satd) + (xd-xdp)*agend
            epqo = epq(j)
            epdo = fd(j)
            efdo = vfldtn(1, j)

C           I_fd should = E_fd * (1 + satd)

            if (mgen .ge. 6) then
              if (xdpp .le. 0.0) xdpp = xp
              if (xqpp .le. 0.0) xqpp = xdpp
              xdpps = xp + (xdpp-xp)/(1.0+satd)
              xqpps = xp + (xqpp-xp)/(1.0+satq)

C             Voltage behind X"d sat --> E"q

              epq(j) = vhq + ra*agenq + xdpps*agend
              edpqo = epq(j)
              fd(j) = vhd + ra*agend - xqpps*agenq
              edpdo = fd(j)

C             Compute and pack initial field current with field voltage

C             cfd = efdo / ( 1. + satd )                                     !wlp
              cfd = efdo
              vfldtn(2, j) = cfd
              if (aqp .ne. 0.0) aqp = 1.0/aqp
              if (adp .ne. 0.0) adp = 1.0/adp
            endif

C           Modify generator table for trapezoidal rule

            if (aq .ne. 0.0) aq = 1.0/aq
            if (ad .ne. 0.0) ad = 1.0/ad
            oid = agend
            oiq = agenq

C           Modify diagonal admittance term for trapezoidal rule
C           SQZ is inverse of complex magnitd squared for division

            sqz = 1.0/(ra*ra+xqp*xdp)
            xc = 0.5*(xdp+xqp)
            if (mgen .ge. 6) then

              sqz = 1.0/(ra*ra+xqpp*xdpp)
              xc = 0.5*(xdpp+xqpp)
              xqr = (xq-xqpp)/(xqpp-xqp)
              xdr = (xd-xdpp)/(xdpp-xdp)

            endif
          endif

C         Detailed machines and induction motors here. Exclude classical
C         machines and SVS

C         INEWTS = 1 for YISOLN, 2 for NEWTON option

  100     if (inewts .eq. 2) then

C           Store extended mach quantities for use in DERIV.

            badmit(i) = badmit(i) - xc*sqz
            gadmit(i) = gadmit(i) + ra*sqz

          else

            bii = bii - xc*sqz
            gii = gii + ra*sqz

          endif

          eyii(j) = vhq
          eyri(j) = vhd
          ctrr = (vhr*ra+vhi*xc)*sqz + oid*sina + oiq*cosa
          ctri = (vhi*ra-vhr*xc)*sqz + oiq*sina - oid*cosa
  110     angl(j) = angnow
          angl1(j) = angnow

C         For induction motors only

          if (mgen .eq. 2) xdxdp = angnow
          vmagt(j) = vmag

C         Change GENDROP values from LS cards to reflect initial power

          ibndrp = igentn(1, j)
          do igndrp = 1, ifcd
            if (mflt(igndrp) .eq. 4) then
              if (dmpg(igndrp) .ne. 0.0) then
                if (iftabn(igndrp) .eq. ibndrp) then
                  if (idgnc(igndrp) .eq. id) then
                    cyc(igndrp) = cyc(igndrp) - 10000.0
                    if (dmpg(igndrp) .le. -90000.) then
                      dmpg(igndrp) =  - pgn
                    else
                      dmpg(igndrp) = dmpg(igndrp)*pgn/pgen1
                      if (abs(dmpg(igndrp)) .gt. pgn) then
                        iabort = 1
                        j = iftabn(igndrp)
                        id = idgnc(igndrp)
                        kbase = ixnamn(j)
                        name = exnamc(j)
                        base = basekv(kbase)
                        write (errbuf(1), 10040) name, base, id
10040                   format ('0 Gen dropping for (', a8, 2x, f6.1, 
     &                   2x, a1, 2x, ')')
                        write (errbuf(2), 10050) dmpg(igndrp), pgn
10050                   format (
     &                   '0 Power to be dropped exceeds total machine po
     &wer '
     &                   , 'pdrop = ', f7.3, ' pgen = ', f7.3)
                        call prterr('E', 2)
                      endif
                    endif
                  endif
                endif
              endif
            endif
          enddo
          if (keybrd(24) .ne. 0) then
            write (outbuf, 10060) name, base, id
10060       format ('0', a8, f5.1, 2x, a1)
            call prtout(1)
            write (outbuf, 10070) esat, csat, satd, satq, angnow, fd(j)
     &       , epq(j), efdo
10070       format (9(3x, f7.4))
            call prtout(1)
          endif

C         initialize governor 
          mgov = govtyp(j)
          if (mgov.ne.0) call govint(j, dt, iabort)

c         initialize cross compound indices
          mturb = turbtyp(j)
          if (mturb .ge. 4 .and. mturb .lt. 8 .and. id .eq. 'H') then
            jhg = j
            igrh = 7
            if (mgen .gt. 2) igrh = mts
            if (mgen .gt. 5) igrh = 39
          endif


c         start of exciter initialization
C         Skip if no exciter for this gen

          if (igndta(5, j) .ne. 0) then
            mex = igndta(5, j)
            iex = 35
            if (mex .gt. 10) iex = 5
            do jk = 1, iex
              creg(jk) = datat(igr+jk)
            enddo
            igr = igr + iex
            efdoi = efdo
            if (.not. (id .ne. 'H' .and. id .ne. 'L')) then
              if (lvref .eq. 0) then

C               Reactance compensation of terminal voltage for cross compound
C               machines

                vmag = vmag + (0.05/vmag)*(vhi*agenr-vhr*ageni)
                vmagt(j) = vmag
              endif
            endif

C           Test for new IEEE excit

            if (mex .gt. 10) then

C             Process new IEEE exciter models

              mex1 = mex - 10
              if (mex .eq. 11) then
                call int3fa(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 12) then
                call int3fb(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 13) then
                call int3fc(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 14) then
                call int3fd(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 15) then
                call int3fe(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 16) then
                call int3ff(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 17) then
                call int3fg(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 18) then
                call int3fh(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 19) then
                call int3fj(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 20) then
                call int3fk(citer(1, j))
                if (iabort .eq. 1) goto 200
              elseif (mex .eq. 21) then
                call int3fl(citer(1, j))
                if (iabort .eq. 1) goto 200
              else
                write (errbuf(1), 10080) name, base, id, mex
                call prterr('E', 1)
10080           format ('0 Incorrect exciter mdl no for ', a8, 2x, 
     &           f5.1, 2x, a1, 5x, 'mex = ', i5)
                goto 200
              endif
            else
              if (mex .eq. 9) then
                vrref = efdo/vmag
                efdoi = vrref
                if (vrref .gt. efdmax) goto 180
                if (vrref .lt. efdmin) goto 180
              elseif (mex .eq. 8) then
                efdoi = efdo/vmag
                if (efdoi .gt. efdmax) goto 180
                if (efdoi .lt. efdmin) goto 180
                vrref = efdo/cke1
              elseif (mex .ne. 4) then
                vfmax = creg(1)
                vfmin = creg(2)
                if (efdo .gt. vfmax) goto 180
                if (mex .ne. 1) then
                  if (efdo .lt. vfmin) goto 180
                  if (mex .eq. 7) then
                    vrref = efdo
                    goto 120
                  endif
                endif
                ckep = 0.
                ckep = csatx*exp(esatx*abs(efdo))
                ck1 = 0.0
                ck2 = 0.0
                if (ckep .ne. 0.0) then
                  ck1 = csatx*exp(esatx*abs(efdo))*(1.0+efdo*esatx)
                  ck2 = efdo*efdo*esatx*csatx*exp(esatx*abs(efdo))
                endif
                if (cke .eq. 0.) cke =  - ckep
                vrref = (cke+ckep)*efdo
                vrmax = vfmax*(cke+csatx*exp(esatx*vfmax))

C               THE FOLLOWING PROCEDURE, WHICH TAKES INTO ACCOUNT POSSIBLE -VE
C               VALUES OF ESATX, IS USED TO COMPUTE VRMIN IN THE 1200 BUS PROGRAM.
C               VTEMP=CKE*VFMIN
C               IF(ESATX.GT.0.) GO TO 7172
C               IF(VFMIN) 7173,7174,7171
C               7171 VTEMP=VTEMP+(VFMIN-ESATX)*(VFMIN-ESATX)/CSATX
C               GO TO 7174
C               7172 IF(VFMIN.GT.-ESATX) GO TO 7174
C               7173 VTEMP=VTEMP-(VFMIN+ESATX)*(VFMIN+ESATX)/CSATX
C               7174 VRMIN=VTEMP
C               END OF THE 1200 BUS PROGRAM PROCEDURE.
                vrmin = vrmax*vrmlt
                if (vrmin .gt. vrmax) then
                  iabort = 1
                  jabort = 1
                  write (errbuf(1), 10090) name, base, id, vrmax, vrmin
10090             format ('0 Regulator limits improper for ', a8, 2x, 
     &             f5.1, 2x, a1, '      vrmax,vrmax = ', 2f5.2)
                  call prterr('E', 1)
                  goto 200
                endif
              elseif (efdo .gt. vbmax) then
                goto 180
              elseif (efdo .ge. 0.) then
                if (xt .gt. 0.) then
                  vtr = vlr
                  vti = vli
                else
                  vtr = vhr
                  vti = vhi
                endif
                vthr = ckp*vtr - cki*ageni
                vthi = ckp*vti + cki*agenr
                vthev = sqrt(vthr*vthr+vthi*vthi)
                curfd = efdo
                xlifd = 0.78*xl*curfd
                if (xlifd .le. vthev) then
                  vrref = efdo - sqrt(vthev*vthev-xlifd*xlifd)
                  vrmaxn = 1.1*vrmax
                  if (abs(vrref) .gt. vrmaxn) goto 170
                  if (abs(vrref) .gt. vrmax) then
                    vrmax = vrmaxn
                    vrmin =  - vrmax
                    write (errbuf(1), 10100) name, base, id
                    write (errbuf(2), 10110) vrref, vrmax, vrmin
                    call prterr('E', 2)
10100               format (
     &               '0  VRMAX,VRMIN ARE BEING RELAXED BY 10 PERCENT FOR
     & type d exciter for '
     &               , a8, 2x, f5.1, 2x, a1)
10110               format (
     &               '  VRREF AND NEW VALUES OF VRMAX,VRMIN ARE ', 3
     &               (2x, f5.2), ' respectively ')
                  endif
                  vref = vmag + vrref/cka
                  goto 130
                else
                  write (errbuf(1), 10120) name, base, id
10120             format (
     &             '0 Values of KI,KP are probably incorrect for type "D
     &"      exciter '
     &             , a8, 2x, f5.1, 2x, a1)
                  call prterr('E', 1)
                  iabort = 1
                  jabort = 1
                  goto 200
                endif
              else
                goto 180
              endif
              if (vrref .gt. vrmax) goto 170
              if (vrref .lt. vrmin) goto 170
  120         if (cka .gt. .001) vref = vmag + vrref/cka
              if (mex .eq. 6) vref = vmag
              if (mex .eq. 8) vref = vref + ckf1*efdo
              if (mex .eq. 5) then
                vbias = vmag
                vref = vmag
                if (cke .eq. 1.0) then
                  if (cka .gt. 10.) vref = vref + vrref/cka
                endif
              endif
  130         x1 = vmag
              x3 = vrref
              x4 = vrref
              x5 = vrref
              if (mex .eq. 8) x5 = ckf1*efdo
              x6 = 0.0
              cvt1 = vmag
              cvt2 = vmag

C             Modify exciter time constants for trapezoidal rule
C             Note: this section (down to SN 2248 applies only to old 'E'
C             type exciters

              ckap = cka
              efdo = vfldtn(1, j)
              if (mgen .gt. 5) efdo = vfldtn(1, j)
              vfd = efdo
              if (mex .eq. 9) vfd = vrref
              tdt = dt
              if (mex .eq. 3) dt = dt/dtsc
              if (mex .eq. 3 .and. dtsc .lt. 0.0) dt = 0.1

C             IF (MEX1 .EQ. 9) DT = DT/DTSJ

              efdmn = vfmin
              if (mex .eq. 4) efdmn = 0.0
              if (mex .eq. 7) efdmn = vrmin
              if (mex .eq. 8) efdmn = efdmin
              if (mex .eq. 9) efdmn = efdmin
              ar = (2.0*creg(13))/dt + 1.0
              br = (ar-2.0)*vmag + cvt2
              if (mex .ne. 5) then
                atf = (2.0*creg(10))/dt + 1.0
                akf = (2.0*creg(6))/dt
                if (mex .eq. 2) af1 = (2.0*creg(14)/dt) + 1.0
                if (mex .eq. 8) af1 = (2.0*creg(14)/dt) + 1.0
                if (mex .eq. 2 .or. mex .eq. 8 .or. mex .eq. 9) then
                  bf = (atf-2.0)*x6 - akf*x5
                  if (mex .ne. 9) then
                    bf1 = (af1-2.0)*x5 + x4
                    if (mex .eq. 8) then
                      bf1 = bf1 - x4 + x5
                      goto 140
                    endif
                  endif
                elseif (mex .eq. 3 .or. mex .eq. 6) then
                  bf = (atf-2.0)*x6 - akf*(ck1+cke)*efdo
                  if (mex .eq. 6) goto 150
                elseif (mex .eq. 5) then
                  goto 140
                else
                  bf = (atf-2.0)*x6 - akf*efdo
                endif
                aa1 = (2.0*creg(11))/dt + 1.0
                ba1 = (aa1-2.0)*x4 + x3
              elseif (cka .le. 10.) then
                bap = (2.0/dt)*x3 + cka*(2.0*vref-x1)
                goto 160
              endif
  140         aa = (2.0*creg(12))/dt + 1.0
              bap = (aa-2.0)*x3 + cka*(2.0*vref-x1)
              if (mex .gt. 6 .and. mex .ne. 10) goto 160
  150         ae = (2.0*creg(9))/dt + cke
              if (mex .eq. 4) then
                be = (ae-2.0*cke+1.0)*efdo
              else
                aep = ae + ck1
                be = (ae-ck1-2.0*cke)*efdo + x4 + 2.0*ck2
                if (mex .eq. 3) then
                  aeps = aep
                  aes = ae
                  bes = be
                  baps = bap
                  ba1s = ba1
                  bfs = bf
                  aess = ae
                endif
                if (mex .eq. 6) then
                  ta = creg(12)
                  aa = ta + dt/2.0
                  slopo = vref - x6 - x1
                endif
              endif
  160         if (mex1 .eq. 9) then
                brs = br
                bapx = bap
                ba1x = ba1
                bfx = bf
                ars = ar
              endif
              if (.not. (mex .ne. 3 .and. mex1 .ne. 9)) then
                if (dtsc .ge. 0.0) then
                  aa1s = aa1
                  akfs = akf
                  atfs = atf
                endif
              endif
              dt = tdt
              goto 190
  170         iabort = 1
              jabort = 1
              write (errbuf(1), 10130) name, base, id, vrmax, vrmin, 
     &         vrref
10130         format ('0 Initial reg. output violates limits for ', a8, 
     &         2x, f5.1, 2x, a1, '   VRMAX,VRMIN,VRREF = ', 3f5.2)
              call prterr('E', 1)
              goto 200

  180         write (errbuf(1), 10140) name, base, id, efdoi
10140         format ('0 Initial FLDVLT violates limit for ', a8, 2x, 
     &         f5.1, 2x, a1, 5x, ' Initial FLDVLT = ', f6.3)
              call prterr('E', 1)
              iabort = 1
              jabort = 1
              goto 200
            endif

C           End of section for only old 'E' type exciters


C           REGOUT stores the regulator voltage for output
C           SUPOUT stores the supplementary signal for output

  190       regout(j) = x3
            supout(j) = 0.0
            vo = vmag

            if (igndta(7, j) .ne. 0) then
              msupp = igndta(7, j)

C             MSUPP = 4 or 5 is a PSS with transient stabilizer

C             Note that the TEB to use is not indexed in generator data
C             but is referenced solely by position if a PSS related
C             flag says to pick up the next TEB.

              if (msupp .eq. 4 .or. msupp .eq. 5) then
                regout(j) = 0.0
                itslkt = itslkt + 1

C               If a manual TSL trip is requested set JFTABN to index in TSL tab

                do igndrp = 1, ifcd
                  if (mflt(igndrp) .eq. 9) then
                    if (ipcdtn(igndrp) .eq. 0) then
                      if (iftabn(igndrp) .eq. igentn(1, j)) then
                        if (idgnc(igndrp) .eq. id) jftabn(igndrp) = 
     &                   itslkt
                      endif
                    endif
                  endif
                enddo
              endif
              isup = 33
              do jk = 1, isup
                csupp(jk) = datat(igr+jk)
              enddo
              isup1 = isup
              if (csupp(33) .gt. 0.0) then
                isup1 = 42
                do jk = 1, 9
                  csupp(isup+jk) = datat(igr+isup+jk)
                enddo
              endif
              csupp(28) = vmag

C             Initialize temp new PSS (inactive)

C             call ssinit (vmag)

              if (.not. (msupp .ne. 2 .and. msupp .ne. 5)) then
                if (jbuss .gt. 0) then
                  jbuss = indx2n(jbuss)
                else
                  jbuss = igentn(1, j)
                endif
              endif

C             Bus for PSS voltage input

              if (ckqv .ne. 0.) then
                if (jbusv .gt. 0) jbusv = indx2n(jbusv)
              endif
            endif
          endif

C         Here after excitation and governor systems init'ed

  200     if (mgen .eq. 1) igr = 7

C         For induction motor

          if (mgen .eq. 2) igr = 26
          if (mgen .gt. 2) igr = mts
          if (mgen .gt. 5) igr = 39
          if (mgen .eq. 9) igr = 4

C         Modification for exponential saturation

csw  filling datat up from cgov.
c         mgov = govtyp(j)
c         if (mgov .ne. 0) then
c           do jk = 1, igv
c             datat(igr+jk) = cgov(jk)
c           enddo
c           igr = igr + igv
c         endif
csw end

          if (mgen .ne. 9) then
            if (.not. (mgen .ne. 5 .and. mgen .ne. 8)) then
              do jk = 1, iex
                datat(igr+jk) = creg(jk)
              enddo
              igr = igr + iex
              if (msupp .ne. 0) then

C               DERIV  * * *

                if (msupp .ne. 3) ckqs = ckqs/6.2831852

C               -  Don't Change sign of PSS input transducer gains if motoring
C               (e.g. pump-storage in pump mode) since not done in WSCC prog.
C               if (pgen .lt. 0.0) then
C               ckqv = -ckqv                                                    !dem
C               ckqs = -ckqs                                                    !dem
C               write (errbuf(1),'( a8,2x,f5.1,2x,a1)') name,base,id            !dem
C               write (errbuf(1)(20:120),'(a)')                                 !dem
C               +    ': is motoring, so signs of PSS input gains are changed.'     !dem
C               call prterr ('W',1)                                             !dem
C               endif                                                             !dem
C               CKQV = SIGN(CKQV,PGEN)
C               CKQS = SIGN(CKQS,PGEN)

                vf1 = 0.0
                vf2 = 0.0
                if (ckqv .ne. 0.) then
                  if (jbusv .gt. 0) then
                    eik = eyr(jbusv)
                    fik = eyi(jbusv)
                    vf1 = sqrt(eik*eik+fik*fik)
                    csupp(28) = vf1
                  else
                    vf1 = vmag
                  endif
                endif
                csupp(4) = 2.0*csupp(4)/dt + 1.0
                do jk = 6, 13
                  csupp(jk) = 2.0*csupp(jk)/dt + 1.0
                enddo
                cn = atq*atq1*atq2
                denom = 1.0/(cn*atq3)
                cn = atq1p*atq2p*atq3p
                cn5 = cn*(atq-1.0)
                a2s = (cn5*ckqs*denom)/aqs
                a3s = (cn5*ckqv*denom)/aqv
                if (csupp(33) .gt. 0.0) then

C                 Initialization for notch filter

                  yp2 = csupp(36)*dt/2.0
                  xp2 = csupp(35)*dt/2.0
                  yp1 = bnf*dt*dt/4.
                  xp1 = dnf*dt*dt/4.
                endif
                do jk = 1, isup1
                  datat(igr+jk) = csupp(jk)
                enddo
                igr = igr + isup1
              endif
            endif
          endif

          igentn(2, j) = igr
          write (l2, rec = j) (datat(iii), iii = 1, igr)

  210     if (keybrd(24) .ne. 0) then
            write (outbuf, 10150) mgen, mgov, mex, msupp
10150       format ('0', 1x, 'MGEN,MGOV,MEX,MSUPP ', 4i10)
            call prtout(1)
            write (outbuf, 10160) j
10160       format ('0', 5x, 'DATA ', i5)
            call prtout(1)
            do jjj = 1, igr, 5
              kkk = min0(jjj+4, igr)
              write (outbuf, 10170) (kz, datat(kz), kz = jjj, kkk)
10170         format (2x, 5(i4, 2x, e12.5))
              call prtout(1)
            enddo
          endif
  220     mgnknt = mgnknt - 1

C         <0     =0    >0

          if (mgnknt .eq. 0) then

C           New WSCC/IEEE govnr. model-retrieve pgen for hp/lp set

c           skip if not cross compound governor
            if (jhg .eq. 0) goto 240

            lgnth = igentn(2, jhg)
            read (l2, rec = jhg) (datat(iii), iii = 1, lgnth)

c           calculate total power on the bus
            genpo = govpwr(jhg) + govpwr(jlg)

c           adjust reference for governor
c csw       datat(igrh+9) = genpo
            mgov = govtyp(jhg)

            if (mgov.eq.1) then
               govdat(govindx(jhg)+17) = genpo
            else if (mgov.ge.2.and.mgov.le.4) then
               govdat(govindx(jhg)+17) = genpo*govdat(govindx(ihg)+3)
            else if (mgov.eq.5) then
               govdat(govindx(jhg)+7) = genpo
            endif

            if (genpo.ge.govdat(govindx(jhg)+2)
     &         .and.genpo.le.govdat(govindx(jhg)+1)) then

c             power is between low and high limits

c             csw.  I dont know what this is doing
c             datat(igrh+11) = 0.0
c             datat(igrh+12) = (2.0/dt)*genpo
c             datat(igrh+13) = genpo*(datat(igrh+5)-1.0)
c             datat(igrh+14) = genpo*(datat(igrh+6)-1.0)
c             datat(igrh+15) =  - datat(igrh+1)*datat(igrh+3)/(datat
c    &                            (igrh+2)*datat(igrh+4))
c             datat(igrh+16) = (genpo+datat(igrh+12)*(datat(igrh+4)
c    &                         -1.0)*dt/2.0)/datat(igrh+4)
c             datat(igrh+26) = genpo*(datat(igrh+22)-1.0)
c             datat(igrh+27) = genpo*(datat(igrh+24)-1.0)

c       csw   usless, I think 
c             ajlg = jlg

c             calculate ratio of high to low pressure power
              if (govpwr(jlg) .ne. 0.0) then
                 pratio = govpwr(jhg)/govpwr(jlg)
              else
                 write (errbuf(1), 10142) name, base, id, govpwr(jhg),
     &             govpwr(jlg)
10142            format ('0 Illegal ratio of H/L governor power ', 
     &              a8, 2x, f5.1, 2x, a1, ' H = ', e11.3, ' L = ', 
     &              e11.3)
                 call prterr('E', 1)
                 iabort = 1
                 jabort = 1
              endif
c             save ratio value
c             datat(igrh+28) = pratio

c             save index where low power is stored
c             csw.  This is used later.
c             idatat(4) = jlg
              govlpi(jhg) = jlg

            else
              write (errbuf(1), 10180) name, base, id, datat(igrh+8),
     &           genpo, datat(igrh+7)
10180         format ('0 Initial power violates limit for ', a8, 2x, 
     &                 f5.1, 2x, a1, '     pmax,pgen,pmin=', 3e12.3)
              call prterr('E', 1)
              iabort = 1
              jabort = 1
            endif
            write (l2, rec = jhg) (datat(iii), iii = 1, lgnth)
            mgen = igndta(1, jhg)
            mgov = govtyp(jhg)
            mex = igndta(5, jhg)
            msupp = igndta(7, jhg)
            igr = lgnth
            goto 210
          endif
          if (mgnknt .le. 0) goto 240
          j = j + 1
        enddo

C       Subtract total ind.mtr. P,Q from load P,Q at this bus

  240   pld = pld + pgenim
        qld = qld + qgenim
        if (imflag .ne. 1) then

C         Load netting for a bus with ind.mtr. but w/o a syn.gen.

          pld = pld - pgen
          qld = qld - qgen

C         Jump to bus load rep processing

        endif

C       --  Prepare for load bus

  250   gld = pld/vsq
        bld =  - qld/vsq
        pltotn(1, i) = pld
        pltotn(2, i) = qld

C       Processing the lshed tables. If LSHDSH = 1, then the shunt
C       load at the bus (-VSQ*CAPCOR(2,I)) must be stored as it
C       will be dropped along with the load.

        if (lsknt .ne. 0) then
          do li = 1, ls
            ibno = lshdno(li)
            if (ibno .eq. i) then
              if (keybrd(31) .ne. 0) then
                name = exnamc(i)
                ibase = ixnamn(i)
                base = basekv(ibase)
                write (outbuf, 10240) name, base, pld
                call prtout(1)
                write (outbuf, 10190)
                call prtout(1)
10190           format ('0  LSHNDO  LSHDCD  LSHDLV  LSHDFR  LSHDSH  ')
                write (outbuf, 10200) lshdno(li), lshdcd(li), lshdlv
     &           (li), lshdfr(li), lshdsh(li)
10200           format ('0', 5(2x, i6))
                call prtout(1)
                write (outbuf, 10210)
10210           format (
     &           '0   SHDLDE   SHDTIM   SHDPCB   SHDLEV   SHDCDE   ', 
     &           'SHDDEL')
                call prtout(1)
                do lj = 1, 5
                  write (outbuf, 10220) shdlde(lj, li), shdtim(lj, li), 
     &             shdpcb(lj, li), shdlev(lj, li), shdcde(lj, li), 
     &             shddel(lj, li)
10220             format ('0', 6(2x, f7.4))
                  call prtout(1)
                enddo
              endif
              lsknt = lsknt - 1
              if (lshdsh(li) .eq. 1) then
                shdsht(li) =  - vsq*capcor(2, i)
                if (keybrd(31) .ne. 0) then
                  write (outbuf, 10230) shdsht(li)
10230             format ('0 SHUNT LOAD = ', f6.3)
                  call prtout(1)
                endif
              endif

C             If LSHDFR(LI) = 1, this is a BPA format bus and the load to be
C             shed, SHDLDE(1,LI), must be changed from mva to per unit of
C             of the inital bus load

              if (lshdfr(li) .ne. 0) then
                pratio = 1.0
                if (abs(pld) .lt. 1.0e-10) then
                  name = exnamc(i)
                  ibase = ixnamn(i)
                  base = basekv(ibase)
                  write (errbuf(1), 10240) name, base, pld
10240             format (' PLD ON BUS ', a8, 2x, f5.1, ' IS ', f8.4)
                  write (errbuf(2), 10250)
10250             format (' PRATIO FOR LOAD SHEDDING SET TO ZERO.')
                  call prterr('E', 2)
                  pratio = 0.0
                  iabort = 1
                endif
                nlevl = lshdlv(li)
                do lj = 1, nlevl
                  shdlde(lj, li) = pratio*shdlde(lj, li)/(pld*bmva)
                enddo
              endif
            endif
          enddo
        endif

C       Convert load representation P,Q to per unit

        if (lrep .ne. 0) then
          iecsl = ldidxn(6, i)
          if (iecsl .eq. 0) then
            yreal(i) = gld
            yimag(i) = bld
          else
            nitem = ldidxn(5, i)
            do while (.true.)
              ifreq = ldndxp(3, incrm+1)
              nitem1 = nitem
              nbusr = ldndxp(4, incrm+1)

C             Consistency check

              nbusl = i
              incold = incrm
              if (nbusl .ge. nbusr) incrm = incrm + 1
              if (nbusl .le. nbusr) goto 260
            enddo

C           Call LODINT to initialize load representation tables

  260       call lodint(i, pld, qld, nitem, iecsl, ifreq, nbusl, nbusr,
     &                  incrm)
            yreal(i) = gld
            yimag(i) = bld

C           Admittance solution INEWTS = 1, Newton solution INEWTS = 2

            if (inewts .eq. 2) then

C             Correct load data for dc converter buses

              finished = .false.
              found = .false.
              do while (nc .gt. 0 .and. nc .le. ndc .and. .not. 
     &         finished)
                if (nsavdc(nc) .lt. i) then
                  nc = nc + 1
                elseif (nsavdc(nc) .eq. i) then
                  found = .true.
                  finished = .true.
                else
                  finished = .true.
                endif
              enddo
              if (.not. found) then
                nc = 0
                gadmit(i) = gadmit(i) + gld
                badmit(i) = badmit(i) + bld
                goto 270
              endif
            endif
          endif
        endif

        gii = gii + gld
        bii = bii + bld

  270   if (i182 .eq. 2) then
          i182 = 1
          ivtyp3 = 2
        endif

C       Renumber and sort branches

  280   mbr = 13
        nbr = kkp(2, i) - 2
        do while (mbr .ne. nbr)
          kbr = mbr
          kbus = jbus(kbr)
          mbrp = mbr + 3
          do jbr = mbrp, nbr, 3
            if (jbus(jbr) .le. kbus) then
              kbr = jbr
              kbus = jbus(kbr)
            endif
          enddo
          if (kbus .gt. i) then
            jbr = nbr
            nbr = nbr - 3
          else
            jbr = mbr
            mbr = mbr + 3
          endif
          if (jbr .ne. kbr) then
            gij1 = gij(kbr)
            gij(kbr) = gij(jbr)
            gij(jbr) = gij1
            bij1 = bij(kbr)
            bij(kbr) = bij(jbr)
            bij(jbr) = bij1
            jbus(kbr) = jbus(jbr)
            jbus(jbr) = kbus
          endif
        enddo

C       Add diagonal and write out row

        nbr = kkp(2, i) + 1
        jbus(nbr) = i
        gij(nbr) = gii
        bij(nbr) = bii
        if (jbus(mbr) .lt. i) mbr = mbr + 3
        idell = (mbr-13)/3
        idelh = (nbr-mbr)/3
        matrow(1) = i
        matrow(2) = idell
        matrow(3) = idelh + 1
        ii = nbr - 3
        if (keybrd(4) .ne. 0) then
          write (outbuf, 10270) i
          call prtout(1)
10270     format (' ', 5x, 'ROW', i10)
        endif
        ind2 = 4
        ccr = 0.0 - ctrr
        cci = 0.0 - ctri
        do ind1 = 13, nbr, 3
          matrow(ind2) = jbus(ind1)
          atrow(ind2+1) = gij(ind1)
          atrow(ind2+2) = bij(ind1)
          jb = jbus(ind1)
          gg = gij(ind1)
          bb = bij(ind1)
          ee = eyr(jb)
          ff = eyi(jb)
          ccr = ccr + ee*gg - ff*bb
          cci = cci + ee*bb + ff*gg
          if (keybrd(4) .ne. 0) then
            write (outbuf, 10280) jb, gg, bb
10280       format (' ', 5x, 'JBUS', i8, 5x, 'GIJ', e12.5, 5x, 'BIJ', 
     &         e12.5)
            call prtout(1)
          endif
          ind2 = ind2 + 3
        enddo
        name = exnamc(i)
        kb = ixnamn(i)
        base = basekv(kb)
        if (keybrd(0) .ne. 0) then
          write (outbuf, 10290) name, base, ccr, cci
10290     format (5x, a8, f8.2, 10x, 'Initial current error is ('
     &     , e12.5, ',', e12.5, ')')
          call prtout(1)
        endif
        ii = ind2 - 1

C       Store y-matrix row

        if (ii .gt. 3*MATROWSIZE+3) then

          name = exnamc(i)
          kb = ixnamn(i)
          base = basekv(kb)
          ii = ii/3
          write (errbuf(1), 10300) name, base, ii
          call prterr('E', 1)
10300     format ('0 bus (', a8, f8.1, ' ) has (', i5, ' ) branches')
          iabort = 1
        else
          call putmat(i, ii)
        endif
      enddo

C     End of bus initialization block  * * * * * * * * * * * * *

      if (inet .ne. 0) then
        write (outbuf, 10310)
10310   format ('0', 'THE FOLLOWING LOAD NETTING CARDS ARE NEEDED.')
        call prtout(1)
        do itrr = 1, inet
          write (outbuf, 10320) (work(iy, itrr), iy = 1, 8)
10320     format (8a10)
          call prtout(1)
        enddo
      endif
      write (outbuf, 10330)
      call prtout(1)
10330 format ('0', 'SUBROUTINE INITL3 HAS BEEN PROCESSED.')
      return
      end

