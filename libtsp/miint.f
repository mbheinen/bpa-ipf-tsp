C    %W% %G%
      subroutine miint(ix, angnow, nim, pgenim, qgenim, pld, qld, sqz, 
     &                 xc, vhi, vhr, sina, cosa, vsqr, qerr)
C     -
C     -    Subroutine to initialize a single induction motor.  This
C     -       code has been moved from INITL3, which now calls it.
C     -    Checks are made for too many motors and for unrealistic
C     -       values of slip and power factor.
C     -    There are two maximum slips, a normal (warning) high and a
C     -       fatal high.
C     -
      include 'tspinc/params.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/gentblc.inc'
      include 'tspinc/int3.inc'
      include 'tspinc/spare1.inc'

      common /wscc_debug/xdebug(20)

C     -  Local variables

      logical debug
      character gname*16
      logical qerr

      data slipmin, slipmax, slipftl/.003, .05, .07/

C     call genname (ix, gname)                                       !dem
C     if (gname .eq. 'MOT MII  500.0 1') then
C     debug = .true.
C     else
C     debug = .false.
C     endif

C     -    Check if too many induction motors
      nim = nim+1
      if ( nim .gt. MAXINDMTR ) then
        iabort = 1
        write (errbuf(1), 10000) MAXINDMTR
10000   format (' Number of induction motors exceeds the limit of ', 
     &   i3)
        call prterr('E', 1)
        qerr = .true.
      else

C       -   Start calculations from PF conditions
        pgen1 = -pld*datat(15)
        qgen1 = -qld*datat(16)
        imlv = imlv+1
        imlvi(imlv) = j

C       -   Fetch impedances
        rs = datat(6)+datat(18)
        xs = datat(7)+datat(17)
        rr = datat(9)
        xr = datat(10)
        xm = datat(8)
        pim = -pgen1
        qim = -qgen1

C       -   Test for negative induction motor load
        if ( pim .le. 0. ) then
          write (errbuf(1), 10010) name, base, id
10010     format (' Induction motor at ', a8, 2x, f5.1, 2x, a1, 
     &     ' has a negative load.')
          call prterr('E', 1)
          iabort = 1
          qerr = .true.
        else

C         -   Do initializing
          xsrm = xs*xr+xs*xm+xr*xm
          xrm = xr+xm
          xsm = xs+xm
          a1 = rs*rs+xsm*xsm
          b1 = 2.*rs*xm*xm
          c1 = xsrm*xsrm+rs*rs*xrm*xrm
          d1 = pim/vsqr

C         -   Calculate rotor slip resistance (Stab User Manual, pg 2-8)
          a2 = a1*d1-rs
          b2 = b1*d1-xm*xm
          c2 = c1*d1-rs*xrm*xrm
          b2sqac = b2*b2-4.*a2*c2

C         -   Check for positive value under square root
          if ( b2sqac .lt. 0. ) then
            write (errbuf(1), 10020) name, base, id, b2sqac
10020       format (' Induction motor at ', a8, 2x, f5.1, 2x, a1, 
     &       ' has complex slip resistance.  Imag part squared is '
     &       , e12.5)
            call prterr('E', 1)
            iabort = 1
            qerr = .true.
          else

C           -   OK to calculate R_slip
            b2sqrt = sqrt(b2sqac)
            r2h = (-b2+b2sqrt)/(2.*a2)
            r2l = (-b2-b2sqrt)/(2.*a2)

C           -   Since we can have two values for R_slip, we must check them
C           -      both and both their resultant slips, though we prefer
C           -      to use lower slip.
C           -   If larger R_slip is negative, then smaller one will be more
C           -      negative.
            if ( r2h .lt. 0. ) then
              write (errbuf(1), 10030) name, base, id, r2h, r2l
10030         format (' Induction motor at ', a8, 2x, f5.1, 2x, a1, 
     &         ' has both R_slips negative (', f9.5, ' and ', f9.5, 
     &         ' )')
              call prterr('E', 1)
              iabort = 1
              qerr = .true.
            else

C             -   This fudge prevents SLIPH from being < SLIPL
              if ( r2l .le. 0. ) r2l = r2h

C             -  Slip calculation
              slipl = rr/r2h
              sliph = rr/r2l

C             -   Prefer to use lower slip and higher R_slip
              slip0 = slipl
              r2 = r2h

C             -    Use high slip if low slip below minimum
              if ( slipl .lt. slipmin ) then
                slip0 = sliph
                r2 = r2l
              endif

C             -    Use low slip if high slip above maximum
              if ( sliph .gt. slipmax ) then
                slip0 = slipl
                r2 = r2h
              endif

C             -  Check if both slips are below minimum.
              if ( sliph .lt. slipmin ) then
                write (errbuf(1), 10040) name, base, id, slipmin
10040           format (' Induction motor at ', a8, 2x, f5.1, 2x, a1, 
     &           ' has both slips less than allowed minimum of ', f7.4)
                write (errbuf(2), 10050) slipl, sliph
10050           format (' Low slip = ', f9.5, ';  High slip = ', f9.5)
                call prterr('E', 2)
                iabort = 1
                qerr = .true.
C             -   Slips > min, check if < fatal max
              elseif ( slipl .gt. slipftl ) then
                write (errbuf(1), 10060) name, base, id, slipftl
10060           format (' Induction motor at ', a8, 2x, f5.1, 2x, a1, 
     &           ' has both slips greater than allowed maximum of ', 
     &           f7.4)
                write (errbuf(2), 10070) slipl, sliph
10070           format (' Low slip = ', f9.5, ';  High slip = ', f9.5)
                call prterr('E', 2)
                iabort = 1
                qerr = .true.
              else

C               -    Check if slip < normal max
                if ( slipl .gt. slipmax ) then
                  write (errbuf(1), 10080) name, base, id, slipmax
10080             format (' Induction motor at ', a8, 2x, f5.1, 2x, a1, 
     &             ' has both slips greater than normal maximum of ', 
     &             f7.4)
                  write (errbuf(2), 10090) slipl, sliph
10090             format (' Low slip = ', f9.5, ';  High slip = ', 
     &             f9.5)
                  call prterr('W', 2)
                endif

C               -    Slip acceptable.  Calculate Q_consump & Pwr factr
                qgeni = vsqr*(xsm*r2*r2+xsrm*xrm)/(r2*(a1*r2+b1)+c1)
                pwrfct = cos(atan2(qgeni, pim))
C               -    Test if power factor is in acceptable range
                if ( pwrfct .lt. 0.7.or.pwrfct .gt. 0.95 ) then
                  write (errbuf(1), 10100) name, base, id, pwrfct
10100             format (' Induction motor at ', a8, 2x, f5.1, 2x, a1, 
     &             ' has power factor (', f8.5, 
     &             ') outside normal range of 0.7 to 0.95')
                  call prterr('W', 1)
                endif
C               -   Calculate Q_load remaining at bus
                qgen1 = -qgeni

C               SUMMING PGEN1,QGEN1 FOR IND.MTR.

                pgenim = pgenim+pgen1
                qgenim = qgenim+qgen1
C               -   Source impedances to R_slip (dir & quad)
                xdp = xs+xr*xm/(xr+xm)
                xqp = xdp
                ra = rs
                xd = xs+xm
                xq = xd
                rr = slip0*r2
                ad = (rr/(xr+xm))*6.2831852
                xdxdp = xd-xdp
                write (outbuf, 10110) name, base, id, r2, xm, slip0, rr
10110           format (' At ', a8, 2x, f5.1, 2x, a1, 5x, 
     &           '-  R2,XM,SLIP0,RR =', 4(1x, e12.5))
                call prtout(1)
C               -
                angp2(j) = -slip0*6.2831852
                xc = 0.5*(xdp+xqp)
C               -    Determine d-q voltages and currents
                agenr = (-pim*vhr-qgeni*vhi)/vsqr
                ageni = (-pim*vhi+qgeni*vhr)/vsqr
                write (outbuf, 10120)
10120           format ('0', 10x, 'PGEN1', 5x, 'QGEN1', 7x, 'VHR', 7x, 
     &           'VHI', 6x, 'VSQR', 5x, 'AGENR', 5x, 'AGENI')
                call prtout(1)
                write (outbuf, 10130) pgen1, qgen1, vhr, vhi, vsqr, 
     &           agenr, ageni
10130           format (4x, 7(1x, f9.5))
                call prtout(1)
C               -
                var = vhr+ra*agenr-xq*ageni
                vai = vhi+ra*ageni+xq*agenr
                pgn = var*agenr+vai*ageni
                govpwr(j) = pgn
                genp(j) = pgn
                angnow = atan2(vai, var)
                sina = sin(angnow)
                cosa = cos(angnow)
                agend = agenr*sina-ageni*cosa
                agenq = agenr*cosa+ageni*sina
                vhd = vhr*sina-vhi*cosa
                vhq = vhr*cosa+vhi*sina
                if ( ad .ne. 0.0 ) ad = 1./ad
                oid = agend
                oiq = agenq
                epq(j) = vhq+ra*agenq+xdp*agend
                epqo = epq(j)
                fd(j) = vhd+ra*agend-xqp*agenq
                epdo = fd(j)
                epdn = epdo
                epqn = epqo
                vfldtn(1, j) = 1000.
C               -    Per/unit freq deviation
                omega0 = 1.-slip0
C               -    Calculate C in the load torque equation
                ca = datat(12)
                cb = datat(13)
                cc = 1.-ca*omega0*omega0-cb*omega0
C               -    Contributions of each torque term
                all = ca*pgn
                bl = cb*pgn
                cl = cc*pgn
                sqz = 1./(ra*ra+xdp*xdp)
                xc = xdp
                qerr = .false.
C               -   Normal finish for induction motor init
C               if (debug) then
C               sparpt(1) = sqrt (vhr ** 2 + vhi ** 2)
C               sparpt(2) = atan2 (vhi, vhr)
C               sparpt(3) = sqrt (vhd ** 2 + vhq ** 2)
C               sparpt(4) = atan2 (vhq, vhd)
C               sparpt(5) = sqrt (oid ** 2 + oiq ** 2)
C               sparpt(6) = atan2 (oiq, oid)
C               sparpt(7) = sqrt (agend ** 2 + agenq ** 2)
C               sparpt(8) = atan2 (agenq, agend)
C               sparpt(9) = sqrt (fd(j) ** 2 + fq(j) ** 2)
C               sparpt(10) = atan2 (fq(j), fd(j))
C               xdebug(1) = 1.0 - slip0
C               xdebug(2) = slip0
C               xdebug(3) = pim
C               xdebug(4) = pgn
C               xdebug(5) = vhr
C               xdebug(6) = vhi
C               endif
              endif
            endif
          endif
        endif
      endif
      return
      end
