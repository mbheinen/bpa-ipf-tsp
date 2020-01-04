C    %W% %G%
      subroutine genupd

C     * * *  THIS SUBROUTINE UPDATES GENERATOR POWER, TERMINAL VOLTAGE,
C     * * *  AND CURRENT INJECTION AFTER A SUCCESFUL NETWORK SOLUTION
C     * * *  USING THE NEW SOLUTION VOLTAGES. IT IS CALLED BY DERIV.

C     Revs:
C     Oct/10/92 - DEM:
C     Corrected error in intermediate calc for d and q axis currents
C     when at a discontinuity & gen has amortisseur windings.
C     -
      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/param.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/svs.inc'
C     -  Local variables
      data pi/3.1415927/

C     -  For each machine
      do i = 1, isg
        i1 = igentn(1, i)
        iecs = igentn(2, i)
        a = angl(i)
        sina = sin(a)
        cosa = cos(a)
        call redecs(datat, iecs, 27)
        mgen = igndta(1, i)
C       * * *
C       * * * STATIC VAR SOURCE
C       * * *
        if (mgen .eq. 9) then
          ksv = igndta(2, i)
csw
c         curr =    bsvs(ksv)*eyr(i1)
c         curi =  - bsvs(ksv)*eyi(i1)
          curr =    bsvs(ksv)*eyi(i1)
          curi =  - bsvs(ksv)*eyr(i1)
csw end
          if (iremte(ksv) .ne. 0) then
            lvref = iremte(ksv)
            vmagt(i) = sqrt(eyr(lvref)*eyr(lvref)+eyi(lvref)*eyi(lvref)
     &       )
          endif
          curn(1, i) = curr
          curn(2, i) = curi
C         * * *
C         * * * INERTIALESS MACHINE  [infbus]
C         * * *
        elseif (consm .eq. 0.0) then
          curn(1, i) = 0.0
          curn(2, i) = 0.0
C         * * *
C         * * * CLASSICAL MACHINE   [classgen]
C         * * *
        elseif (mgen .eq. 1) then
C         -  For class gen, P_elec = V_t * conj (I_t)
          genp(i) = eyr(i1)*eyri(i) + eyi(i1)*eyii(i)
          curr = eyri(i) - eyi(i1)/xdp
          curi = eyii(i) + eyr(i1)/xdp
          curn(1, i) = curr
          curn(2, i) = curi
        else
          wf = 1.0
C         * * *
C         * * * INDUCTION MACHINE   [indmot]
C         * * *
          if (mgen .eq. 2) then
            sina = sin(xdxdp)
            cosa = cos(xdxdp)
C           -  terminal voltage
            vhr = eyr(i1)
            vhi = eyi(i1)
C           * * *
C           * * *  TEST FOR BLOCKED ROTOR INDUCTION  MACHINE
C           * * *
            if (datat(26) .eq. -2.0) then
              rzsq = 1.0/(ra*ra+xdp*xdp)
              curr = (vhr*ra+vhi*xdp)*rzsq
              curi = (vhi*ra-vhr*xdp)*rzsq
C             -  For blocked rotor, P_elec = V_t^2 / R_load (?)
              genp(i) = vhr*curr + vhi*curi
              curn(1, i) = curr
              curn(2, i) = curi
              call ritecs(datat, iecs, 26)
            else
C             -  Terminal voltage in d,q coords
              vhd = vhr*sina - vhi*cosa
              eyri(i) = vhd
              vhq = vhr*cosa + vhi*sina
              eyii(i) = vhq
C             -  Impedance before voltage source
              xdmodd = xdmod*wf
              xqmodd = xqmod*wf
              xc = 0.5*(xdmodd+xqmodd)
              zsqi = 1./(ra*ra+xdmodd*xqmodd)
C             -  Voltage source (either E'x or E"x)
              fqtemp = fq(i)
              fdtemp = fd(i)
              fdvhd = fdtemp*wf - vhd
              fqvhq = fqtemp*wf - vhq
C             -  Current through R_ld + jX_r (call it I_r)
              oid = (fqvhq*xqmodd+fdvhd*ra)*zsqi
              oiq = (fqvhq*ra-fdvhd*xdmodd)*zsqi
              curr = oid*sina + oiq*cosa
              curi = oiq*sina - oid*cosa
              curn(1, i) = curr
              curn(2, i) = curi
              epqo = epqn
              epdo = epdn
C             -  Voltage behind X_s (call it V_r)
              epqn = vhq + ra*oiq + xdp*oid
              epdn = vhd + ra*oid - xdp*oiq
C             -  P_elec = V_r * conj (I_r)
              genp(i) = epdn*oid + epqn*oiq
              call ritecs(datat, iecs, 26)
            endif
          else
C           * * *
C           * * * TWO AXIS MACHINE   [2axgen]
C           * * *
            if (mgen .gt. 5) then
C             -  Machine has damper windings
              call redecs(datat, iecs, 39)
C             * * *
C             * * *  GET FREQ. DEP. WF = W/WO
C             * * *
              delw = 0.0
              if (mfdep .eq. 2 .or. mfdep .eq. 3) delw = angp2(i)
              wf = 1.0 + delw/(2.0*pi)
            endif
            vhr = eyr(i1)
            vhi = eyi(i1)
C           -  Calc V_t in d,q coords
            vhd = vhr*sina - vhi*cosa
            eyri(i) = vhd
            vhq = vhr*cosa + vhi*sina
            eyii(i) = vhq
C           -  Possible freq adjustment of impedance to volt source
            xdmodd = xdmod*wf
            xqmodd = xqmod*wf
            xc = 0.5*(xdmodd+xqmodd)
            zsqi = 1./(ra*ra+xdmodd*xqmodd)
C           -  Voltage source (either E'x or E"x)
            fqtemp = fq(i)
            fdtemp = fd(i)
C           -  delta-V from V_t to V_source in d,q coords
            fdvhd = fdtemp*wf - vhd
            fqvhq = fqtemp*wf - vhq
            if (idsw .eq. 7) then
              if (mgen .le. 5) then
                fdvhd = epdo - vhd
                fqvhq = epqo - vhq
              else
                fdvhd = edpdo*wf - vhd
                fqvhq = edpqo*wf - vhq
C               FQVHD = EDPQO*WF - VHQ
              endif
            endif
C           50 OID = (FQVHQ*XQMODD + FDVHD*RA)*ZSQI
C           -  Terminal current in D,Q & ref_bus coords
            oid = (fqvhq*xqmodd+fdvhd*ra)*zsqi
            oiq = (fqvhq*ra-fdvhd*xdmodd)*zsqi
            curr = oid*sina + oiq*cosa
            curi = oiq*sina - oid*cosa
            curn(1, i) = curr
            curn(2, i) = curi
            if (idsw .eq. 7) then
C             -  Processing + side of discontinuity
              call ritecs(datat, iecs, 27)
C             -  Calculate E'q at end of step
            elseif (mgen .le. 5) then
              epqo = vhq + ra*oiq + oid*((xdp+xp*satd)/(1.+satd))
              epdo = vhd + ra*oid - oiq*((xqp+xp*satq)/(1.+satq))
              epq(i) = epqo
              call ritecs(datat, iecs, 27)
            else
              edpqo = (vhq+ra*oiq)/wf + oid*((xdpp+xp*satd)/(1.+satd))
              edpdo = (vhd+ra*oid)/wf - oiq*((xqpp+xp*satq)/(1.+satq))
              taqp = edt*(1.+satq)/(2.*aqp+edt*(1.+satq))
              tadp = edt*(1.+satd)/(2.*adp+edt*(1.+satd))
              epqo = oid*((xdp-xdpp)/(1.+satd)) + (edpqo/tadp) - fqpp
              if (aq .ne. 0.0) epdo =  - oiq*((xqp-xqpp)/(1.+satq)) + 
     &         edpdo/taqp - fdpp
              epq(i) = edpqo
              call ritecs(datat, iecs, 39)
            endif
C           -  Calculate voltage behind Potier reactance.
            vpd = vhd + ra*oid - xp*oiq
            vpq = vhq + ra*oiq + xp*oid
C           -  P_elec = V_p * conj (I_t)
            genp(i) = vpd*oid + vpq*oiq
C           -  Jump if at + side of discontinuity
            if (idsw .eq. 7) then
              if (esat .ne. 0.0) then
C               -  Calc new machine saturation values
                vpm = sqrt(vpd**2+vpq**2)
                if (vpm .le. esat) then
                  satd = 0.0
                  satq = 0.0
                else
                  satd = (csat*(vpm-esat)*(vpm-esat))/vpm
                  satq = (xq/xd)*satd
                endif
              endif
            endif
            if (mgen .ge. 5) then
C             -  Vmagt[] stores term voltage for exciter (and PSS) input
              call ritecs(satd, iecs+25, 2)
              lvref = igndta(2, i)
              if (lvref .eq. 0) then
                vld = vhd + rt*oid - xt*oiq
                vlq = vhq + rt*oiq + xt*oid
                vmagt(i) = sqrt(vld*vld+vlq*vlq)
              else
                vmagt(i) = sqrt(eyr(lvref)*eyr(lvref)+eyi(lvref)*eyi
     &           (lvref))
              endif
            endif
          endif
        endif
      enddo
C     -  end of big loop for each machine

C     -  Debug if selected from DEBUG input card
      if (keybrd(28) .ne. 0) then
        write (outbuf, 10000)
10000   format ('0', 2(5x, 'IGENT'), 10x, 'GENP')
        call prtout(1)
        do jjj = 1, isg, 3
          kkk = min0(jjj+2, isg)
          write (outbuf, 10010) (igentn(1, i), igentn(2, i), genp(i), i 
     &     = jjj, kkk)
10010     format (2x, 3(2(3x, i11), f10.5))
          call prtout(1)
        enddo
        call skipln(1)
C       -
      endif
      return
      end
