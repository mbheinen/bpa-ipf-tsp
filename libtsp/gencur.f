C    %W% %G%
      subroutine gencur
C     
C     This subroutine calculates the current injection at
C     each machine. It is called by DERIV.
C     
      include 'tspinc/params.inc'
      include 'tspinc/iter.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/svs.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/param.inc'
      include 'tspinc/ecstbj.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/cnewt.inc'
      include 'tspinc/newton.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/spare1.inc'

C     -  Local variables

      logical debug
      character gname * 16

      data pi/3.1415927/

      do i = 1, isg

c       call genname (i, gname)                                        !dem
c       if (gname .eq. 'MOT MII  500.0 1') then
c         debug = .true.
c       else 
c         debug = .false.
c       endif

        wf = 1.0
        iecs = igentn(2, i)
        call redecs(datat, iecs, 27)
        mgen = igndta(1, i)
        i1 = igentn(1, i)
        eyr1 = eyr(i1)
        eyi1 = eyi(i1)
C       
C       STATIC VAR SOURCE
C       
        if (mgen .eq. 9) then
          ksv = igndta(2, i)
C         
C         ADDMITTANCE  OPTION
C         
          if (inewts .eq. 1 .or. mdeyoj .eq. 1) then
C         
C           INERTIALESS MACHINE
C         
            eyri(i) = bsvs(ksv)*eyi1
            eyii(i) =  - bsvs(ksv)*eyr1
C           
C           NEWTONS  OPTION
C           
          elseif (mdeyoj .eq. 2) then
            bnewt(2*i1) = bsvs(ksv)
            bnewt(2*i1-1) =  - bsvs(ksv)
            eyri(i) = 0.0
            eyii(i) = 0.0
          endif
        elseif (consm .eq. 0.0) then
          eyii(i) = 0.0
          eyri(i) = 0.0
          if (mgen .ge. 3) genp(i) = 0.0
C         
C         CLASSICAL MACHINE
C         
        elseif (mgen .ne. 1) then
C         -  For induction motor
          if (mgen .ne. 2) then
C           
C           TWO AXIS MACHINE  MGEN .GT. 5 IMPLIES DAMPER WINDINGS
C           
            if (mgen .gt. 5) then
              call redecs(datat, iecs, 39)
C             
C             TEST IF FREQ DEP AND GET WF = W/WO
C             
              delw = 0.0
              if (mfdep .eq. 2 .or. mfdep .eq. 3) delw = angp2(i)
              wf = 1.0 + delw/(2.0*pi)
            endif
            sina = sin(angl(i))
            cosa = cos(angl(i))
            if (iter .le. 1) then
              if (lppwr .eq. 0) then
                vhd = eyri(i)
                vhq = eyii(i)
               
                eyr1 = vhd*sina + vhq*cosa
                eyi1 = vhq*sina - vhd*cosa
C               
C               UPDATE FQ, FD AT A DISCONTINUITY
C               
                if (idsw .ne. 7) then
                  efdn = vfldtn(1, i)
C                 
C                 UPDATE FQ,FD FOR A NORMAL TIME STEP ADVANCE
C                 Two axis machine w/o dampers
C                 
                  if (mgen .lt. 6) then

C                 - XDMOD is impedance between q-axis source voltage and
C                 - Source voltage is either E'q or E'q.

                    xdmod = edt/(2.*ad+edt*(1.+satd))
                    xqmod = edt/(2.*aq+edt*(1.+satq))
                    fq(i) = epqo - xdmod*(2.*(1.+satd)*epqo-efdo-efdn+
     &               (xd-xdp)*oid)
                    fd(i) = epdo 
     &                    - xqmod*(2.*(1.+satq)*epdo-(xq-xqp)*oiq)
                    xqmod = xqmod*(xq-xqp) + (xqp+xp*satq)/(1.+satq)
                    xdmod = xdmod*(xd-xdp) + (xdp+xp*satd)/(1.+satd)
                  else
C                   
C                   Two axis machine with dampers

                    if (aq .eq. 0.0) then
                      taq = 0.0
                    else
                      taq = edt*(1.+satq)/(2.*aq)
                    endif
                    if (ad .eq. 0.0) then
                      tad = 0.0
                    else
                      tad = edt*(1.+satd)/(2.*ad)
                    endif
                    tadp = edt*(1.+satd)/(2.*adp+edt*(1.+satd))
                    taqp = edt*(1.+satq)/(2.*aqp+edt*(1.+satq))
                    adxdr = tad/(tad*xdr-1.)
                    aqxqr = taq/(taq*xqr-1.)
                    aqppi = 1./(1./tadp-adxdr*(xdr+1.))
                    adppi = 1./(1./taqp-aqxqr*(xqr+1.))
                    fqp = adxdr*((1./(1.+satd))*(efdo+efdn)
     &                  - edpqo*(xdr+1.))
     &                  + (epqo/(tad*xdr-1.)) * (tad*xdr+1.)
                    fqpp = edpqo*(1./tadp-2.) + epqo 
     &                   - ((xdp-xdpp)/(1.+satd))*oid
                    fq(i) = (fqpp-fqp)*aqppi
                    fdp = 0.0
                    if (aq .ne. 0.0) fdp =  - aqxqr*(edpdo*(xqr+1.)) 
     &                 + (epdo/(taq*xqr-1.))*(xqr*taq+1.)
                    fdpp = edpdo*(1./taqp-2.) 
     &                   + epdo + ((xqp-xqpp)/(1.+satq))*oiq
                    fd(i) = (fdpp-fdp)*adppi
                    xdmod = (xdpp-xp)/(1.+satd) + xp 
     &                    + (aqppi/(1.+satd))*(xdp-xdpp)
                    xqmod = (xqpp-xp)/(1.+satq) + xp 
     &                    + (adppi/(1.+satq))*(xqp-xqpp)
                  endif
                elseif (mgen .le. 5) then

C                 -  2-axis w/o dampers

                  xdmod = (xdp+xp*satd)/(1.+satd)
                  xqmod = (xqp+xp*satq)/(1.+satq)
                else

C                 -  2-axis w/o dampers

                  xdmod = (xdpp-xp)/(1.+satd) + xp
                  xqmod = (xqpp-xp)/(1.+satq) + xp
                endif
              endif
            endif

C           -  end of 2-axis with damper calcs

            vhr = eyr1
            vhi = eyi1
            vhd = vhr*sina - vhi*cosa
            vhq = vhr*cosa + vhi*sina
C           
C           ADJUST FREQ DEP PARAM
C           
            xdmodd = xdmod*wf
            xqmodd = xqmod*wf
            zsq = ra*ra + xdmodd*xqmodd
            fqvhq = wf*fq(i) - vhq
            fdvhd = wf*fd(i) - vhd
            if (idsw .eq. 7) then
              if (mgen .le. 5) then
                fqvhq = epqo - vhq
                fdvhd = epdo - vhd
              else
                fqvhq = edpqo*wf - vhq
                fdvhd = edpdo*wf - vhd
              endif
            endif
C           
C           REVERT TO USUAL EQUATION AT DISCONTINUITY
C           
            curd = (fqvhq*xqmodd+fdvhd*ra)/zsq
            curq = (fqvhq*ra-fdvhd*xdmodd)/zsq
            vpd = vhd + ra*curd - xp*curq
            vpq = vhq + ra*curq + xp*curd
            if (mgen .le. 5) then
              zsqi = 1./(ra*ra+xqp*xdp)
              xc = 0.5*(xdp+xqp)
            else
              zsqi = 1./(ra*ra+xqpp*xdpp)
              xc = 0.5*(xdpp+xqpp)
            endif
          elseif (datat(26) .eq. -2.0) then
            curr = 0.0
            curi = 0.0
            cdel = 0.0
            goto 100
          else
            sina = sin(xdxdp)
            cosa = cos(xdxdp)
            slip = angp2(i)
            adp = edt/(2.*ad+edt)
            addif = 0.5*(1.-adp)*edt
            xqmod = xdp + adp*(xd-xdp)
            xdmod = xqmod
            if (iter .le. 1) then
              if (lppwr .eq. 0) then
                vhd = eyri(i)
                vhq = eyii(i)
                eyr1 = vhd*sina + vhq*cosa
                eyi1 = vhq*sina - vhd*cosa
C               
C               *  - Update FQ, FD for induction motor
C               
C               - Note: For induction motor XQ=XD and XQP=XDP so the lat
C               are used for all instances of the former
 
                if (idsw .eq. 7) then
                  fq(i) = epqn + adp*(xd-xdp)*oid
                  fd(i) = epdn - adp*(xd-xdp)*oiq
                else
                  fq(i) = epqn - adp*(2.*epqn+(xd-xdp)*oid) 
     &                  + 2.*addif*slip*epdn
                  fd(i) = epdn - adp*(2.*epdn-(xd-xdp)*oiq) 
     &                  - 2.*addif*slip*epqn
                endif
              endif
            endif
            vhr = eyr1
            vhi = eyi1
            vhd = vhr*sina - vhi*cosa
            vhq = vhr*cosa + vhi*sina
C           
C           ADJUST FREQ DEP PARAM
C           
            xdmodd = xdmod*wf
            xqmodd = xqmod*wf
            zsq = ra*ra + xdmodd*xqmodd
            if (lppwr .ne. 0) then
              fq(i) = fq(i) + addif*(slip*epdn-slip0*epdo)
              fd(i) = fd(i) - addif*(slip*epqn-slip0*epqo)
            endif
            fqvhq = wf*fq(i) - vhq
            fdvhd = wf*fd(i) - vhd
            curd = (fqvhq*xqmodd+fdvhd*ra)/zsq
            curq = (fqvhq*ra-fdvhd*xdmodd)/zsq
            zsqi = 1./(ra*ra+xdp*xdp)
            xc = xdp
          endif
C         
C         TEST IF THIS ITERATION FOR YISOLN OR NEWTON ITER
C         
          if (inewts .eq. 1 .or. mdeyoj .eq. 1) then
C           
C           YISOLN
C           
            curr = (vhr*ra+vhi*xc)*zsqi + curd*sina + curq*cosa
            curi = (vhi*ra-vhr*xc)*zsqi + curq*sina - curd*cosa
            cdel = abs(curd-oid) + abs(curq-oiq)
            oid = curd
            oiq = curq
          elseif (mdeyoj .ne. 1) then
C           
C           NEWTON
C           
            zsqi = 1.0/(ra*ra+xdmod*xqmod)
            curr = zsqi*((cosa*ra+sina*xqmod)*fq(i)
     &                  +(sina*ra-cosa*xdmod)*fd(i))
            curi = zsqi*((sina*ra-cosa*xqmod)*fq(i)
     &                  -(cosa*ra+sina*xdmod)*fd(i))
C           
C           MATRIX DIAG. MODIFICATION DUE TO TRANSIENT SALIENCY
C           
            i1 = igentn(1, i)
            gnewt(2*i1-1) = gnewt(2*i1-1) 
     &                    + zsqi*(ra+sina*cosa*(xqmod-xdmod))
            gnewt(2*i1) = gnewt(2*i1) 
     &                  + zsqi*(ra-sina*cosa*(xqmod-xdmod))
            bnewt(2*i1-1) = bnewt(2*i1-1) 
     &                    + zsqi*(sina*sina*xqmod+cosa*cosa*xdmod)
            bnewt(2*i1) = bnewt(2*i1) 
     &                  + zsqi*(-cosa*cosa*xqmod-sina*sina*xdmod)
            cdel = abs(curr-curro(i)) + abs(curi-curio(i))
            curro(i) = curr
            curio(i) = curi
          endif
  100     eyri(i) = curr
          eyii(i) = curi
          if (keybrd(16) .ne. 0) then
            write (outbuf, 10000) i, imax, mfdep
            call prtout(1)
            write (outbuf, 10010) cmax, pi, delw, wf, xdmod
            call prtout(1)
            write (outbuf, 10010) xdmodd, xqmod, xqmodd, zsq, fqvhq
            call prtout(1)
            write (outbuf, 10010) fdvhd, curd, curq, curr, curi
            call prtout(1)
            write (outbuf, 10010) oid, oiq
            call prtout(1)
10000       format ('  FREQ DEP. DBUG', 3i5)
10010       format (1x, 5e16.8)
          endif
          if (cmax .le. cdel) then
 
            imax = i + nmx
            cmax = cdel
          endif
C         
C         INDUCTION MACHINE UPDATE FQ, FD AT A DISCONTINUITY
C         
          if (mgen .eq. 2) then
C         -  for classical machine
            if (idsw .eq. 7) then
              fq(i) = epqn + adp*(xd-xdp)*curd
              fd(i) = epdn - adp*(xd-xdp)*curq
            endif
            call ritecs(datat, iecs, 22)
          else
            if (idsw .eq. 7) then
              if (mgen .le. 5) then
                fq(i) = epqo
                fd(i) = epdo
              else
                fq(i) = edpqo
                fd(i) = edpdo
              endif
            endif
            if (mgen .le. 5) call ritecs(datat, iecs, 27)
            if (mgen .gt. 5) call ritecs(datat, iecs, 39)
          endif
        elseif (iter .le. 1) then
          sina = sin(angl(i))
          cosa = cos(angl(i))
          vmag = fd(i)
          curr = vmag/xdp
          eyri(i) = curr*sina
          eyii(i) =  - curr*cosa
        endif
      enddo
      if (keybrd(19) .ne. 0) then
        write (outbuf, 10020)
        call prtout(1)
10020   format ('0igen', 5x, 'eyri', 11x, 'eyii')
        do i = 1, isg
          jgen = igentn(1, i)
          write (outbuf, 10030) jgen, eyri(i), eyii(i)
        enddo
10030   format (1x, i4, 2f15.6)
        call prtout(1)
      endif
      return
      end
