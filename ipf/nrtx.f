C    @(#)nrtx.f	20.9 10/13/99
      subroutine nrtx

C     This subroutine checks LTC's with PV nodes on either terminal.
C     The LTC may change it's type temporarily to RQ if that will
C     improve the reactive limits on the terminal buses.

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/trmdbg.inc'
      include 'ipfinc/xdata.inc'

      integer status, switchbx, tbxtr_status(MAXLTC)
      logical tbxtr_init

      data tbxtr_init /.false./

      save 

C     Process tandem busses adjoined with an LTC transformer

      if (.not. tbxtr_init) then
        do j = 1, ntbxtr
          tbxtr_status(j) = 0
        enddo
        tbxtr_init = .true.
      endif

      if (msw .le. 1) then
        do j = 1, ntbxtr
          if (tbxtr_status(j) .gt. 0) go to 100
          jt = tbxtr(1, j)
          kt = ltran(1, jt)
          mt = ltran(9, jt)
          if (ltran(2, jt) .eq. -1) then
            kc = kt
          else if (ltran(2, jt) .eq. -2) then
            kc = mt
          else
            kc = ltran(2,jt)
          endif
          if (kc .gt. 0) then
            kc_kvolt = kvolt(kc)
          else
            kc_kvolt = 0
          endif
          jk = tbxtr(2, j)
          jm = tbxtr(3, j)
          do i = 4, 14
            tbxtr(i, j) = 0
          enddo
          ityp = mod(ltran(10, jt), 100)
          if (ityp .ne. 1) goto 100
          lt = ltran(10, jt) / 100
          ksw =  - 1

C         Compute Tx flow Qkm

          ek = e(kt)
          fk = f(kt)
          em = e(mt)
          fm = f(mt)
          la1 = ltran(3, jt)
          gkm = gkmu(la1)
          bkm = bkmu(la1)

C         Compute 2-port admittances.

          gkktx =  - gkm/tap(jt)
          bkktx =  - bkm/tap(jt)

C         Compute currents, injections

          aim = em*gkm - fm*bkm
          bim = em*bkm + fm*gkm
          rh =  - ek*bim + fk*aim
          rj =  - ek*aim - fk*bim
          aim = aim + ek*gkktx - fk*bkktx
          bim = bim + ek*bkktx + fk*gkktx
          qkm =  - ek*bim + fk*aim

C         Compute Q at terminal KT.

          call nrpqv(kt, pk, dpk, qk, dqk, vk)
          if (abs(dpk) .gt. option(7)) goto 100
          if (jk .gt. 0) then
            ltyp = tbx(1, jk)
            qmink = tbx(4, jk)
            qmaxk = tbx(3, jk)
            if (ltyp .eq. 5) then
              nt = tbx(5, jk)
              userek = xdata(5, nt)
              usecap = xdata(6, nt)
              qmink = qmink - amax1(0.0, vk**2*usecap/bmva)
              qmaxk = qmaxk - amin1(0.0, vk**2*userek/bmva)
            endif
            dq1 = dim(qk, qmaxk) - dim(qmink, qk)
          else
            qmaxk = 1.0e8
            qmink =  - 1.0e8
            dq1 = 0.0
          endif
          tbxtr(4, j) = dq1

C         Compute Q at terminal MT.

          call nrpqv(mt, pm, dpm, qm, dqm, vm)
          if (abs(dpm) .gt. option(7)) goto 100
          if (jm .gt. 0) then
            ltyp = tbx(1, jm)
            qminm = tbx(4, jm)
            qmaxm = tbx(3, jm)
            if (ltyp .eq. 5) then
              nt = tbx(5, jm)
              userek = xdata(5, nt)
              usecap = xdata(6, nt)
              qminm = qminm - amax1(0.0, vm**2*usecap/bmva)
              qmaxm = qmaxm - amin1(0.0, vm**2*userek/bmva)
            endif
            dq2 = dim(qm, qmaxm) - dim(qminm, qm)
          else
            qmaxm = 1.0e8
            qminm =  - 1.0e8
            dq2 = 0.0
          endif
          tbxtr(5, j) = dq2

          if (dq1*dq2 .gt. 0.0) goto 100
          if ((kvolt(kt) .ne. 0 .or. kvolt(mt) .ne. 0) .and.
     &         kc_kvolt .ne. 0) then

C           Level 1 quantification: Compute delta_q to alleviate
C           Q_violations.

            ksw = 0
            if (amax1(abs(dq1), abs(dq2)) .gt. option(7)) then
              if (abs(dq1) .gt. abs(dq2)) then
                if (dq1 .gt. 0) then
                  dq =  - amin1(dq1, dim(qmaxm, qm))
                else
                  dq =  - amax1(dq1, -dim(qm, qminm))
                endif
                ksw = 1
              elseif (dq2 .gt. 0) then
                dq = amin1(dq2, dim(qmaxk, qk))
                ksw = 2
              else
                dq = amax1(dq2, -dim(qk, qmink))
                ksw = 2
              endif
              if (abs(dq) .ge. option(7)) then
                dx =  - 0.01*dq

C               Determine if LTC control will be implemented

                if (ksw .eq. 1) then
                  call ltcctl(jt, 8, 3, 0, qkm, dq)
                  ltran(10, jt) = 100*lt + 8
                else
                  call ltcctl(jt, 9, 4, 0, qkm, dq)
                  ltran(10, jt) = 100*lt + 9
                endif
                tran(4, jt) = dq 
                tran(5, jt) = dx
                ksw = jacltc(jt, 1)

C               KSW assignments from JACLTC:

C               2 -- normal - LTC control of VARS
C               0 -- manual control of LTC adjustment (no control if
C                    DPT(1,JT) is zero).

                if ((ksw .eq. 2) .or. 
     &              (abs(dpt(1, jt)) .gt. 0.25*abs(dx))) then

C                 This duplicate "redefination" is necessary because
C                 JACLTC processes type "5" LTC's as temporary input
C                 and immediately resets it to type "1" within the
C                 routine.

                  if (ksw .eq. 1) then
                    ltran(10, jt) = 100*lt + 8
                  else
                    ltran(10, jt) = 100*lt + 9
                  endif
                  tran(4, jt) = dq 
                  tran(5, jt) = dx
                  tbxtr(6, j) = dq
                  tbxtr_status(j) = tbxtr_status(j) + 1
                  if (jk .gt. 0) then
                    if (abs(dq) .gt. 0.50 .or. 
     &                  abs(dq) .gt. 0.33*abs(dq1)) then

C                     Disable for one iteration the bus type switching 

c                     for this bus.

                      tbx(1, jk) =  -iabs (ifix (sngl(tbx(1, jk))))
                      dqtot = dqtot + abs(dq1)
                    endif
                  endif
                  if (jm .gt. 0) then
                    if (abs(dq) .gt. 0.50 .or. 
     &                  abs(dq) .gt. 0.33*abs(dq2)) then

C                     Disable for one iteration the bus type switching 

c                     for this bus.

                      tbx(1, jm) =  - iabs (ifix (sngl (tbx(1, jm))))
                      dqtot = dqtot + abs(dq2)
                    endif
                  endif
                endif
                if (idswb .ne. 0) then
                  write (dbug, 10000) jt, kt, mt, jk, jm, kvolt(kt), 
     &             kvolt(mt), ksw, qkm, dq, dq1, dq2, dx, tap(jt), 
     &             tran(7, jt), tran(8, jt)
10000             format (' TX-TBX level 1: ', i3, 2i5, 2i4, 2i5, i2, 
     &             4e11.3, 4f9.4)
                endif
                if (iterm .ne. 0) write (*, 10000) jt, kt, mt, jk, jm, 

     &           kvolt(kt), kvolt(mt), ksw, qkm, dq, dq1, dq2, dx, 
     &           tap(jt), tran(7, jt), tran(8, jt)
              endif
              goto 100
            endif
          endif

C         Level 2 quantification: Compute delta_t to alleviate
C         V_violations.

          dv1 = dim(vk,vlimx(kt)) - dim(vlimn(kt),vk)
          dv2 = dim(vm,vlimx(mt)) - dim(vlimn(mt),vm)
          if (vlimx(kc) .gt. vlimn(kc) .and. dv1*dv2 .le. 0.0) then
            if (amax1(abs(dv1), abs(dv2)) .gt. 0.0010) then
              if (abs(dv1) .gt. abs(dv2)) then
                if (dv1 .gt. 0.0) then
                  dx = amin1( dv1, (vlimx(mt)-vm)*vk/vm)
                else
                  dx = amax1( dv1, (vlimn(mt)-vm)*vk/vm)
                endif
                tran(5, jt) = -dx
                ltran(10, jt) = 100*lt + 6
              else  
                if (dv2 .gt. 0) then
                  dx = amin1( dv2, (vlimx(kt)-vk)*vm/vk)
                else
                  dx = amax1(dv2, (vlimn(kt)-vk)*vm/vk)
                endif
                tran(5, jt) = -dx
                ltran(10, jt) = 100*lt + 7
              endif
              tran(4, jt) = 0.0
              tbxtr(7, j) = dx
              tbxtr_status(j) = tbxtr_status(j) + 1

C             Disable for one iteration the bus type switching for
C             this bus.

              if (jk .gt. 0) tbx(1,jk) =  -iabs(ifix(sngl(tbx(1,jk))))
              if (jm .gt. 0) tbx(1,jm) =  -iabs(ifix(sngl(tbx(1,jm))))

              if (idswb .ne. 0) then
                write (dbug, 10010) jt, kt, mt, ksw, dv1, dv2, dx, vk, 

     &           vm, tap(jt), tran(7, jt), tran(8, jt)
10010           format (' TX-TBX level 2: ', i3, 2i5, i2, 15x, 8f10.5)
              endif
              if (iterm .ne. 0) write (*, 10010) jt, kt, mt, ksw, dv1, 

     &         dv2, dx, vk, vm, tap(jt), tran(7, jt), tran(8, jt)
            else

C             Compute circulating vars.

              if (qkm .gt. 0.0) then
                qc = amin1(dim(qk, qmink), qkm, dim(qmaxm, qm))
              elseif (qkm .lt. 0.0) then
                qc = amax1(-dim(qmaxk, qk), qkm, -dim(qk, qminm))
              else
                qc = 0.0
              endif
              tbxtr(8, j) = qc
c
c             Special logic for type BX buses
c
              bold = -9999.0
              bnew = -9999.0
              if (kc .eq. kt .and. jk .gt. 0 .and. 
     &            abs(qc) .gt. 0.10) then
                ltyp = tbx(1,jk)
                ityp = tbx(7,jk)
                if (ltyp .eq. 5) then
                  nt = tbx(5,jk)
                  if (ityp .eq. 1) then
C       
C                   BX bus in state PV -- go to nearest discrete state.
C       
                    dvdq = 1.0 / xsen(nt)
                    call getxct (jk, kt, kc, nt, vk, qk, ityp, dvdq)   
                    ityp = 4   
                    kvolt(kt) = 0  
                    tbx(7,jk) = ityp
                  endif
                  status = switchbx (jk, nt, kt, vk, qc, bold, bnew)
                endif
              else if (kc .eq. mt .and. jm .gt. 0 .and.
     &            abs(qc) .gt. 0.10) then
                ltyp = tbx(1,jm)
                ityp = tbx(7,jm)
                if (ltyp .eq. 5) then
                  nt = tbx(5,jm)
                  if (ityp .eq. 1) then
C       
C                   BX bus in state PV -- go to nearest discrete state.
C       
                    dvdq = 1.0 / xsen(nt)
                    call getxct (jm, mt, mt, nt, vm, qm, ityp, dvdq)   
                    ityp = 4   
                    kvolt(mt) = 0  
                    tbx(7,jm) = ityp
                  endif
                  status = switchbx (jm, nt, mt, vm, -qc, bold, bnew)
                endif
              endif
              tbxtr_status(j) = tbxtr_status(j) + 1
              if (idswb .ne. 0) then
                write (dbug, 10020) jt, kt, mt, qc, qkm, bold, bnew
10020           format (' TX-TBX level 3: ', i3, 2i5, 22x, 2e11.3, 
     &           2f10.1)
              endif
              if (iterm .ne. 0) write (*, 10020) jt, kt, mt, qc, qkm, 
     &         bold, bnew

            endif
          endif
  100     continue
        enddo
      endif
      return
      end
