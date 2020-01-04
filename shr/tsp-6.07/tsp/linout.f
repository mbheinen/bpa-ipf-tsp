C    %W% %G%
      subroutine linout
C     
C     THIS SUBROUTINE FORMS FINAL TABLES FOR REQUESTED LINE OUTPUT
C     DATA AND WRITES THE TABLES TO THE OUTPUT FILE (FOR006).
C     IT IS CALLED BY NOUT2.  IT CALLS RDOT.
C     -
C     Revisions:
C     Oct/05/92 - DEM: For alternate auxilary file format, expanded
C     time to 6 decimal places & parameters to 15
C     characters.
C     -
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/dc.inc'
      include 'tspinc/worst.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/mvn.inc'
      include 'tspinc/prtmax.inc'
      include 'tspinc/geno.inc'
      include 'tspinc/indx2n.inc'
      include 'tspinc/linmod.inc'
      include 'tspinc/lindat.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/kbusno.inc'
      include 'tspinc/rdcal.inc'
      include 'tspinc/room.inc'
      include 'tspinc/fltim.inc'
      include 'tspinc/linanl.inc'
      include 'tspinc/vymn.inc'
 
      common /tcscdata/ tcscdata(MAXSTP,2,VYMNMAX)

      equivalence (nolin1, noline(1)), (nolin2, noline(2)), 
     &            (nozapp, noline(3)), (nolin4, noline(4)), 
     &            (nolin5, noline(5)), (nolin6, noline(6)),
     &            (nolin7, noline(7)), (nolin8, noline(8))
      dimension mvott(8), mvadt(8)
      dimension msub13(2)
      equivalence (ksub(9), ksb12b), (msub(25), msub13)
      logical plotop, prntop, auxop, anlop
      character*8 bus1, bus2, k3, ipar
      character*15 atem, atem1
      character lname*40, crvend*10

C     In-line functions

      plotop(iop) = iop .eq. 2 .or. iop .eq. 3 .or. iop .eq. 6 .or. 
     &              iop .eq. 7 
      prntop(iop) = iop .eq. 1 .or. iop .eq. 3 .or. iop .eq. 5 .or. 
     &              iop .eq. 7 
      auxop(iop) = iop .ge. 4
      anlop(iop) = iop .gt. 0 .and. iop .le. 8
 
C     Begin     begin     begin     begin     begin     begin

      call mpost('LINOUT')
      if (auxfmt .eq. 'STD') then
        crvend = '  -100.00'
      else
        crvend = '/END'
      endif
 
C     CALCULATE AND OUTPUT LINE QUANTITIES
C     
C     THE ARRAY THAT WOULD HAVE BEEN WRITTEN  AT KBUSID 
C     (IN SUBROUTINE BUSOUT) IS NOW CALLED KBUSNO.                          

      ktnol1 = 0
      ktnol2 = 0
      ktzpp = 0
      ktnol4 = 0
      ktnol5 = 0
      ktnol6 = 0
      ktnol7 = 0
      ktnol8 = 0
      if (ildatt .ne. 0) then
        ksb13a = kwork
        ksb13b = kwork + noline(1)
        ksb13d = ksb13b + noline(2)
        ksb13e = ksb13d + noline(3)
        ksb13f = ksb13e + noline(4)
        ksb13g = ksb13f + noline(5)
        ksb13h = ksb13g + noline(6)
        ksb13i = ksb13h + noline(7)
        kwork = ksb13i + noline(8)
        ilinal = 0
        linekt = ildatt
        kount = 50
        istart =  - 49
        istop = 0
        iecs = ladecs
        if (imod .ne. 0) then
          do ii = 1, imod
            k1 = lnmodn(1, ii)
            k2 = lnmodn(2, ii)
            k3 = lnmodc(ii)
            k1 = indx2n(k1)
            k2 = indx2n(k2)
            lnmodn(1, ii) = k1
            lnmodn(2, ii) = k2
          enddo
        endif
        do while (.true.)
          if (linekt .le. 50) kount = linekt

C         NOTE THAT IECS HAS BEEN SET EQUAL TO LADECS 
C         EARLIER IN THE PROGRAM.  THE ARRAY THAT WAS 
C         WRITTEN AT LADECS WAS PACKED.  THE NEW      
C         ARRAYS ARE LNDATN AND LNDATC.             

          iecs = iecs + 9*kount
          linekt = linekt - kount
          istart = istart + 50
          istop = istop + 50
          if (linekt .lt. 50) istop = ildatt
          do i = istart, istop

C           NOTE: LINDAT(1,I) HAS NOT BEEN UNPACKED YET 

            if (lndatn(1, 1, i) .ne. 0) then
              imodsw = 0
              if (lndatn(1, 1, i) .le. 0) then
                imodsw =  - lndatn(1, 1, i)
                lndatn(1, 1, i) = lnmodn(1, imodsw)
                lndatn(1, 2, i) = lnmodn(2, imodsw)
                r = brnmod(4, imodsw)
                x = brnmod(5, imodsw)
                bndatn(3, 1, i) = r/(r*r+x*x)
                bndatn(4, 1, i) =  - x/(r*r+x*x)
                bndatn(5, 1, i) = brnmod(2, imodsw)
                bndatn(6, 1, i) = brnmod(3, imodsw)
                bndatn(7, 1, i) = 1./(r*r+x*x)
              endif
              ibus1 = lndatn(1, 1, i)
              ibus2 = lndatn(1, 2, i)
              ipar = lndatc(i)

C             THE PACKED ARRAY NEWTAB WAS WRITTEN AT  
C             KNEWT IN OUTPUT1.  THE NEW ARRAYS ARE   
C             NEWTBC AND INWTB.                     

              bus1 = newtbc(ibus1)
              bus2 = newtbc(ibus2)
              ibase1 = inwtb(ibus1)
              ibase2 = inwtb(ibus2)
              base1 = basekv(ibase1)
              base2 = basekv(ibase2)
C             
C             INITIALIZE TABLES FOR LINE QUANTITY SUMMARY
C             
              if (anlop(lndatn(2, 3, i)) .or. 
     &            anlop(lndatn(2, 5, i)) .or. 
     &            anlop(lndatn(3, 3, i)) .or. 
     &            anlop(lndatn(3, 7, i)) .or.
     &            anlop(lndatn(4, 4, i)) .or.
     &            anlop(lndatn(4, 5, i)) .or.
     &            anlop(lndatn(4, 6, i)) .or.
     &            anlop(lndatn(4, 7, i))) then
                ilinal = ilinal + 1
                anlnm1(ilinal) = bus1
                anlkv1(ilinal) = base1
                anlnm2(ilinal) = bus2
                anlkv2(ilinal) = base2
                anlpar(ilinal) = ipar
                do jtrr = 1, 7
                  anlmax(jtrr, ilinal) = 0.0
                  anlmin(jtrr, ilinal) = 0.0
                  anltin(jtrr, ilinal) = 0.0
                  anltax(jtrr, ilinal) = 0.0
                enddo
              endif
              if (prntop(lndatn(2, 3, i)) .or. 
     &            prntop(lndatn(2, 5, i)) .or. 
     &            prntop(lndatn(2, 7, i)) .or. 
     &            prntop(lndatn(3, 3, i)) .or. 
     &            prntop(lndatn(3, 5, i)) .or. 
     &            prntop(lndatn(3, 7, i)) .or.
     &            prntop(lndatn(4, 4, i)) .or.
     &            prntop(lndatn(4, 5, i)) .or.
     &            prntop(lndatn(4, 6, i)) .or.
     &            prntop(lndatn(4, 7, i))) then
                write (outbuf, 10000) bus1, base1, bus2, base2, ipar
                call prtout(1)
10000           format ('0', 5x, 'OUTPUT LISTING FOR LINE ', a8, f7.1, 
     &            2x, a8, f7.1, 1x, a2)
              endif
              nqty = lndatn(2, 3, i)/4 
     &             + lndatn(2, 5, i)/4 
     &             + lndatn(2, 7, i)/4 
     &             + lndatn(3, 3, i)/4 
     &             + lndatn(3, 5, i)/4 
     &             + lndatn(3, 7, i)/4
     &             + lndatn(4, 4, i)/4
     &             + lndatn(4, 5, i)/4
     &             + lndatn(4, 6, i)/4
     &             + lndatn(4, 7, i)/4
 
              write (lname, 10010) bus1, base1, bus2, base2, ipar, nqty
10010         format (a8, f5.1, 1x, a8, f5.1, 1x, a1, i2)
 
              if (nqty .gt. 0 .and. auxfmt .eq. 'STD') 
     &          write (l11, '(2a)') 'L  ', lname
C             
C             INITIALIZE ARRAYS FOR MAX AND MIN PRINT DAT
C             
              do itrr = 1, 18
                prtmax(itrr) =  - 10000.
                prtmin(itrr) = 10000.
              enddo
              iadr1 = kbusno(ibus1)
              iadr2 = kbusno(ibus2)
              ib1 = 2802
              ib2 = ib1 + icnt2 + 1
              call redecp(msub13(2), ksub13+iadr1-1, 1)
              call readmp (13, worksp(ib1+1), icnt2, 1)
              call redecp(msub13(2), ksub13+iadr2-1, 1)
              call readmp (13, worksp(ib2+1), icnt2, 1)
              ind = ib2 + icnt2 + 1
C             
C             CHECK IF ANY LINE QUANTITES WERE REQUESTED ON MV CAR
C             
              mvn = 0
              if (mvlkt .ne. 0) then
C               MVN = 0
                do l = 1, 2*mvkt
                  if (mvcde(l) .eq. 'L') then
                    if (mvgen(l) .eq. i) then
                      mvn = mvn + 1
                      mvott(mvn) = mvopt(l)
                      mvadt(mvn) = l
                    endif
                  endif
                enddo
              endif
              iopt1 = lndatn(2, 3, i)
              iopt2 = lndatn(2, 5, i)
              iopt3 = lndatn(2, 7, i)
              iopt4 = lndatn(3, 3, i)
              iopt5 = lndatn(3, 5, i)
              iopt6 = lndatn(3, 7, i)
              iopt7 = lndatn(4, 5, i)
              iopt8 = lndatn(4, 7, i)
              if (iopt1 .ne. 0) then
                lin1 = ind
                ind = ind + icount + 3
                lin1a = lin1 + icount + 1
                worksp(lin1a) = 0.0
                worksp(lin1a+1) = 0.0
              endif
              if (iopt2 .ne. 0) then
                lin2 = ind
                ind = lin2 + icount + 3
                lin2a = lin2 + icount + 1
                worksp(lin2a) = 0.0
                worksp(lin2a+1) = 0.0
              endif
              if (iopt4 .ne. 0) then
                lin4 = ind
                ind = lin4 + icount + 3
                lin4a = lin4 + icount + 1
                worksp(lin4a) = 0.0
                worksp(lin4a+1) = 0.0
              endif
              if (iopt5 .ne. 0) then
                lin5 = ind
                ind = lin5 + icnt2 + 1
              endif
              if (iopt6 .ne. 0) then
                lin6 = ind
                ind = lin6 + icnt2 + 1
              endif
              if (iopt7 .ne. 0) then
                lin7 = ind
                ind = lin7 + icount + 3
                lin7a = lin7 + icount + 1
                worksp(lin7a) = 0.0
                worksp(lin7a+1) = 0.0
              endif
              if (iopt8 .ne. 0) then
                lin8 = ind
                ind = lin7 + icount + 3
                lin8a = lin8 + icount + 1
                worksp(lin8a) = 0.0
                worksp(lin8a+1) = 0.0
              endif
              if (iopt3 .ne. 0) iz = ind
              gij = bndatn(3, 1, i)
              bij = bndatn(4, 1, i)
              gio = bndatn(5, 1, i)
              bio = bndatn(6, 1, i)
              gt = gio + gij
              bt = bio + bij
              imod2 = 0
              imodsw = 0
              if (imod .ne. 0) then
                do ind = 1, imod
                  if (lndatn(1, 1, i) .eq. lnmodn(1, ind)) then
                    if (lndatn(1, 2, i) .eq. lnmodn(2, ind)) then
                      if (lndatc(i) .eq. lnmodc(ind)) then
                        if (imodsw .eq. 0) imodsw = ind
                        imod2 = ind
                      endif
                    endif
                  endif
                enddo
                if (imodsw .ne. 0) timsw = brnmod(6, imodsw)
              endif
 
C             CALCULATE LINE QUANTITIES FOR EACH TIME STEP T(1)-T(ICOUNT
 
              do j = 1, icount
                ind = 2*j - 1
                ind1 = ind + 1
                if (j .ne. 1) then
                  if (imodsw .ne. 0) then
                    if (.not. ((t(j-1) .ne. timsw) .or. 
     &                         (t(j) .ne. timsw))) then
                      r = brnmod(4, imodsw)
                      x = brnmod(5, imodsw)
                      gij = r/(r*r+x*x)
                      bij =  - x/(r*r+x*x)
                      gio = brnmod(2, imodsw)
                      bio = brnmod(3, imodsw)
                      gt = gio + gij
                      bt = bio + bij
                      imodsw = imodsw + 1
                      timsw = brnmod(6, imodsw)
                      if (imodsw .gt. imod2) imodsw = 0
                      write (outbuf, 10020)
                      call prtout(1)
                      write (outbuf, 10030) gij, bij, gio, bio, t(j)
                      call prtout(1)
10020                 format ('0', 5x, ' GIJ,BIJ,GIO,BIO,TIME')
10030                 format (5x, 5f12.3)
                    endif
                  endif
                endif
                ei = worksp(ib1+ind)
                fi = worksp(ib1+ind1)
                ej = worksp(ib2+ind)
                fj = worksp(ib2+ind1)
                c1r = ei*gt - fi*bt
                c1i = ei*bt + fi*gt
                c2r = ej*gij - fj*bij
                c2i = ej*bij + fj*gij
                izer = 1
                ctr = c1r - c2r
                cti = c1i - c2i
 
C               IZER=0 IMPLIES THE LINE IS OPEN AT THIS TIME STEP
 
                if ((abs(ctr) .le. .001) .and. (abs(cti) .le. .001))
     &           izer = 0
                zbase = (base1*base1)/bmva
                ctbase = (bmva*1000.)/(base1*1.73205)
                if (iopt1 .ne. 0) then
 
C                 CALCULATE MW'S IF REQUESTED
 
                  worksp(lin1+j) = (ei*ctr+fi*cti)*bmva
                  if (worksp(lin1+j) .gt. worksp(lin1a)) 
     &              worksp(lin1a) = worksp(lin1+j)
                  if (worksp(lin1+j) .lt. worksp(lin1a+1)) 
     &              worksp(lin1a+1) = worksp(lin1+j)
C                 
C                 DO NOT SAVE MAX AND MINS IF A FAULT IS APPLIED A
C                 
                  if (ifltkt .ne. 0) then
                    to = t(j)
                    if (j .gt. 1) then
                      tlast = t(j-1)
                    else
                      tlast =  - 1.
                    endif
                    do l = 1, ifltkt
                      if (to .eq. fstrt(l) .and. to .eq. tlast) 
     &                 goto 100
                      if (to .eq. fstop(l) .and. to .ne. tlast) 
     &                 goto 100
                      if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))
     &                 goto 100
                    enddo
                  endif
                  if (t(j) .ge. wtim1 .and. t(j) .le. wtim2 .or. 
     &                wtim2 .eq. -1.0) then
                    if (worksp(lin1+j) .gt. prtmax(1)) then
                      prtmax(1) = worksp(lin1+j)
                      timax1 = t(j)
                    endif
                    if (worksp(lin1+j) .lt. prtmin(1)) then
                      prtmin(1) = worksp(lin1+j)
                      timin1 = t(j)
                    endif
                  endif
                endif
  100           if (iopt2 .ne. 0) then
 
C                 CALCULATE MVAR'S IF REQUESTED
 
                  worksp(lin2+j) = (fi*ctr-ei*cti)*bmva
                  if (worksp(lin2+j) .gt. worksp(lin2a)) 
     &             worksp(lin2a) = worksp(lin2+j)
                  if (worksp(lin2+j) .lt. worksp(lin2a+1)) 
     &             worksp(lin2a+1) = worksp(lin2+j)
C                 
C                 DO NOT SAVE MAX AND MINS IF A FAULT IS APPLIED A
C                 
                  if (ifltkt .ne. 0) then
                    to = t(j)
                    if (j .gt. 1) then
                      tlast = t(j-1)
                    else
                      tlast =  - 1.
                    endif
                    do l = 1, ifltkt
                      if (to .eq. fstrt(l) .and. to .eq. tlast) 
     &                 goto 110
                      if (to .eq. fstop(l) .and. to .ne. tlast) 
     &                 goto 110
                      if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))
     &                 goto 110
                    enddo
                  endif
                  if (t(j) .ge. wtim1 .and. t(j) .le. wtim2 .or. 
     &                wtim2 .eq. -1.0) then
                    if (worksp(lin2+j) .gt. prtmax(2)) then
                      prtmax(2) = worksp(lin2+j)
                      timax2 = t(j)
                    endif
                    if (worksp(lin2+j) .lt. prtmin(2)) then
                      prtmin(2) = worksp(lin2+j)
                      timin2 = t(j)
                    endif
                  endif
                endif
  110           if (iopt3 .ne. 0) then
 
C                 CALCULATE APPARENT IMPEDANCE IF REQUESTED
 
                  if (izer .eq. 1) then
                    worksp(iz+ind1) = (-ei*cti+fi*ctr) / 
     &                                (ctr*ctr+cti*cti)*zbase
                    worksp(iz+ind) = (ei*ctr+fi*cti) / 
     &                               (ctr*ctr+cti*cti) * zbase
                  else
                    worksp(iz+ind) = 0.0
                    worksp(iz+ind1) = 0.0
                  endif
                endif
                if (iopt4 .ne. 0) then
 
C                 CALCULATE LINE CURRENT IF REQUESTED
 
                  xmw = ei*ctr + fi*cti
                  worksp(lin4+j) = sqrt(ctr*ctr+cti*cti)*ctbase
                  if (worksp(lin4+j) .gt. worksp(lin4a)) 
     &              worksp(lin4a) = worksp(lin4+j)
                  if (worksp(lin4+j) .lt. worksp(lin4a+1)) 
     &              worksp(lin4a+1) = worksp(lin4+j)
C                 
C                 DO NOT SAVE MAX AND MINS IF A FAULT IS APPLIED A
C                 
                  if (ifltkt .ne. 0) then
                    to = t(j)
                    if (j .gt. 1) then
                      tlast = t(j-1)
                    else
                      tlast =  - 1.
                    endif
                    do l = 1, ifltkt
                      if (to .eq. fstrt(l) .and. to .eq. tlast) 
     &                 goto 120
                      if (to .eq. fstop(l) .and. to .ne. tlast) 
     &                 goto 120
                      if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))
     &                 goto 120
                    enddo
                  endif
                  if (t(j) .ge. wtim1 .and. t(j) .le. wtim2 .or. 
     &                wtim2 .eq. -1.0) then
                    if (worksp(lin4+j) .gt. prtmax(6)) then
                      prtmax(6) = worksp(lin4+j)
                      timax6 = t(j)
                    endif
                    if (worksp(lin4+j) .lt. prtmin(6)) then
                      prtmin(6) = worksp(lin4+j)
                      timin6 = t(j)
                    endif
                  endif
                endif
  120           if (iopt5 .ne. 0) then
 
C                 CALCULATE Z-Z DOT IF REQUESTED
 
                  if (j .eq. 1) then
                    if (izer .eq. 1) then
                      zr = zbase*(ei*ctr+fi*cti)/(ctr*ctr+cti*cti)
                      zi = zbase*(-ei*cti+fi*ctr)/(ctr*ctr+cti*cti)
                    else
                      zr = 0.0
                      zi = 0.0
                    endif
                    zpast = sqrt(zr*zr+zi*zi)
                    worksp(ind+lin5) = zpast
                    worksp(ind1+lin5) = 0.0
                  else
                    delt = t(j) - t(j-1)
                    if (delt .gt. .001) then
                      if (izer .eq. 1) then
                        zr = zbase*(ei*ctr+fi*cti)/(ctr*ctr+cti*cti)
                        zi = zbase*(-ei*cti+fi*ctr)/(ctr*ctr+cti*cti)
                      else
                        zr = 0.0
                        zi = 0.0
                      endif
                      znow = sqrt(zr*zr+zi*zi)
                      worksp(lin5+ind) = znow
                      worksp(lin5+ind1) = 60.*(znow-zpast)/delt
                      if (t(j) .ge. wtim1 .and. t(j) .le. wtim2 .or.
     &                    wtim2 .eq. -1.0) then
                        if (worksp(lin5+ind) .gt. prtmax(9)) 
     &                   prtmax(9) = worksp(lin5+ind)
                        if (worksp(lin5+ind) .lt. prtmin(9)) 
     &                   prtmin(9) = worksp(lin5+ind)
                        if (worksp(lin5+ind1) .gt. prtmax(10)) 
     &                   prtmax(10) = worksp(lin5+ind1)
                        if (worksp(lin5+ind1) .lt. prtmin(10)) 
     &                   prtmin(10) = worksp(lin5+ind1)
                      endif
                      zpast = znow
                    else
                      worksp(lin5+ind) = worksp(lin5+ind-2)
                      worksp(lin5+ind1) = worksp(lin5+ind1-2)
                    endif
                  endif
                endif
 
                if (iopt6 .ne. 0) then
 
C                 CALCULATE R-R DOT IF REQUESTED
 
                  pmw = (ei*ctr+fi*cti)
                  qmv = (fi*ctr-ei*cti)
                  vkv = sqrt(ei*ei+fi*fi)
                  if (j .ne. 1) deltim = t(j) - t(j-1)
                  call rdot(j)
                  worksp(lin6+ind) = rnow
                  worksp(lin6+ind1) = rdnow
C                 
C                 DO NOT SAVE MAX AND MINS IF A FAULT IS APPLIED A
C                 
                  if (ifltkt .ne. 0) then
                    to = t(j)
                    if (j .gt. 1) then
                      tlast = t(j-1)
                    else
                      tlast =  - 1.
                    endif
                    do l = 1, ifltkt
                      if (to .eq. fstrt(l) .and. to .eq. tlast) 
     &                 goto 130
                      if (to .eq. fstop(l) .and. to .ne. tlast) 
     &                 goto 130
                      if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))
     &                 goto 130
                    enddo
                  endif
                  if (t(j) .ge. wtim1 .and. t(j) .le. wtim2 .or. 
     &                wtim2 .eq. -1.0) then
                    if (worksp(lin6+ind) .gt. prtmax(7)) then
                      prtmax(7) = worksp(lin6+ind)
                      timax7 = t(j)
                    endif
                    if (worksp(lin6+ind) .lt. prtmin(7)) then
                      prtmin(7) = worksp(lin6+ind)
                      timin7 = t(j)
                    endif
                    if (worksp(lin6+ind1) .gt. prtmax(8)) then
                      prtmax(8) = worksp(lin6+ind1)
                      timax8 = t(j)
                    endif
                    if (worksp(lin6+ind1) .lt. prtmin(8)) then
                      prtmin(8) = worksp(lin6+ind1)
                      timin8 = t(j)
                    endif
                  endif
                endif
  130           if (iopt7 .ne. 0) then
 
C                 Plot TCSC reactance if requested
 
                  jtcsc = lndatn(4, 3, i)
                  if (jtcsc .gt. 0) then
                    worksp(lin7+j) = tcscdata(j,1,jtcsc)
                    if (worksp(lin7+j) .gt. worksp(lin7a)) 
     &                worksp(lin7a) = worksp(lin7+j)
                    if (worksp(lin7+j) .lt. worksp(lin7a+1)) 
     &                worksp(lin7a+1) = worksp(lin7+j)
                  else
                    worksp(lin7+j) = 0.0
                  endif
C                 
C                 DO NOT SAVE MAX AND MINS IF A FAULT IS APPLIED A
C                 
                  if (ifltkt .ne. 0) then
                    to = t(j)
                    if (j .gt. 1) then
                      tlast = t(j-1)
                    else
                      tlast =  - 1.
                    endif
                    do l = 1, ifltkt
                      if (to .eq. fstrt(l) .and. to .eq. tlast) 
     &                 goto 132
                      if (to .eq. fstop(l) .and. to .ne. tlast) 
     &                 goto 132
                      if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))
     &                 goto 132
                    enddo
                  endif
                  if (t(j) .ge. wtim1 .and. t(j) .le. wtim2 .or. 
     &                wtim2 .eq. -1.0) then
                    if (worksp(lin7+j) .gt. prtmax(11)) then
                      prtmax(11) = worksp(lin7+j)
                      timax11 = t(j)
                    endif
                    if (worksp(lin7+j) .lt. prtmin(11)) then
                      prtmin(11) = worksp(lin7+j)
                      timin11 = t(j)
                    endif
                  endif
                endif
  132           if (iopt8 .ne. 0) then
 
C                 Plot TCSC synthetic angle if requested
 
                  jtcsc = lndatn(4, 3, i)
                  if (jtcsc .gt. 0) then
                    worksp(lin8+j) = tcscdata(j,2,jtcsc)
                    if (worksp(lin8+j) .gt. worksp(lin8a)) 
     &                worksp(lin8a) = worksp(lin8+j)
                    if (worksp(lin8+j) .lt. worksp(lin8a+1)) 
     &                worksp(lin8a+1) = worksp(lin8+j)
                  else
                    worksp(lin8+j) = 0.0
                  endif
C                 
C                 DO NOT SAVE MAX AND MINS IF A FAULT IS APPLIED A
C                 
                  if (ifltkt .ne. 0) then
                    to = t(j)
                    if (j .gt. 1) then
                      tlast = t(j-1)
                    else
                      tlast =  - 1.
                    endif
                    do l = 1, ifltkt
                      if (to .eq. fstrt(l) .and. to .eq. tlast) 
     &                 goto 134
                      if (to .eq. fstop(l) .and. to .ne. tlast) 
     &                 goto 134
                      if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))
     &                 goto 134
                    enddo
                  endif
                  if (t(j) .ge. wtim1 .and. t(j) .le. wtim2 .or. 
     &                wtim2 .eq. -1.0) then
                    if (worksp(lin8+j) .gt. prtmax(12)) then
                      prtmax(12) = worksp(lin8+j)
                      timax12 = t(j)
                    endif
                    if (worksp(lin8+j) .lt. prtmin(12)) then
                      prtmin(12) = worksp(lin8+j)
                      timin12 = t(j)
                    endif
                  endif
                endif
  134           continue
              enddo

C             STORE MAX AND MIN DATA FOR LINE QUANTITY SUMMARY
C             STORE MW QUANTITIES
C             
              if (anlop(lndatn(2, 3, i))) then
                anlmax(1, ilinal) = prtmax(1)
                anltax(1, ilinal) = timax1
                anlmin(1, ilinal) = prtmin(1)
                anltin(1, ilinal) = timin1
              endif

C             STORE MVAR QUANTITIES

              if (anlop(lndatn(2, 5, i))) then
                anlmax(2, ilinal) = prtmax(2)
                anltax(2, ilinal) = timax2
                anlmin(2, ilinal) = prtmin(2)
                anltin(2, ilinal) = timin2
              endif

C             STORE CURRENT QUANTITIES

              if (anlop(lndatn(3, 3, i))) then
                anlmax(3, ilinal) = prtmax(6)
                anltax(3, ilinal) = timax6
                anlmin(3, ilinal) = prtmin(6)
                anltin(3, ilinal) = timin6
              endif

C             STORE R AND RDOT QUANTITIES

              if (anlop(lndatn(3, 7, i))) then
                anlmax(4, ilinal) = prtmax(7)
                anltax(4, ilinal) = timax7
                anlmin(4, ilinal) = prtmin(7)
                anltin(4, ilinal) = timin7
                anlmax(5, ilinal) = prtmax(8)
                anltax(5, ilinal) = timax8
                anlmin(5, ilinal) = prtmin(8)
                anltin(5, ilinal) = timin8
              endif

C             STORE TCSC Xc QUANTITIES
C             
              if (anlop(lndatn(4, 6, i))) then
                anlmax(6, ilinal) = prtmax(11)
                anltax(6, ilinal) = timax11
                anlmin(6, ilinal) = prtmin(11)
                anltin(6, ilinal) = timin11
              endif

C             STORE TCSC Angle QUANTITIES
C             
              if (anlop(lndatn(4, 7, i))) then
                anlmax(7, ilinal) = prtmax(12)
                anltax(7, ilinal) = timax12
                anlmin(7, ilinal) = prtmin(12)
                anltin(7, ilinal) = timin12
              endif
C             
C             STORE LINE DATA REQUESTED ON MV CARDS
C             
              if (mvn .ne. 0) then
                do n = 1, mvn
                  nmvi = mvadt(n)
                  ioptv = mvott(n)
                  mvadr(nmvi) = madr1
                  if (ioptv .eq. 1) mvstrt = lin1
                  if (ioptv .eq. 2) mvstrt = lin2
                  if (ioptv .eq. 5) mvstrt = lin4
                  if (ioptv .eq. 3 .or. ioptv .eq. 4) mvstrt = iz
                  if (ioptv .eq. 6 .or. ioptv .eq. 7) mvstrt = lin5
                  if (ioptv .eq. 8) mvstrt = lin6
                  if (ioptv .eq. 1 .or. ioptv .eq. 2 .or. ioptv .eq. 5)
     &             then
                    do l = 1, icount + 2
                      vspce(madr1+l) = worksp(mvstrt+l)
                    enddo
                    madr1 = madr1 + icount + 2
                  elseif (ioptv .eq. 4 .or. ioptv .eq. 7 .or. 
     &                    ioptv .eq. 8) then
                    vmax = 0.0
                    vmin = 0.0
                    do l = 1, icount
                      iadr1 = mvstrt + 2*l
                      if (worksp(iadr1) .gt. vmax) vmax = worksp(iadr1)
                      if (worksp(iadr1) .lt. vmin) vmin = worksp(iadr1)
                      vspce(madr1+l) = worksp(iadr1)
                    enddo
                    vspce(madr1+icount+1) = vmax
                    vspce(madr1+icount+2) = vmin
                    madr1 = madr1 + icount + 2
                  elseif (ioptv .eq. 3 .or. ioptv .eq. 6) then
                    vmax = 0.0
                    vmin = 0.0
                    do l = 1, icount
                      iadr1 = mvstrt + 2*l - 1
                      if (worksp(iadr1) .gt. vmax) vmax = worksp(iadr1)
                      if (worksp(iadr1) .lt. vmin) vmin = worksp(iadr1)
                      vspce(madr1+l) = worksp(iadr1)
                    enddo
                    vspce(madr1+icount+1) = vmax
                    vspce(madr1+icount+2) = vmin
                    madr1 = madr1 + icount + 2
                  endif
                enddo
              endif
              if (iopt1 .ne. 0) then
                if (plotop(iopt1)) then
                  ktnol1 = ktnol1 + 1
                  call writmp (13, worksp(lin1+1), icount+2, 1)
                  call ritecp(msub13(2), ksb13a+ktnol1-1, 1)
                endif
                if (prntop(iopt1)) then
                  write (outbuf, 10040) prtmax(1), prtmin(1)
10040             format ('0', 5x, 'LINE FLOW REAL (MW)', 5x, 
     &             ' MAX = ', f10.2, ' MIN = ', f10.2)
                  call prtout(1)
                  do jjj = 1, icount, 5
                    kkk = min0(jjj+4, icount)
                    write (outbuf, 10050) (t(jj), worksp(lin1+jj), 
     &               jj = jjj, kkk)
                    call prtout(1)
                  enddo
10050             format (5(1x, f8.2, 'CYCLES ', f10.2))
                endif
                if (auxop(iopt1)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10060)
10060               format ('REAL POWER')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! REAL POWER'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
C                   CALL VARFMT (WORKSP(LIN1+JJ),ATEM,10)
C                   WRITE(L11,6210) T(JJ), ATEM
C                   6210       FORMAT (F9.2,1X,A10)
                      call varfmt(worksp(lin1+jj), atem, 10)
                      write (l11, '(F9.2,1X,A10)') t(jj), atem
                    else
                      call varfmt(worksp(lin1+jj), atem, 15)
                      write (l11, '(F13.6,1X,A15)') t(jj), atem
                    endif
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE(L11,6215)
C                 6215     FORMAT('  -100.00')
                endif
              endif
              if (iopt2 .ne. 0) then
                if (plotop(iopt2)) then
                  ktnol2 = ktnol2 + 1
                  call writmp (13, worksp(lin2+1), icount+2, 1)
                  call ritecp(msub13(2), ksb13b+ktnol2-1, 1)
                endif
                if (prntop(iopt2)) then
                  write (outbuf, 10070) prtmax(2), prtmin(2)
10070             format ('0', 5x, 'LINE FLOW REACTIVE (MVAR)', 5x,
     &             ' MAX = ', f10.2, ' MIN = ', f10.2)
                  call prtout(1)
                  do jjj = 1, icount, 5
                    kkk = min0(jjj+4, icount)
                    write (outbuf, 10080) (t(jj), worksp(lin2+jj), 
     &               jj = jjj, kkk)
                    call prtout(1)
                  enddo
10080             format (5(1x, f8.2, 'CYCLES ', f10.2))
                endif
                if (auxop(iopt2)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10090)
10090               format ('REACTIVE POWER')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! REACTIVE POWER'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
C                   CALL VARFMT(WORKSP(LIN2+JJ),ATEM,10)
C                   WRITE(L11,6210) T(JJ),ATEM
                      call varfmt(worksp(lin2+jj), atem, 10)
                      write (l11, '(F9.2,1X,A10)') t(jj), atem
                    else
                      call varfmt(worksp(lin2+jj), atem, 15)
                      write (l11, '(F13.6,1X,A15)') t(jj), atem
                    endif
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE(L11,6215)
                endif
              endif
              if (iopt3 .ne. 0) then
                if (plotop(iopt3)) then
                  ktzpp = ktzpp + 1
                  if (icnt2 .gt. 0) call writmp (13, worksp(iz+1),
     &             icnt2, 1)
                  call ritecp(msub13(2), ksb13d+ktzpp-1, 1)
                endif
C               
C               WRITE APPARENT R AND X TO AUXILIARY FILE
C               
                if (auxop(iopt3)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10100)
10100               format ('APPARENT RESISTANCE')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! APPARENT RESISTANCE'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
                    ind = 2*jj + iz - 1
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
                      call varfmt(worksp(ind), atem, 10)
                      write (l11, '(F9.2,1X,A10)') t(jj), atem
                    else
                      call varfmt(worksp(ind), atem, 15)
                      write (l11, '(F13.6,1X,A15)') t(jj), atem
                    endif
C                   CALL VARFMT(WORKSP(IND),ATEM,10)
C                   WRITE(L11,6210) T(JJ),ATEM
                    call endo()
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE (L11,6215)
 
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10110)
10110               format ('APPARENT REACTANCE')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! APPARENT REACTANCE'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
                    ind1 = 2*jj + iz
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
                      call varfmt(worksp(ind1), atem, 10)
                      write (l11, '(F9.2,1X,A10)') t(jj), atem
                    else
                      call varfmt(worksp(ind1), atem, 15)
                      write (l11, '(F13.6,1X,A15)') t(jj), atem
                    endif
C                   CALL VARFMT(WORKSP(IND1),ATEM,10)
C                   WRITE(L11,6210) T(JJ),ATEM
                    call endo()
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE (L11,6215)
                endif
                if (prntop(iopt3) .or. auxop(iopt3)) then
                  do jj = 1, icount
 
                    ind1 = 2*jj + iz
                    ind = ind1 - 1
                    ra = worksp(ind)
                    xa = worksp(ind1)
                    if (.not. ((ra .eq. 0.0) .and. (xa .eq. 0.0))) then
                      worksp(ind) = sqrt(ra*ra+xa*xa)
                      worksp(ind1) = atan2(xa, ra)*degrad
                      if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or.
     &                    wtim2 .eq. -1.0) then
                        if (worksp(ind) .gt. prtmax(3)) 
     &                   prtmax(3) = worksp(ind)
                        if (worksp(ind) .lt. prtmin(3)) 
     &                   prtmin(3) = worksp(ind)
                        if (worksp(ind1) .gt. prtmax(4)) 
     &                   prtmax(4) = worksp(ind1)
                        if (worksp(ind1) .lt. prtmin(4)) 
     &                   prtmin(4) = worksp(ind1)
                      endif
                    endif
                  enddo
                endif
                if (prntop(iopt3)) then
                  write (outbuf, 10120)
10120             format ('0', 5x, 
     &             'APPARENT IMPEDANCE - MAGNITUDE(OHMS) AND',
     &             ' ANGLE(DEGREES)')
                  call prtout(1)
                  write (outbuf, 10130) prtmax(3), prtmin(3), 
     &             prtmax(4), prtmin(4)
10130             format ('0', 5x, ' MAX MAG = ', f9.4, ' MIN MAG = ',
     &             f9.4, ' MAX ANGLE = ', f7.2, ' MIN ANGLE = ', f7.2)
                  call prtout(1)
                  do jjj = 1, icount, 4
                    kkk = min0(jjj+3, icount)
                    write (outbuf, 10140) (t(jj), worksp(2*jj-1+iz),
     &               worksp(2*jj+iz), jj = jjj, kkk)
                    call prtout(1)
                  enddo
10140             format (4(1x, f8.2, 'CYCLES ', f9.4, 1x, f7.2))
                endif
                if (auxop(iopt3)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10150)
10150               format ('IMPEDANCE (MAG AND ANGLE)')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! IMPEDANCE (MAG AND ANGLE)'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
                    lj = (2*jj) + iz - 1
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
C                   CALL VARFMT(WORKSP(2*JJ-1+IZ),ATEM,10)
C                   WRITE(L11,6355) T(JJ), ATEM, WORKSP(2*JJ+IZ)
C                   6355         FORMAT(F9.2,1X,A10,F10.4)
                      call varfmt(worksp(lj), atem, 10)
                      write (l11, '(F9.2,1X,A10,F10.4)') t(jj), atem,
     &                 worksp(lj+1)
                    else
                      call varfmt(worksp(lj), atem, 15)
                      call varfmt(worksp(lj+1), atem1, 15)
                      write (l11, '(F13.6,2(1X,A15))') t(jj), atem,
     &                 atem1
                    endif
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE (L11,6215)
                endif
              endif
              if (iopt4 .ne. 0) then
                if (plotop(iopt4)) then
                  ktnol4 = ktnol4 + 1
                  call writmp (13, worksp(lin4+1), icount+2, 1)
                  call ritecp(msub13(2), ksb13e+ktnol4-1, 1)
                endif
                if (prntop(iopt4)) then
                  write (outbuf, 10160) prtmax(6), prtmin(6)
10160             format ('0', 5x, 'LINE CURRENT IN AMPS', 5x,
     &             ' MAX = ', f9.2, ' MIN = ', f9.2)
                  call prtout(1)
                  do jjj = 1, icount, 5
                    kkk = min0(jjj+4, icount)
                    write (outbuf, 10170) (t(jj), worksp(jj+lin4), 
     &               jj = jjj, kkk)
10170               format (5(1x, f8.2, 'CYCLES', f9.2))
                    call prtout(1)
                  enddo
                endif
                if (auxop(iopt4)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10180)
10180               format ('LINE CURRENT')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! LINE CURRENT'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
C                   CALL VARFMT(WORKSP(JJ+LIN4),ATEM,10)
C                   WRITE(L11,6363) T(JJ),ATEM
C                   6363       FORMAT(F9.2,1X,A10)
                      call varfmt(worksp(jj+lin4), atem, 10)
                      write (l11, '(F9.2,1X,A10)') t(jj), atem
                    else
                      call varfmt(worksp(jj+lin4), atem, 15)
                      write (l11, '(F13.6,1X,A15)') t(jj), atem
                    endif
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE (L11,6215)
                endif
              endif
              if (iopt5 .ne. 0) then
                if (plotop(iopt5)) then
                  ktnol5 = ktnol5 + 1
                  call writmp (13, worksp(lin5+1), icnt2, 1)
                  call ritecp(msub13(2), ksb13f+ktnol5-1, 1)
                endif
                if (prntop(iopt5)) then
                  write (outbuf, 10190)
                  call prtout(1)
10190             format ('0', 5x,
     &             ' APPARENT IMPEDANCE-MAGNITUDE(OHMS) AND RATE',
     &             '(OHMS/SEC)')
                  write (outbuf, 10200) prtmax(9), prtmin(9), prtmax
     &             (10), prtmin(10)
                  call prtout(1)
10200             format ('0', 5x, ' MAX MAG = ', f9.2, ' MIN MAG = ',
     &             f9.2, ' MAX RATE = ', f9.2, ' MIN RATE = ', f9.2)
                  do jjj = 1, icount, 3
                    kkk = min0(jjj+2, icount)
                    write (outbuf, 10210) (t(jj), worksp(lin5+2*jj-1),
     &               worksp(lin5+2*jj), jj = jjj, kkk)
10210               format (4(1x, f8.2, 'CYCLES', f8.3, 1x, f7.2))
                    call prtout(1)
                  enddo
                endif
                if (auxop(iopt5)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10220)
10220               format ('APPARENT IMPEDANCE - MAGNITUDE AND RATE')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(2A)')
     &               '  ! APPARENT IMPEDANCE - MAGNITUDE ', 'AND RATE'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
                    lj = (2*jj) + lin5 - 1
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
C                   CALL VARFMT(WORKSP(LIN5+2*JJ-1),ATEM,10)
C                   ATEM1 = ATEM
C                   CALL VARFMT(WORKSP(LIN5+2*JJ),ATEM,10)
C                   WRITE(L11,7027) T(JJ),ATEM1,ATEM
C                   7027       FORMAT(F9.2,1X,2A10)
                      call varfmt(worksp(lj), atem, 10)
                      call varfmt(worksp(lj+1), atem1, 10)
                      write (l11, '(F9.2,1X,2A10)') t(jj), atem, atem1
                    else
                      call varfmt(worksp(lj), atem, 15)
                      call varfmt(worksp(lj+1), atem1, 15)
                      write (l11, '(F13.6,2(1X,A15))') t(jj), atem,
     &                 atem1
                    endif
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE (L11,6215)
                endif
              endif
              if (iopt6 .ne. 0) then
                if (plotop(iopt6)) then
                  ktnol6 = ktnol6 + 1
                  call writmp (13, worksp(lin6+1), icnt2, 1)
                  call ritecp(msub13(2), ksb13g+ktnol6-1, 1)
                endif
                if (prntop(iopt6)) then
                  write (outbuf, 10230)
10230             format ('0', 5x,
     &             ' APPARENT RESISTANCE-MAGNITUDE(OHMS) AND RATE',
     &             '(OHMS/SEC)')
                  call prtout(1)
                  write (outbuf, 10240) prtmax(7), prtmin(7), 
     &             prtmax(8), prtmin(8)
                  call prtout(1)
10240             format ('0', 5x, ' MAX MAG = ', f8.3, ' MIN MAG = ',
     &             f8.3, ' MAX RATE = ', f7.2, ' MIN RATE = ', f7.2)
                  do jjj = 1, icount, 3
                    kkk = min0(jjj+2, icount)
                    write (outbuf, 10250) (t(jj), worksp(lin6+2*jj-1),
     &               worksp(lin6+2*jj), jj = jjj, kkk)
10250               format (4(1x, f8.2, 'CYCLES', f8.3, 1x, f7.2))
                    call prtout(1)
                  enddo
                endif
C               
C               CALL RMARGN TO CALCULATE AND PRINT THE TRIP MARGIN
C               R-RDOT RELAY
C               
                call rmargn(bus1, base1, bus2, base2, ipar, lin6)
                if (auxop(iopt6)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10260)
10260               format ('APPARENT RESISTANCE - MAGNITUDE AND RATE')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)')
     &               '  ! APPARENT RESISTANCE - MAGNITUDE ', 'AND RATE'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
                    lj = (2*jj) + lin6 - 1
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
C                   CALL VARFMT(WORKSP(LIN6+2*JJ-1),ATEM,10)
C                   ATEM1 = ATEM
C                   CALL VARFMT(WORKSP(LIN6+2*JJ),ATEM,10)
C                   WRITE(L11,7067) T(JJ),ATEM1,ATEM
C                   7067       FORMAT(F9.2,1X,2A10)
                      call varfmt(worksp(lj), atem, 10)
                      call varfmt(worksp(lj+1), atem1, 10)
                      write (l11, '(F9.2,1X,2A10)') t(jj), atem, atem1
                    else
                      call varfmt(worksp(lj), atem, 15)
                      call varfmt(worksp(lj+1), atem1, 15)
                      write (l11, '(F13.6,2(1X,A15))') t(jj), atem,
     &                 atem1
                    endif
                  enddo
                  write (l11, '(A)') crvend
C                 WRITE (L11,6215)
                endif
              endif
              if (iopt7 .ne. 0) then
                if (plotop(iopt7)) then
                  ktnol7 = ktnol7 + 1
                  call writmp (13, worksp(lin7+1), icount+2, 1)
                  call ritecp(msub13(2), ksb13h+ktnol7-1, 1)
                endif
                if (prntop(iopt7)) then
                  write (outbuf, 10262) prtmax(11), prtmin(11)
10262             format ('0', 5x, 'TCSC Impedance (ohms)', 
     &             5x, ' MAX = ', f10.2, ' MIN = ', f10.2)
                  call prtout(1)
                  do jjj = 1, icount, 5
                    kkk = min0(jjj+4, icount)
                    write (outbuf, 10050) (t(jj), worksp(lin7+jj), jj =
     &               jjj, kkk)
                    call prtout(1)
                  enddo
                endif
                if (auxop(iopt7)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10264)
10264               format ('TCSC XC')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! TCSC XC'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
                      call varfmt(worksp(lin7+jj), atem, 10)
                      write (l11, '(F9.2,1X,A10)') t(jj), atem
                    else
                      call varfmt(worksp(lin7+jj), atem, 15)
                      write (l11, '(F13.6,1X,A15)') t(jj), atem
                    endif
                  enddo
                  write (l11, '(A)') crvend
                endif
              endif
              if (iopt8 .ne. 0) then
                if (plotop(iopt8)) then
                  ktnol8 = ktnol8 + 1
                  call writmp (13, worksp(lin8+1), icount+2, 1)
                  call ritecp(msub13(2), ksb13h+ktnol8-1, 1)
                endif
                if (prntop(iopt8)) then
                  write (outbuf, 10266) prtmax(12), prtmin(12)
10266             format ('0', 5x, 'TCSC Synthetic angle (degrees)', 
     &             5x, ' MAX = ', f10.2, ' MIN = ', f10.2)
                  call prtout(1)
                  do jjj = 1, icount, 5
                    kkk = min0(jjj+4, icount)
                    write (outbuf, 10050) (t(jj), worksp(lin8+jj), 
     &               jj = jjj, kkk)
                    call prtout(1)
                  enddo
                endif
                if (auxop(iopt8)) then
                  if (auxfmt .eq. 'STD') then
                    write (l11, 10268)
10268               format ('TCSC angle')
                  else
                    write (l11, '(A)') '/CURVE'
                    write (l11, '(2A)') '  LEGEND  ', lname
                    write (l11, '(A)') '  ! TCSC ANGLE'
                    write (l11, '(A)') '  DATA'
                  endif
                  do jj = 1, icount
C                   -  New format has wider fields
                    if (auxfmt .eq. 'STD') then
                      call varfmt(worksp(lin8+jj), atem, 10)
                      write (l11, '(F9.2,1X,A10)') t(jj), atem
                    else
                      call varfmt(worksp(lin8+jj), atem, 15)
                      write (l11, '(F13.6,1X,A15)') t(jj), atem
                    endif
                  enddo
                  write (l11, '(A)') crvend
                endif
              endif
            endif
          enddo
          if (linekt .le. 0) goto 140
        enddo
      endif
C     
C     CALL LINSUM TO PRINT LINE QUANTITY SUMMARY
C     
  140 if (ilinal .ne. 0) call linsum()
      return
      end
