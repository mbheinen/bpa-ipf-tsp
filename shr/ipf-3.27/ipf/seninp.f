C    @(#)seninp.f	20.3 2/13/96
      subroutine seninp
C
C     Processe the following sensitivity records:
C
C       /BUS_SENSITIVITY
C       /LINE_SENSITIVITY
C       /TRANSFER_SENSITIVITY
C       /LOSS_SENSITIVITY
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/lnsen.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/transf.inc'
 
      integer find_bus, areasr, error

      character id * 1

      complex z, yeq(2,2), y1(2,2), y2(2,2), yxy(2,2)
 
      external kmpsen, swpsen, kmptrt, swptrt, kmptrf, swptrf,
     1         kmptrl, swptrl
 
      error = 0
      if (index(buf,'BUSSEN') .ne. 0 .or. index(buf,'BUS_SEN') .ne. 0)
     1   then
         idat = 0
         call sendec
         if (idat .gt. 0) then
            error = 0
            do 1300 i = 1, idat
            nb = find_bus(namdat(i),bsedat(i))
            if (nb .le. 0) then
               write (errbuf(1),1000) namdat(i), bsedat(i)
 1000          format ('0 BUS_SENSITIVITY bus ',a8,f7.1,
     &                 ' is not in system. Record is ignored.')
               call prterx ('W',1)
               error = 1
            else
               nsen = nsen + 1
               if (nsen .gt. 50) then
                  write (errbuf(1),1100)
                  call prterx ('W',1)
 1100             format ('0 More than 50 BUS_SENSITIVITIES. ')
                  error = 1
                  nsen = 50
                  go to 1400
               else
                  ksen(1,nsen) = nb
C
C     ISW CODE: 1 - G PERTURBATION
C               2 - B PERTURBATION
C               3 - MW LOAD PERTURBATION
C               4 - MVAR LOAD PERTURBATION
C               5 - MW GENERATION PERTURBATION
C               6 - MVAR GENERATION PERTURBATION
C               7 - G PERTURBATION (DP/DV SENSITIVITY)
C               8 - MW LOAD PERTURBATION (DP/DV SENSITIVITY)
C               9 - MW GENERATION PERTURBATION (DP/DV SENSITIVITY)
C
                  iflag = 0
                  do 1200 j = 1, 6
                  if (data(j,i) .ne. 0) then
                     if (zondat(i) .eq. 'BV') then
                        ksen(2,nsen) = 6 + (j + 1) / 2
                     else
                        ksen(2,nsen) = j
                     endif
                     sen(3,nsen) = data(j,i)
                     iflag = 1
                  endif
 1200             continue
                  if (iflag .eq. 0) then
                     ksen(2,nsen) = 6
                     sen(3,nsen) = 100.0
                  endif
               endif
            endif
 1300       continue
 1400       if (nsen .eq. 0) then
               write (errbuf(1),1500)
 1500          format('0 No data follows "BUS_SENSITIVITIES" text.')
               call prterx ('W',1)
               error = 1
            else
               ndup = 0
               call qiksrt(1,nsen,kmpsen,swpsen)
               if (ndup .gt. 0) then
C
C     Eliminate duplicate data items
C
                  j = 1
                  k = 2
 1600             if (kmpsen(j,k) .lt. 0) then
                     j = j + 1
                     if (j .lt. k) call swpsen(j,k)
                     k = k + 1
                  else
                     nb = ksen(1,j)
                     write (errbuf(1),1700) bus(k1), base(k1)
 1700                format('0 Duplicate "BUS_SENSITIVITIES": ',
     1                       a8,f6.1,'. Second item ignored.')
                     call prterx ('W',1)
                     error = 1
                     k = k + 1
                  endif
                  if (k .le. nsen) go to 1600
                  nsen = j
               endif
               do 1900 i = 1, nsen
               nb = ksen(1,i)
 1900          ksen(1,i) = inp2opt(nb)
            endif
         endif
      else if (index(buf,'LINESE') .ne. 0 .or.
     1         index(buf,'LINE_SE') .ne. 0) then
         idat = 0
         call sendec
         if (idat .gt. 0) then
            do 2600 i = 1, idat - 1, 2
            error = 0
            k1 = find_bus(namdat(i),bsedat(i))
            if (k1 .le. 0) then
               write (errbuf(1),2000) namdat(i), bsedat(i)
 2000          format ('0 LINE_SENSITIVITY bus ',a8,f7.1,
     &                 ' is not in system and is ignored. ')
               call prterx ('W',1)
               error = 1
            endif
            k2 = find_bus(namdat(i+1),bsedat(i+1))
            if (k2 .le. 0) then
               write (errbuf(1),2000) namdat(i+1), bsedat(i+1)
               call prterx ('W',1)
               error = 1
            endif
            id = zondat(i+1)(1:1)
            read (zondat(i+1),2100,err=2102) ksect
 2100       format (1x,i1)
            go to 2106
 2102       write (errbuf(1), 2104) namdat(i), bsedat(i), namdat(i+1),
     1         bsedat(i+1),id, zondat(i+1)(2:2)
 2104       format ('0 LINE_SENSITIVITY branch ',a8,f7.1,1x,
     1            a8,f7.1,1x,a1,' has an illegal section "',a,'"')
            call prterx ('W',1)
            error = 1
            ksect = 0
 2106       nbr = numbrn (k1,k2,id,ksect)
            if (k1 .gt. 0 .and. k2 .gt. 0 .and. nbr .le. 0) then
               write (errbuf(1),2200) namdat(i), bsedat(i), namdat(i+1),
     1            bsedat(i+1),id, ksect
 2200          format ('0 LINE_SENSITIVITY branch ',a8,f7.1,1x,
     1            a8,f7.1,1x,a1,i2,' is not in system and is ignored. ')
               call prterx ('W',1)
               error = 1
            endif
            if (nbr .gt. 0 .and. zondat(i)(1:1) .eq. '>') then
               nlsen = nlsen + 1
               if (nlsen .gt. 50) then
                  write (errbuf(1),2300) 50
 2300             format ('0 More than ',i3,' LINE_SENSITIVITIES.')
                  call prterx ('W',1)
                  error = 1
                  nslen = 50
                  go to 2700
               else
                  ltype = index ('PVL',zondat(i)(2:2))
                  if (ltype .eq. 0) then
                     write (errbuf(1),2400) zondat(i)
 2400                format ('0 Illegal LINE_SENSITIVITY type (',a2,')')
                     call prterx ('W',1)
                     error = 1
                  endif
                  lsen(1,nlsen) = ltype
C
C     LSEN CODE: 1 - dPij/dBkl sensitivity
C                2 - dVi/dBkl sensitivity
C                3 - dLoss/dBkl sensitivity
C
                  lsen(2,nlsen) = k1
                  lsen(3,nlsen) = k2
                  lsen(4,nlsen) = ichar (id)
                  lsen(5,nlsen) = ksect
                  lsen(6,nlsen) = 0
C
C                 Check sensitivity dF/dX or dF/dB
C
                  if ((data(2,i) .eq. 0.0 .and. data(4,i) .eq. 0.0) .or.
     1                (data(2,i) .ne. 0.0 .and. data(4,i) .ne. 0.0) .or.
     2                (data(2,i) .ne. 0.0 .and. data(4,i) .eq. 0.0))
     3               then
                     ysen(7,nlsen) = data(2,i)
                     ysen(8,nlsen) = 0.0
                  else
                     ysen(7,nlsen) = 0.0
                     ysen(8,nlsen) = data(4,i)
                  endif
               endif
            else if (nbr .gt. 0 .and. (zondat(i)(1:1) .eq. 'L' .or.
     1                                 zondat(i)(1:1) .eq. 'E')) then
               ndsen = ndsen + 1
               if (ndsen .gt. 100) then
                  write (errbuf(1),2300) 100
                  call prterx ('W',1)
                  error = 1
                  ndsen = 100
                  go to 2700
               else
                  ldsen(1,ndsen) = k1
                  ldsen(2,ndsen) = k2
                  ldsen(3,ndsen) = ichar (id)
                  ldsen(4,ndsen) = ksect
                  ldsen(5,ndsen) = 0
                  ldsen(6,ndsen) = 0
                  if (nlsen .eq. 0) then
                     write (errbuf(1),2500) bus(k1), base(k1), bus(k2),
     1                  base(k2),id, ksect
 2500                format ('0 LINE_SENSITIVITY record ',a8,f6.1,
     1                       1x,a8,f6.1,1x,a1,i2,
     &                       ' is not preceded with an ">P" record.')
                     call prterx ('W',1)
                     error = 1
C
C     Check LSEN code: 1 - dPij/dBkl sensitivity
C
                  else if (lsen(1,nlsen) .ne. 1) then
                     write (errbuf(1),2500) bus(k1), base(k1), bus(k2),
     1                  base(k2),id, ksect
                     call prterx ('W',1)
                     error = 1
                  else if (lsen(6,nlsen) .eq. 0) then
                     lsen(6,nlsen) = ndsen
                  else if (ndsen .gt. 1) then
                     ldsen(5,ndsen-1) = ndsen
                  else
                  endif
               endif
            endif
 2600       continue
 2700       if (nlsen .eq. 0 .and. ndsen .eq. 0) then
               write (errbuf(1),2800)
 2800          format('0 No data follows "LINE_SENSITIVITIES" text.')
               call prterx ('W',1)
               error = 1
            endif
         endif
 
      else if (index(buf,'TRANSF') .ne. 0) then
         idat = 0
         call sendec
         if (idat .gt. 0) then
            do 2890 i = 1, idat - 1, 2
            error = 0
C
C     TRANSFER_SENSITIVITY code: "T" = Intertie transfer
C                                "L" = Outage
C                                "F" = Monitored overload
C
            if (zondat(i)(2:2) .eq. 'T') then
               k1 = areasr (aredat(i))
               if (k1 .le. 0) then
                  write (errbuf(1),2810) namdat(i), bsedat(i)
 2810             format ('0 TRANSFER_SENSITIVITY area ',a10,
     &                    ' is not in system and is ignored. ')
                  call prterx ('W',1)
                  error = 1
               endif
               k2 = areasr (aredat(i+1))
               if (k2 .le. 0) then
                  write (errbuf(1),2810) aredat(i+1)
                  call prterx ('W',1)
                  error = 1
               endif
               if (error .eq. 0) then
                  numt = numt + 1
                  if (numt .gt. MXTRSF) then
                     write (errbuf(1),2820) MXTRSF
 2820                format ('0 More than ',i3,' >TRANSFER interties.')
                     call prterx ('W',1)
                     error = 1
                     numt = MXTRSF
                     go to 2890
                  else
                     ktdata(1,numt) = k1
                     ktdata(2,numt) = k2
                     ktdata(3,numt) = 0
                     ktdata(4,numt) = 0
                  endif
               endif
 
            else if (zondat(i)(2:2) .eq. 'L' .or.
     1               zondat(i)(2:2) .eq. 'F') then
 
               k1 = find_bus (namdat(i),bsedat(i))
               if (k1 .le. 0) then
                  write (errbuf(1),2840) namdat(i), bsedat(i)
 2840             format ('0 >OVERLOAD or >OUTAGE terminal bus ',
     1              a8,f6.1,' is not in system and is ignored. ')
                  call prterx ('W',1)
                  error = 1
               endif
               k2 = find_bus (namdat(i+1),bsedat(i+1))
               if (k2 .le. 0) then
                  write (errbuf(1),2840) namdat(i+1), bsedat(i+1)
                  call prterx ('W',1)
                  error = 1
               endif
               id = zondat(i+1)(1:1)
               read (zondat(i+1),2850,err=2852) ksect
 2850          format (1x,i1)
               go to 2856
 2852          write (errbuf(1), 2854) namdat(i), bsedat(i),
     1            namdat(i+1), bsedat(i+1),id, zondat(i+1)(2:2)
 2854          format ('0 LINE_SENSITIVITY branch ',a8,f7.1,1x,
     1            a8,f7.1,1x,a1, ' has an illegal section "', a, '"')
               call prterx ('W',1)
               error = 1
               ksect = 0
 2856          nbr = numbrn (k1,k2,id,ksect)
               if (k1 .gt. 0 .and. k2 .gt. 0 .and. nbr .le. 0) then
                  write (errbuf(1),2860) namdat(i), bsedat(i),
     1               namdat(i+1), bsedat(i+1), id, ksect
 2860             format ('0 >OVERLOAD or >OUTAGE branch ',
     &                    a8,f7.1,1x,a8,f7.1,1x,a1,i2,
     &                    ' is not in system and is ignored. ')
                  call prterx ('W',1)
                  error = 1
               endif
               if (error .eq. 0) then
                  call getyeq (k1,k2,id,ksect,yeq,y1,yxy,y2)
                  z = -cmplx (1.0,0.0) / yeq(1,2)
                  r = real (z)
                  x = aimag (z)
                  if (abs (r) .gt. abs (x)) then
                     write (errbuf(1),2862) bus(k1), base(k1),
     1                  bus(k2), base(k2), id, ksect, r, x
 2862                format (' >OVERLOAD or >OUTAGE branch ',a8,f6.1,1x,
     1                  a8,f6.1,1x,a1,i2,' has R (',f8.5,') > X (',
     2                  f8.5,').')
                     call prterx ('W',1)
                     error = 1
                  endif
               endif
               if (error .eq. 0) then
                  if (zondat(i)(2:2) .eq. 'L') then
                     numl = numl + 1
                     if (numl .gt. MXTRSF) then
                        write (errbuf(1),2870) MXTRSF
 2870                   format ('0 More than ',i3,
     &                          ' >OVERLOAD branches.')
                        call prterx ('W',1)
                        error = 1
                        numl = MXTRSF
                        go to 2890
                     else
                        kldata(1,numl) = k1
                        kldata(2,numl) = k2
                        kldata(3,numl) = ichar (id)
                        kldata(4,numl) = ksect
                        kldata(5,numl) = nbr
                     endif
                  else
                     if (brnch(4,nbr) .eq. 0.0) then
                        write (errbuf(1),2872) bus(k1), base(k1),
     1                     bus(k2), base(k2), id, ksect
 2872                   format (' >OVERLOAD branch ',a8,f6.1,1x,a8,
     1                          f6.1,1x,a1,i2,' has zero rating ')
                        call prterx ('W',1)
                        error = 1
                        go to 2890
                     endif
                     numf = numf + 1
                     if (numf .gt. MXTRSF) then
                        write (errbuf(1),2880) MXTRSF
 2880                   format ('0 More than ',i3,
     &                          ' >OVERLOAD branches.')
                        call prterx ('W',1)
                        error = 1
                        numf = MXTRSF
                        go to 2890
                     else
                        kfdata(1,numf) = k1
                        kfdata(2,numf) = k2
                        kfdata(3,numf) = ichar (id)
                        kfdata(4,numf) = ksect
                        kfdata(5,numf) = nbr
                     endif
                  endif
               endif
            endif
 
 2890       continue
 
            if (numl .eq. 0 .or. numt .eq. 0 .or. numf .eq. 0) then
               write (errbuf(1),2900) numt, numl, numf
 2900          format('0 /TRANSFER_SENSITIVITIES data set has the',
     &                ' following populations: >TRANSFER (',i3,
     &                '), >OVERLOAD (',i3,'), >OUTAGE (',i3,').')
               write (errbuf(2),2910)
 2910          format('  Each category must have at least one item.')
               call prterx ('W',2)
               error = 1
            else
 
               if (numt .gt. 0) then
                   call qiksrt(1,numt,kmptrt,swptrt)
               endif
 
               if (numf .gt. 0) then
                   call qiksrt(1,numf,kmptrf,swptrf)
               endif
 
               if (numl .gt. 0) then
                   call qiksrt(1,numl,kmptrl,swptrl)
               endif
 
            endif
         endif
      else if (index(buf,'LOSSSEN') .ne. 0 .or.
     1         index(buf,'LOSS_SEN') .ne. 0) then
         idat = 0
         call sendec
      endif
      return
      end
