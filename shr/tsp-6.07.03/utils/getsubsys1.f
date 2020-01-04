C****************************************************************
C
C       File: getsubsys1.f
C       Purpose: Routine to generate a user-defined subsystem given
C                a kernel bus and the adjacency level.
C
C       Author: Walt Powell  Date: 13 Jan 1994
C                            Modified: 
C       Called by:
C
C****************************************************************
C
        subroutine getsubsys1 (lunsdi, lunswio) 
        integer lunsdi, lunswio

	include 'ipfinc/parametr.inc'	
	include 'ipfinc/lfiles.inc'
	include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/cbus.inc'

	common /shellsort/ buffer(1000)
        integer buffer

	external komp_buf, swap_buf

        character bus_name*8, capital*12, text*120, word(20)*12, 
     &            NULL*1, LINEFEED*1, query*1, code * 6, ratec * 10,
     &            record(6000)*80, sdidata(100)*80, basekvc * 4,
     &            casename * 12, ch*1, tempc * 12, pfcfile * 60, 
     &            solfile * 60, swifilen * 60
	integer find_bus, busback, first, ptr, subsystem(2,100), 
     &          whichend1, whichend2, flag(MAXBUS), status, open_file
        logical finished, found
        complex v1, v2, a12, z12
c
c       Set up pointers X-data (busxdtptr)
c
        if (.not. xdt_flag) then
           do nb = 1, ntot
              busxdtptr(nb)  = 0
           enddo
           do i = 1, kxtot
              kxd = xdata(1,i)
              if (kxd .gt. 0) busxdtptr(kxd) = i
           enddo
           xdt_flag = .true.
        endif
C
C       Input/Output request: Enter kernel bus
C
        NULL = char(0)
        LINEFEED = char(10)

  100   num_sub = 0
        num_sys = 0
        num_swi = 0
        num_sol = 0
        mark_bus = 0

        lunpfc = 2
        lunswin = 13
        lunsol = 15

        pfcfile = ' '
        solfile = ' '
        swifilen = ' '

        finished = .false.
        do while (.not. finished)
           last = 1
           write (*,210)
  210      format (' > Enter busname, KV, buses-back : ',$)
           read (*,212) text
  212      format (a)
           bus_name = ' '
           base_kv = 0.0
           busback = 0
           if (text .eq. ' ') go to 9000
c
C          Parse TEXT into busname, kv, and buses-back
c
           call parse_bus (text, word, nwrd)
           do i = nwrd+1, 3
              word(i) = ' '
           enddo
           bus_name = capital(word(1))
           if (index(word(2), '.') .eq. 0) then
              last = lastch(word(2))
              word(2) = word(2)(1:last) // '.0'
           endif
           read (word(2), 215, err = 220) base_kv
  215      format (f6.0)
           read (word(3), 216, err = 220) busback
  216      format (i1)

           if (bus_name .eq. ' ') go to 9000
           nb = find_bus (bus_name, base_kv)
   
           if (nb .gt. 0) then
              kernel = nb
              finished = .true.
   	      ilevel = 0
              first = 1
              last = 1
              buffer(last) = nb
              next = last
              do while (ilevel .le. busback .and. next .ge. last) 
                 if (first .lt. last) 
     &              call shellsrt (first, last, komp_buf, swap_buf)
                 do iv = first, last
                    nb = buffer(iv)
                    if (iv .eq. 1) then
                       num_sys = num_sys + 1
                       subsystem(1,num_sys) = nb
                       subsystem(2,num_sys) = 0
                    else
                       write (*,217) ilevel, bus(nb), base(nb)
  217                  format (' > ', i2, ' Keep, Drop, or Mark bus ', 
     &                         a8, f7.1, ' ? ',$)
                       read (*,212) query
                       if (query .eq. 'K' .or. query .eq. 'k') then
                          num_sys = num_sys + 1
                          subsystem(1,num_sys) = nb
                          subsystem(2,num_sys) = 0
                       else if (query .eq. 'M' .or. query .eq. 'm') then
                          num_sys = num_sys + 1
                          subsystem(1,num_sys) = nb
                          subsystem(2,num_sys) = 0
                          mark_bus = nb
                          busback = ilevel
                       endif
                    endif
                    ptr = kbsdta(16,nb)
                    do while (ptr .gt. 0)
                       do j = 1, next 
                          if (buffer(j) .eq. ky(ptr)) go to 242
                       enddo
                       next = next + 1
                       buffer(next) = ky(ptr)
  242                  continue
                       ptr = brnch_nxt(ptr)
                    enddo
                 enddo
                 ilevel = ilevel + 1
                 first = last + 1
                 last = next
              enddo
              if (num_sys .eq. 1) then
                 nb = buffer(1)
                 write (*,243) 
  243            format (' > Error - subsystem has only one bus')
                 go to 100
              else if (mark_bus .eq. 0) then
                 write (*,244) 
  244            format (' > Warning - No marked bus selected ')
                 do i = 1, num_sys
                    nb = subsystem(1,i)
                    write (*, 245) bus(nb), base(nb)
  245               format (' > Mark bus ', a8, f7.1, ' ? ',$)
                    read (*,212) query
                    if (query .eq. 'M' .or. query .eq. 'm' .or.
     &                  query .eq. 'Y' .or. query .eq. 'y') then
                       mark_bus = nb
                    endif
                 enddo
              endif
              write (*, 1244) num_sys
 1244         format (/, ' > Subsystem has ', i2, ' buses ', /)
              do i = 1, ntot
                 flag(i) = 0
              enddo
              do i = 1, num_sys
                 nb = subsystem(1,i)
                 flag(nb) = i
                 if (nb .eq. mark_bus) then
                    text = 'Marked'
                 else
                    text = ' '
                 endif
                 write (*, 1245) bus(nb), base(nb), text(1:6)
 1245            format (' > Bus ', a8, f7.1, 1x, a)
              enddo
              if (mark_bus .gt. 0) then
                 write (*, 246) 
  246            format (' > Enter INF BUS voltage (p.u.) and relative a
     &ngle (degrees) : ')
                 read (*, *) voltage, angle
c
c                Compute flow to cut system
c
                 pintot = 0.0
                 qintot = 0.0
                 ptr = kbsdta(16,mark_bus)
                 do while (ptr .gt. 0)
                    do j = 1, num_sys
                       if (subsystem(1,j) .eq. ky(ptr)) go to 247
                    enddo
                    ltyp = brtype(ptr)
                    if (ltyp .eq. 1) then
                       call gtlfq (ptr, pin, qin, ploss, qloss, ovld, 
     &                             ratec, actual_amps, whichend1, 
     &                             actual_mva, whichend2)
                       pintot = pintot + pin
                       qintot = qintot + qin
                       nptr = brnch_nxt(ptr)
                       do while (nptr .gt. 0 .and.
     &                          (ky(nptr) .eq. ky(ptr) .and.
     &                           brid(nptr) .eq. brid(ptr)))
                          oldptr = nptr
                          nptr = brnch_nxt(nptr)
                       enddo
                       ptr = oldptr
                       nptr = 0
                    else if (ltyp .eq. 4) then
                    else
                       call gtlfq (ptr, pin, qin, ploss, qloss, ovld, 
     &                             ratec, actual_amps, whichend1, 
     &                             actual_mva, whichend2)
                       pintot = pintot + pin
                       qintot = qintot + qin
                    endif
  247               ptr = brnch_nxt(ptr)
                 enddo
              endif

              casename = bus(kernel) 
              i = 1
              last = len (casename)
              do while (i .le. lastch (casename))
                 ch = casename(i:i)
                 if ((ch .ge. 'a' .and. ch .le. 'z') .or.
     &               (ch .ge. 'A' .and. ch .le. 'Z') .or.
     &               (ch .ge. '0' .and. ch .le. '9')) then
                    i = i + 1
                 else if (i .lt. last) then
                    casename(i:) = casename(i+1:)
                    casename(last:last) = ' '
                 else
                    casename(last:last) = ' '
                 endif
              enddo

              last = lastch (casename)
              pfcfile = casename(1:last) // '.PFC'
              solfile = casename(1:last) // '.VOLTAGES'
              swifilen = casename(1:last) // '.SWI'

              status = open_file( lunpfc, pfcfile, 'F', 'W', ios)
              if (status .ne. 0) then
                 last = lastch (pfcfile)
                 write (*, 130) pfcfile(1:last)
  130            format (' * Error opening file ', A)
                 go to 9000
              endif

              write(*, 140)
  140	      format(' Use base solution (Y or N)? ')
              read (*, 150) query
  150         format (a)
              if (query .eq. 'y' .or. query .eq. 'Y') then
                 status = open_file (lunsol, solfile, 'F', 'W', ios)
                 if (status .ne. 0) then
                    last = lastch (solfile)
                    write (*, 130) solfile(1:last)
                    lunsol = 0
                 endif
              else
                 lunsol = 0
              endif

              if (lunswio .gt. 0 .or. lunsdi .gt. 0) then
                 status = open_file (lunswin, swifilen, 'F', 'W', ios)
                 if (status .ne. 0) then
                    last = lastch (swifilen)
                    write (*, 130) swifilen(1:last)
                    lunswin = 0
                 endif
              else
                 lunswin = 0
              endif

              tempc = casename
              casename = capital (tempc)
              write (lunpfc, 151) casename(1:8)
  151         format ('(POWERFLOW, CASE=', a, ')')
              write (lunpfc, 152) 
  152         format ('/ P_INPUTLIST,FULL ')
              write (lunpfc, 153) 
  153         format ('/ P_OUTPUTLIST,FULL ')
              write (lunpfc, 154) 
  154         format ('/ P_ANALYSIS_RPT,LEVEL=4 ')
              last = lastch (casename)
              write (lunpfc, 155) casename(1:last)
  155         format ('/ NEW_BASE, FILE = ', a, '.bse')
              write (lunpfc, 156) 
  156         format ('/ NETWORK_DATA, FILE = *')
              num_rec = 6
              do i = 1, num_sys
                 nb = subsystem(1,i)
                 call bcdbus (nb, text)
                 num_rec = num_rec + 1
                 write (lunpfc, '(a)') text
                 ncb = kbsdta(15,nb)
                 do while (ncb .gt. 0)
                    call bcdcbs (ncb, text)
                    ncb = bctbl_nxt(ncb)
                    num_rec = num_rec + 1
                    write (lunpfc, '(a)') text
                 enddo
                 if (kbsdta(1,nb) .eq. 11) then
                    kxd = busxdtptr(nb)
                    if (kxd .gt. 0) then
                       call bcdxdt (kxd, text)
                       num_rec = num_rec + 1
                       write (lunpfc, '(a)') text
                    endif
                 endif
                 ptr = kbsdta(16,nb)
                 do while (ptr .gt. 0)
                    do j = 1, num_sys
                       if (subsystem(1,j) .eq. ky(ptr) .and. 
     &                     j .gt. i) then
                          ltyp = brtype(ptr)
                          if (ltyp .ne. 1) then
                             call bcdbrn (ptr, text)
                             num_rec = num_rec + 1
                             write (lunpfc, '(a)') text
                          endif
                       endif
                    enddo
                    ptr = brnch_nxt(ptr)
                 enddo
              enddo
              if (mark_bus .eq. 0) then
              else if (pintot .eq. 0.0 .and. qintot .eq. 0.0) then
                 write (*, 248) bus(mark_bus), base(mark_bus)
  248            format (' > No cut system from marked bus ', a8, f6.1)
              else
c
c                Encode INF BUS bus record 
c
                 text = 'BS    INF BUS  '
                 text(15:19) = code (base(mark_bus), 4, 0)
                 text(58:61) = code (voltage, 4, 3)
                 num_rec = num_rec + 1
                 write (lunpfc, '(a)') text
c
c                Encode branch record
c
                 text(1:2) = 'L '
                 text(20:27) = bus(mark_bus)
                 text(28:) = code (base(mark_bus), 4, 0)
                 kt = inp2opt(mark_bus)
                 mt = inp2opt(kernel)
                 v1 = cmplx (e(kt), f(kt))
                 angref = atan2(f(mt), e(mt)) + angle / 57.2957795
                 v2 = cmplx (voltage*cos(angref), voltage*sin(angref))
                 pintot = pintot / bmva
                 qintot = qintot / bmva
                 a12 = cmplx (pintot, -qintot) / conjg(v1)
                 z12 = (v1 - v2) / a12
                 text(39:44) = code (real(z12), 6, 5)
                 text(45:50) = code (aimag(z12), 6, 5)
                 num_rec = num_rec + 1
                 write (lunpfc, '(a)') text
              endif
              if (lunsol .gt. 0) then
                 write (lunpfc, 157) 
  157            format ('/ SOLUTION')
                 last = lastch (solfile)
                 write (lunpfc, 158) solfile(1:last)
  158            format ('> LOAD_SOLUTION, VOLTAGES=POLAR,FILE=', a,
     &                   ', SOLUTION=HOTSTART')
                 num_rec = num_rec + 2
              endif
              write (lunpfc, 159) 
  159         format ('( STOP )')
              num_rec = num_rec + 1

              if (lunsol .gt. 0 .and. mark_bus .gt. 0) then
                 do i = 1, num_sys
                    nb = subsystem(1,i)
                    kt = inp2opt(nb)
                    ang = atan2(f(kt), e(kt)) - angref
                    vmag = sqrt (e(kt) ** 2 + f(kt) ** 2)
                    text(1:) = 'B'
                    text(7:14) = bus(nb)
                    text(15:19) = code (base(nb), 4, 0)
                    write (text(20:32), '(f13.9)') vmag
                    write (text(34:46), '(f13.9)') ang
                    write (lunsol, '(a)') text
                 enddo
                 text = 'B     INF BUS  '
                 text(15:19) = code (base(mark_bus), 4, 0)
                 write (text(20:32), '(f13.9)') voltage
                 write (text(34:46), '(f13.9)') 0.0
                 write (lunsol, '(a)') text
                 num_sol = num_sol + 1
              endif
              numsdi = 0
              if (lunsdi .gt. 0 .and. mark_bus .gt. 0) then
                 write (lunswin,890) casename(1:8)
  890             format ('CASE ',a8,'      1                            
     &.6500.0300.0500.0400.3  ')
                 write (lunswin,892) casename
  892            format ('  CASE ',a8,'     SW 01 ')
                 text = 'BS    INF BUS  '
                 text(15:19) = code (base(mark_bus), 4, 0)
                 text(58:61) = code (voltage, 4, 3)
                 write (lunswin,893) bus(mark_bus)
  893            format ('  ',a8,' ON ISOLATED LOAD, RATED CONDITIONS.')
                 kt = inp2opt(mark_bus)
                 vmag = sqrt (e(kt) ** 2 + f(kt) ** 2)
                 write (lunswin,894) bus(mark_bus), pintot, qintot,
     &                  vmag
  894            format ('  ',a8,' - INF BUS:P(Q)=',f5.0,'(',f7.2,') @',
     &                   f4.2,'PU')
                 write (lunswin,895) 
  895            format ('  APPLY SHORT CKT TO CHECK EXCITER AND PSS RES
     &PONSE RATIO.')
                 write (lunswin,896) 
  896            format ('  ORIGINAL MACHINE MODEL ')
                 basekvc = code (base(mark_bus), 4, 0)
                 write (lunswin,897) bus(mark_bus), basekvc, basekvc
  897            format ('LS  ',a8,a4, '  INF BUS ', a4,'     -1  0.0'
     &)
                 write (lunswin,898) bus(mark_bus), basekvc, basekvc
  898            format ('LS  ',a8,a4, '  INF BUS ', a4,'      1  12.0
     &')
                 write (lunswin,899) bus(mark_bus), basekvc, basekvc
  899            format ('LS  ',a8,a4, '  INF BUS ', a4,'     -1  13.0
     &')
                 write (lunswin,900) basekvc
  900            format ('LA INF BUS ', a4,'                  1.0  1.0'
     &)                       
                 write (lunswin,901) 'INF BUS ', basekvc
  901            format ('LN ', a, a4)
                 write (lunswin,901) bus(mark_bus), basekvc
                 num_swi = num_swi + 12

                 maxdata = 6000
                 call load_sdi (lunsdi, sdifile, maxdata, numdata, 
     &                          record)
                 do i = 1, numdata
                    if (index ('MEFGTSLU', record(i)(1:1)) .ne. 0) then
                       bus_name = record(i)(4:11)
                       read (record(i)(12:15), '(f4.0)') base_kv
                       nb = find_bus (bus_name, base_kv)
                       if (nb .gt. 0) then
                          if (flag(nb) .gt. 0) then
                             ix = index ('MEFGTSU', record(i)(1:1))
                             if (ix .ne. 0) subsystem(2,flag(nb)) = 1
                             numsdi = numsdi + 1
                             sdidata(numsdi) = record(i)
                          endif
                       endif
                    endif
                 enddo
              endif
              if (lunswio .gt. 0 .and. mark_bus .gt. 0) then
                 do while (.true.)
                    read (lunswio, end=250, fmt='(a)'), text
                    text(3:3) = ' '
                    if (index ('MEFGTSLU', text(1:1)) .ne. 0 .and.
     &                  text(1:2) .ne. 'LS') then
                       bus_name = text(4:11)
                       read (text(12:15), '(f4.0)') base_kv
                       nb = find_bus (bus_name, base_kv)
                       if (nb .gt. 0) then
                          if (flag(nb) .gt. 0) then
                             ix = index ('MEFGTSU', text(1:1))
                             if (ix .ne. 0) subsystem(2,flag(nb)) = 1
                             subsystem(2,flag(nb)) = 1
                             k = 1
                             found = .false.
                             do while (k .le. numsdi .and.
     &                                  .not. found)
                                if (text(1:16) .eq. sdidata(k)(1:16)) 
     &                             then
                                   sdidata(k) = text
                                   found = .true.
                                else
                                   k = k + 1
                                endif
                             enddo
                             if (.not. found) then
                                numsdi = numsdi + 1
                                sdidata(numsdi) = text
                             endif
                          else
                             j = j + 1
                          endif
                       endif
                    else if (text(1:2) .eq. '90') then
                       go to 250
                    endif
                 enddo
  250            continue
              endif
              if (subsystem(2,1) .eq. 0) then
                 nb = subsystem(1,1)
                 write (*, 260) bus(kernel), base(kernel)
  260            format (' * Warning - no machine data found for bus ',
     &                   a8, f6.1)
              endif
              if (numsdi .gt. 0 .and. mark_bus .gt. 0) then
                 do i = 1, numsdi
                    write (lunswin, '(a)') sdidata(i)
                    num_swi = num_swi + 1
                 enddo
              endif
              if (mark_bus .gt. 0) then
                 write (lunswin,902)
  902            format ('FF      2.0 0600.')
                 write (lunswin,903) 
  903            format ('90')
                 write (lunswin,904)
  904            format ('MH  6       1.2        0.8        40.0       0
     &90.   600   ')
                 write (lunswin,905)
  905            format ('BH')
                 num_swi = num_swi + 4
                 do i = 1, num_sys
                    nb = subsystem(1,i)
                    basekvc = code (base(nb), 4, 0)
                    write (lunswin,906) bus(nb), basekvc
  906               format ('B  ', a8, a, '  4  4')
                    num_swi = num_swi + 1
                 enddo
                 write (lunswin,908)
  908            format ('GH')
                 num_swi = num_swi + 1
                 do i = 1, numsdi
                    if (sdidata(i)(1:2) .eq. 'MC' .or.
     &                  sdidata(i)(1:2) .eq. 'MF') then
                       write (lunswin,909) sdidata(i)(4:15), 
     &                    sdidata(i)(16:16)
  909                  format ('G  ', a12, 1x, a1, 
     &                    '  4  4  4  4  4  4  4  4  4  4  4  4    ')                                               
                       num_swi = num_swi + 1
                    endif
                 enddo
                 write (lunswin,911)
  911            format ('99     1')  
                 num_swi = num_swi + 1
              endif
C
           else
              write (*, 300) bus_name, base_kv
  300         format (' Bus ', A8, F6.1, ' is not in system ')
              j1 = 1
              j2 = ntot_alf
              found = .false.
              do while (j1 .le. j2 .and. .not. found)
                 i = (j1 + j2) / 2
                 j = alf2inp(i)
                 komp = kompr (bus(j), bus_name, junk) 
                 if (komp .eq. 0) komp = 100.0 * (base(j) - base_kv)
                 if (komp .lt. 0) then
                    j1 = i + 1
                 else if (komp .gt. 0) then
                    j2 = i - 1
                 else
                    found = .true.
                   j1 = i
                   j2 = i
                endif
              enddo
              ierr = 1
              do i = min(j1,j2)-1, max(j1,j2)+1
                 if (i .gt. 0 .and. i .le. ntot_alf) then
                    j = alf2inp(i)
                    ierr = ierr + 1
                    write (*, 142) bus(j),base(j)
  142               format (' Adjacent bus names > ', a8, f6.1)
                 endif
              enddo
           endif
           go to 310

  220      write (*, 224) 
  224      format (' *** Illegal data entry ***')
   
  310      continue
        enddo

 9000   continue

        if (pfcfile .ne. ' ') then
           last = lastch (pfcfile)
           write (*, 170) num_rec, pfcfile(1:last)
  170      format (' * ', i4, ' records written to file ', a)
           close (unit=lunpfc)
        endif
        if (num_sol .gt. 0) then
           last = lastch (solfile)
           write (*, 170) num_sol, solfile(1:last)
           close (unit=lunsol)
        endif
        if (num_swi .gt. 0) then
           last = lastch (swifilen)
           write (*, 170) num_swi, swifilen(1:last)
           close (unit=lunswin)
        endif

        return
        end
