C****************************************************************
C
C       File: getsubsys2.f
C       Purpose: Routine to append a set of disconnected generators
c                to a common infinite bus.
C
C       Author: Walt Powell  Date: 13 Jan 1994
C                            Modified: 
C       Called by:
C
C****************************************************************
C
        subroutine getsubsys2 (lunsdi, lunswio) 
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

        parameter (MAXSUBSYTSEM = 1000)
        character bus_name*8, capital*12, text*120, word(20)*12, 
     &            NULL*1, LINEFEED*1, query*1, code * 6, ratec * 10,
     &            machine(6000)*80, sdidata(100)*80, basekvc * 4,
     &            casename * 12, ch*1, tempc * 12, pfcfile * 60, 
     &            solfile * 60, swifilen * 60, infbus1 * 12, 
     &            infbus2 * 12, netdat(1000) * 80, zc * 6, icode * 2,
     &            basekvc0 * 4
	integer find_bus, ptr, sub(2,MAXSUBSYTSEM), 
     &          whichend1, whichend2, flag(MAXBUS), status, open_file,
     &          xsub(2,100)
        logical finished, found
        complex v1, v2, a12, z12, flows(4,MAXSUBSYTSEM), stot, zloss
        real angref(100)
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

        NULL = char(0)
        LINEFEED = char(10)

        infbus1 = 'INFBUS00100.'
        num_sub = 0    ! Number of buses in subsytems associated with
                       ! kernel bus
        num_sys = 0    ! Number of separate subsystems, each with
                       ! their own kernel bus
        nun_net = 0
        num_swi = 0
        num_sol = 0
        num_pfc = 0
        num_sdi = 0
        
        lunpfc = 2
        lunswin = 13
        lunsol = 15

        pfcfile = ' '
        solfile = ' '
        swifilen = ' '

        write (*, 80)
   80   format (' > Enter case name : ')
        read (*, '(a)') casename
        if (casename .eq. ' ' .or. casename(1:1) .eq. NULL) go to 510

        i = 1
        last = len (casename)
        do while (i .le. lastch (casename))
           ch = casename(i:i)
           if ((ch .ge. 'a' .and. ch .le. 'z') .or.
     &         (ch .ge. 'A' .and. ch .le. 'Z') .or.
     &         (ch .ge. '0' .and. ch .le. '9')) then
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
  130      format (' * Error opening file ', A)
           go to 510
        endif

        write(*, 140)
  140	format(' Use base solution (Y or N)? ')
        read (*, 150) query
  150   format (a)
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

        write (*, 152) 
  152   format (' > Enter INF BUS voltage (p.u.) and relative angle (deg
     &rees) : ')
        read (*, *) voltage, angle

        if (lunsdi .gt. 0) then
           maxdata = 6000
           call load_sdi (lunsdi, sdifile, maxdata, num_mac, machine)
        endif

        tempc = casename
        casename = capital (tempc)
        write (lunpfc, 160) casename(1:8)
  160   format ('(POWERFLOW, CASE=', a, ')')
        write (lunpfc, 170) 
  170   format ('/ P_INPUTLIST,FULL ')
        write (lunpfc, 180) 
  180   format ('/ P_OUTPUTLIST,FULL ')
        write (lunpfc, 190) 
  190   format ('/ P_ANALYSIS_RPT,LEVEL=4 ')
        last = lastch (casename)
        write (lunpfc, 200) casename(1:last)
  200   format ('/ NEW_BASE, FILE = ', a, '.BSE')
        write (lunpfc, 210) 
  210   format ('/ NETWORK_DATA, FILE = *')
        num_pfc = 6

        write (lunswin, 220) casename(1:8)
  220   format ('CASE ',a8,'      1                            .6500.030
     &0.0500.0400.3  ')
        write (lunswin, 230) casename
  230   format ('  CASE ',a8,'     SW 01 ')
        write (lunswin, 240) 
  240   format ('  APPLY SHORT CKT TO CHECK EXCITER AND PSS RESPONSE RAT
     &IO.')
        write (lunswin, 250) 
  250   format ('  ORIGINAL MACHINE MODEL ')
        num_swi = num_swi + 4
C
C       Main loop "num_sys": Enter kernel bus
C
  280   continue
        do while (.true.)
           last = 1
           write (*,290) num_sys+1
  290      format (' > Enter machine for subsystem ', i2, 
     &        ' (busname, KV) : ',$)
           read (*, 300) text
  300      format (a)
           bus_name = ' '
           base_kv = 0.0
           busback = 0
           if (text .eq. ' ') go to 510
c
C          Parse TEXT into busname, kv, and buses-back
c
           call parse_bus (text, word, nwrd)
           do i = nwrd+1, 2
              word(i) = ' '
           enddo
           bus_name = capital(word(1))
           if (index(word(2), '.') .eq. 0) then
              last = lastch(word(2))
              word(2) = word(2)(1:last) // '.0'
           endif
           read (word(2), 310, err = 480) base_kv
  310      format (f6.0)

           if (bus_name .eq. ' ') go to 510
           nb = find_bus (bus_name, base_kv)
   
           if (nb .gt. 0) then

              do i = 1, num_sub
                 if (nb .eq. sub(1,i)) then
                    write (*, 312) bus_name, base_kv
  312               format (' * Generator bus ', a8, f6.1, 
     &                      ' is already selected ')
                    go to 280
                 endif
              enddo

              kernel = nb
              num_sys = num_sys + 1
              num_sub = num_sub + 1
              xsub(1,num_sys) = num_sub 
              sub(1,num_sub) = nb
              sub(2,num_sub) = 0
c
c             Add INFBUSxx to powerflow data base
c
              infbus2 = infbus1
              infbus2(7:8) = icode (num_sys, 2)
              bus(ntot+num_sys) = infbus2(1:8)
              read (infbus2(9:12), '(bz, f4.0)') base(ntot+num_sys)
              inp2opt(ntot+num_sys) = ntot + num_sys
              e(ntot+num_sys) = voltage
              f(ntot+num_sys) = 0.0
              ptr = kbsdta(16,nb)
              oldk2 = 0
              do while (ptr .gt. 0)
                 if (ky(ptr) .ne. oldk2) then
                    num_sub = num_sub + 1
                    sub(1,num_sub) = ky(ptr)
                    sub(2,num_sub) = 0
                    ptr = brnch_nxt(ptr)
                 endif
              enddo
              xsub(2,num_sys) = num_sub 
              if (num_sub .eq. xsub(1,num_sys)) then
                 nb = sub(1,num_sub)
                 write (*, 330) 
  330            format (' * Error - subsystem has only one bus')
                 go to 500
              endif
              num = xsub(2,num_sys) - xsub(1,num_sys) + 1
              write (*, 340) num
  340         format (/, ' * Subsystem has ', i2, ' buses ')
              do i = 1, ntot
                 flag(i) = 0
              enddo
              do i = xsub(1,num_sys), xsub(2,num_sys)
                 nb = sub(1,i)
                 flag(nb) = i
              enddo
c
c             Compute flows from adjacent bus to cut system
c
              do i = xsub(1,num_sys)+1, xsub(2,num_sys)
                 flows(1,i) = cmplx (0.0, 0.0)
                 flows(2,i) = cmplx (0.0, 0.0)
                 flows(3,i) = cmplx (0.0, 0.0)
                 flows(4,i) = cmplx (0.0, 0.0)
                 nb = sub(1,i)
c
c                Compute flow to cut system
c
                 ptr = kbsdta(16,nb)
                 do while (ptr .gt. 0)
                    do j = xsub(1,num_sys), xsub(2,num_sys)
                       if (sub(1,j) .eq. ky(ptr)) go to 360
                    enddo
                    ltyp = brtype(ptr)
                    if (ltyp .eq. 1) then
                       call gtlfq (ptr, pin, qin, ploss, qloss, ovld, 
     &                             ratec, actual_amps, whichend1, 
     &                             actual_mva, whichend2)
                       flows(1,i) = flows(1,i) + cmplx (pin, qin)
                       flows(2,i) = flows(2,i) 
     &                            + cmplx (pin-ploss, qin-qloss)
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
                       flows(1,i) = flows(1,i) + cmplx (pin, qin)
                       flows(2,i) = flows(2,i) 
     &                            + cmplx (pin-ploss, qin-qloss)
                    endif
  360               ptr = brnch_nxt(ptr)
                 enddo
              enddo
c
c             Write powerflow data to netdat(), beginning with INFBUSxx 
c             bus record 
c
              text = 'BE    ' // infbus2
              text(19:20) = icode (num_sys, 2)
              text(39:42) = code (9999.0, 4, 0)
              text(43:47) = code (1.0, 5, 0)
              text(48:52) = code (9999.0, 5, 0)
              text(53:57) = code (-9999.0, 5, 0)
              text(58:61) = code (voltage, 4, 3)
              num_net = num_net + 1
              netdat(num_net) = text
              inf_indx = num_net         ! save for later reference

              do i = xsub(1,num_sys), xsub(2,num_sys)
                 nb = sub(1,i)
                 call bcdbus (nb, text)
c
c                Modify zone to coincide with num_sys
c
                 text(19:20) = icode (num_sys, 2)
c
c                Overwrite %vars on BG machines to 100%
c
                 if (text(1:2) .eq. 'BG') text(78:80) = '100'
                 num_net = num_net + 1
                 netdat(num_net) = text
                 ncb = kbsdta(15,nb)
                 do while (ncb .gt. 0)
                    call bcdcbs (ncb, text)
                    ncb = bctbl_nxt(ncb)
                    num_net = num_net + 1
                    netdat(num_net) = text
                 enddo
                 if (kbsdta(1,nb) .eq. 11) then
                    kxd = busxdtptr(nb)
                    if (kxd .gt. 0) then
                       call bcdxdt (kxd, text)
                       num_net = num_net + 1
                       netdat(num_net) = text
                    endif
                 endif
                 ptr = kbsdta(16,nb)
                 do while (ptr .gt. 0)
                    do j = xsub(1,num_sys), xsub(2,num_sys)
                       if (sub(1,j) .eq. ky(ptr) .and. 
     &                     j .gt. i) then
                          ltyp = brtype(ptr)
                          if (ltyp .eq. 5) then
                             call bcdbrn (ptr, text)
                             num_net = num_net + 1
                             netdat(num_net) = text
                             text(32:32) = 'Z'
                             text(39:62) = ' '
                             text(45:50) = code (25.0, 6, 5)
                             num_net = num_net + 1
                             netdat(num_net) = text
                          else if (ltyp .ne. 1) then
                             call bcdbrn (ptr, text)
                             num_net = num_net + 1
                             netdat(num_net) = text
                          endif
                       endif
                    enddo
                    ptr = brnch_nxt(ptr)
                 enddo
              enddo
c
c             Append INF BUS bus and equivalent branch data to powerflow
c             data.
c
              do i = xsub(1,num_sys)+1, xsub(2,num_sys)
                 nb = sub(1,i)
                 if (cabs(flows(1,i)) .eq. 0.0) then
                    write (*, 370) bus(nb), base(nb)
  370               format (' * No flows from cut system at bus ', a8, 
     &                 f6.1)
                 else
c
c                   Add equivalent branch record
c
                    text(1:) = 'T '
                    text(7:18) = infbus2
                    text(20:27) = bus(nb)
                    text(28:31) = code (base(nb), 4, 0)
                    kt = inp2opt(nb)
                    mt = inp2opt(kernel)
                    v1 = cmplx (e(kt), f(kt))
                    angref(num_sys) = atan2(f(mt), e(mt)) 
     &                              + angle / 57.2957795
                    ek = voltage * cos(angref(num_sys))
                    fk = voltage * sin(angref(num_sys))
                    v2 = cmplx (ek, fk)
                    flows(1,i) = flows(1,i) / cmplx (bmva, 0.0)
                    flows(2,i) = flows(2,i) / cmplx (bmva, 0.0)
                    flows(3,i) = conjg (flows(1,i)) / conjg(v1)
                    flows(4,i) = (v1 - v2) / flows(3,i)
                    text(39:44) = code (real(flows(4,i)), 6, 5)
                    text(45:50) = code (aimag(flows(4,i)), 6, 5)
                    text(63:67) = code (100.0, 5, 2)
                    text(68:72) = code (base(nb), 5, 2)
                    num_net = num_net + 1
                    netdat(num_net) = text
c
c                   Add equivalent branch comments to TSP 
c
                    write (lunswin, 380) bus(nb)
  380               format ('  ', a8, ' ON ISOLATED LOAD, RATED CONDITIO
     &NS.')
                    kt = inp2opt(nb)
                    vmag = sqrt (e(kt) ** 2 + f(kt) ** 2)
                    stot = flows(1,i) * cmplx (bmva, 0.0)
                    write (lunswin, 390) bus(nb), stot, vmag
  390               format ('  ', a8, ' - INFBUS:P(Q)=', f6.0, '(', 
     &                  f7.0, ') @', f4.2, 'PU')
                    num_swi = num_swi + 2

                 endif
              enddo
c
c             Compute pgen + j qgen for infbus2 (with 1 + j1 MVA bias)
c
              stot = cmplx (0.01, 0.01)
              do i = xsub(1,num_sys)+1, xsub(2,num_sys)
                 zloss = flows(3,i) * conjg(flows(3,i)) * flows(4,i)
                 stot = stot + flows(1,i) - zloss
              enddo
              stot = stot * cmplx (bmva, 0.0)
              netdat(inf_indx)(21:25) = code (real (stot), 5, 0)
              netdat(inf_indx)(26:30) = code (aimag (stot), 5, 0)
c
c             Conditionally append machine data from lunsdi to TSP
c
              if (lunsdi .gt. 0) then

                 do i = 1, num_mac
                    if (index ('MEFGTSLU', machine(i)(1:1)) .ne. 0) then
                       bus_name = machine(i)(4:11)
                       read (machine(i)(12:15), '(f4.0)') base_kv
                       nb = find_bus (bus_name, base_kv)
                       if (nb .gt. 0) then
                          if (flag(nb) .gt. 0) then
                             ix = index ('MEFGTSU', machine(i)(1:1))
                             if (ix .ne. 0) sub(2,flag(nb)) = 1
                             num_sdi = num_sdi + 1
                             sdidata(num_sdi) = machine(i)
                          endif
                       endif
                    endif
                 enddo
              endif
c
c             Conditionally append machine data from lunswi to TSP.
c             If a similar record from lunsdi already has been added,
c             overwrite it with the ultimate record being form lunswi.
c
              if (lunswio .gt. 0) then
                 rewind lunswio
                 do while (.true.)
                    read (lunswio, end=440, fmt='(a)'), text
                    text(3:3) = ' '
                    if (index ('MEFGTSLU', text(1:1)) .ne. 0 .and.
     &                  text(1:2) .ne. 'LS') then
                       bus_name = text(4:11)
                       read (text(12:15), '(f4.0)') base_kv
                       nb = find_bus (bus_name, base_kv)
                       if (nb .gt. 0) then
                          if (flag(nb) .gt. 0) then
                             ix = index ('MEFGTSU', text(1:1))
                             if (ix .ne. 0) sub(2,flag(nb)) = 1
                             sub(2,flag(nb)) = 1
                             k = 1
                             found = .false.
                             do while (k .le. num_sdi .and.
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
                                num_sdi = num_sdi + 1
                                sdidata(num_sdi) = text
                             endif
                          else
                             j = j + 1
                          endif
                       endif
                    else if (text(1:2) .eq. '90') then
                       go to 440
                    endif
                 enddo
  440            continue
              endif
              isub = xsub(1,num_sys)
              if (sub(2,isub) .eq. 0) then
                 nb = sub(1,1)
                 write (*, 450) bus(kernel), base(kernel)
  450            format (' * Warning - no machine data found for bus ',
     &                   a8, f6.1)
              endif

           else
              write (*, 460) bus_name, base_kv
  460         format (' Bus ', A8, F6.1, ' is not in system ')
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
                    write (*, 470) bus(j),base(j)
  470               format (' * Adjacent bus names > ', a8, f6.1)
                 endif
              enddo
           endif
           go to 500

  480      write (*, 490) 
  490      format (' *** Illegal data entry ***')
   
  500      continue
        enddo

  510   continue
c
c       End of subsystem loop
c
        if (num_sys .eq. 1) then

           sub(1,num_sub+1) = ntot+1
           sub(2,num_sub+1) = 0
c
c          If only a single system is selected, search netdat() and
c          change infbus2 to type BS
c
           infbus2 = infbus1
           infbus2(7:8) = '01'
           finished = .false.
           i = 1
           do while (i .le. num_net .and. .not. finished)
              if (netdat(i)(1:1) .eq. 'B' .and. 
     &            netdat(i)(7:18) .eq. infbus2) then
                 netdat(i)(2:2) = 'S'
                 finished = .true.
              else
                 i = i + 1
              endif
           enddo
        else
c
c          Otherwise append the second subsystem to infbus1.
c
           do i = 1, num_sys
              sub(1,num_sub+i) = ntot+i
              sub(2,num_sub+i) = 0
           enddo
           sub(1,num_sub+num_sys+1) = ntot + num_sys + 1
           sub(2,num_sub+num_sys+1) = 0
           bus(ntot+num_sys+1) = infbus1(1:8)
           read (infbus1(9:12), '(bz, f4.0)') base(ntot+num_sys+1)
           inp2opt(ntot+num_sys+1) = ntot + num_sys + 1
           e(ntot+num_sys+1) = voltage
           f(ntot+num_sys+1) = 0.0
c
c          Pass 1 - write AC records
c
           text = '.====================================='
           write (lunpfc, '(a)') text
           num_pfc = num_pfc + 1
           do i = 0, num_sys
              infbus2 = infbus1
              infbus2(7:8) = icode (i, 2)
              write (text, 520) infbus2(7:8), infbus2, 0.0, infbus2(7:8)
  520         format ('A  SYSTEM ', a, t14, a, t27, f8.1, t36, a)
              write (lunpfc, '(a)') text
              num_pfc = num_pfc + 1
           enddo
c
c          Pass 2 - write INFBUS00 - INFBUSXX records
c
           text = '.====================================='
           write (lunpfc, '(a)') text
           num_pfc = num_pfc + 1

           text = 'BS    ' // infbus1
           text(19:20) = icode (0, 2)
           text(58:61) = code (voltage, 4, 3)
           write (lunpfc, '(a)') text
           num_pfc = num_pfc + 1

           do i = 1, num_sys
              infbus2 = infbus1
              infbus2(7:8) = icode (i, 2)
              zc = code (25.0, 6, 5)
              write (text, 530) infbus1, infbus2, zc
  530         format ('L', t7, a, t20, a, t45, a)
              write (lunpfc, '(a)') text
              num_pfc = num_pfc + 1
           enddo

        endif
c
c       Now write netdat() to lunpfc
c
        i = 1
        infbus2 = infbus1
        infbus2(7:8) = icode (i, 2)
        do j = 1, num_net
           if (netdat(j)(1:1) .eq. 'B' .and. 
     &         netdat(j)(7:18) .eq. infbus2) then
              text = '.====================================='
              write (lunpfc, '(a)') text
              num_pfc = num_pfc + 1
              i = i + 1
              infbus2(7:8) = icode (i, 2)
           endif
           write (lunpfc, '(a)') netdat(j)
           num_pfc = num_pfc + 1
        enddo

        text = '.====================================='
        write (lunpfc, '(a)') text
        num_pfc = num_pfc + 1
c
c       Complete PFC commands
c
        if (lunsol .gt. 0) then
           write (lunpfc, 910) 
  910      format ('/ SOLUTION')
           last = lastch (solfile)
           write (lunpfc, 920) solfile(1:last)
  920      format ('> LOAD_SOLUTION, VOLTAGES=POLAR,FILE=', a,
     &               ', SOLUTION=HOTSTART')
           num_pfc = num_pfc + 2
        endif

        write (lunpfc, 930) 
  930   format ('( STOP )')
        num_pfc = num_pfc + 1
c
c       Conditionally add base solution data to lunsol
c
        if (lunsol .gt. 0) then
           do i = 1, num_sys
              js = xsub(2,i)
              if (i .eq. num_sys) then
                 if (num_sys .eq. 1) then
                    js = js + 1
                 else
                    js = js + num_sys + 1
                 endif
              endif
              do j = xsub(1,i), js   ! Note: this catches INFBUSxx
                 nb = sub(1,j)
                 kt = inp2opt(nb)
                 ang = atan2(f(kt), e(kt)) - angref(i)
                 if (i .eq. num_sys .and. j .gt. xsub(2,i)) ang = 0.0
                 vmag = sqrt (e(kt) ** 2 + f(kt) ** 2)
                 text(1:) = 'B'
                 text(7:14) = bus(nb)
                 text(15:19) = code (base(nb), 4, 0)
                 write (text(20:32), '(f13.9)') vmag
                 write (text(34:46), '(f13.9)') ang
                 write (lunsol, '(a)') text
                 num_sol = num_sol + 1
              enddo
           enddo
        endif
c
c       Add Line Switching data to TSP
c        
        infbus2 = infbus1
        infbus2(7:8) = icode (1, 2)
        i = xsub(1,1) + 1
        nb = sub(1,1) 
        basekvc = code (base(nb), 4, 0)
        write (lunswin, 400) bus(nb), basekvc, infbus2
  400   format ('LS  ',a8, a4, 2x, a, '     -1   0.0 ')
        num_swi = num_swi + 1

        do i = 1, num_sys
           nb0 = sub(1,xsub(1,i)) 
           basekvc0 = code (base(nb0), 4, 0)
           do j = xsub(1,i)+1, xsub(2,i)
              nb = sub(1,j) 
              basekvc = code (base(nb), 4, 0)
              write (lunswin, 410) bus(nb), basekvc, bus(nb0), basekvc0
  410         format ('LS -',a8, a4, 2x, a8, a4, '         12.0 ')
              write (lunswin, 420) bus(nb), basekvc, bus(nb0), basekvc0
  420         format ('LS  ',a8, a4, 2x, a8, a4, '         13.0 ')
              num_swi = num_swi + 2
           enddo
        enddo
c
c       Add LA data to TSP
c
        write (lunswin, 422) infbus1
  422   format ('LA ', a, '                  1.0  1.0 ')
        num_swi = num_swi + 1
c
c       Add Load Netting data to TSP
c
        write (lunswin, 426) infbus1
  426   format ('LN ', a)
        num_swi = num_swi + 1
c
c       Add Load Netting data to TSP
c        
        do i = 1, num_sys   ! This double loop skips generators
           infbus2 = infbus1
           infbus2(7:8) = icode (i, 2)
           write (lunswin, 428) infbus2
  428      format ('LN ', a)
           num_swi = num_swi + 1

           do j = xsub(1,i)+1, xsub(2,i)
              nb = sub(1,j)
              basekvc = code (base(nb), 4, 0)
              write (lunswin, 430) bus(nb), basekvc
  430         format ('LN ', a, a)
              num_swi = num_swi + 1
           enddo
        enddo
c
c       Append all Machine data
c
        do i = 1, num_sdi
          write (lunswin, '(a)') sdidata(i)
          num_swi = num_swi + 1
        enddo
c
c       Set up TSP plotting commands
c
        write (lunswin, 940)
  940   format ('FF      2.0 0600.')
        write (lunswin, 950) 
  950   format ('90')
        write (lunswin, 960)
  960   format ('MH  6       1.2        0.8        40.0       090.   600
     &   ')
        write (lunswin, 970)
  970   format ('BH')
        num_swi = num_swi + 4
c
c       Add INF BUS to "B" records
c
        if (num_sys .gt. 1) then
           nb = sub(1,num_sub+num_sys+1)
           basekvc = code (base(nb), 4, 0)
           write (lunswin, 980) bus(nb), basekvc
  980      format ('B  ', a8, a, '  4  4')
           num_swi = num_swi + 1
        endif

        do i = 1, num_sys   ! This double loop skips generators
           nb = sub(1,num_sub+i)
           basekvc = code (base(nb), 4, 0)
           write (lunswin, 980) bus(nb), basekvc
           num_swi = num_swi + 1
           do j = xsub(1,i)+1, xsub(2,i)
              nb = sub(1,j)
              basekvc = code (base(nb), 4, 0)
              write (lunswin, 980) bus(nb), basekvc
              num_swi = num_swi + 1
           enddo
        enddo

        write (lunswin, 990)
  990   format ('GH')
        num_swi = num_swi + 1
        do i = 1, num_sdi
           if (sdidata(i)(1:2) .eq. 'MC' .or.
     &         sdidata(i)(1:2) .eq. 'MF') then
              write (lunswin, 1000) sdidata(i)(4:15), sdidata(i)(16:16)
 1000         format ('G  ', a12, 1x, a1, 
     &                '  4  4  4  4  4  4  4  4  4  4  4  4    ') 
              num_swi = num_swi + 1
           endif
        enddo
        write (lunswin, 1010)
 1010   format ('99     1')  
        num_swi = num_swi + 1

        if (pfcfile .ne. ' ') then
           last = lastch (pfcfile)
           write (*, 1020) num_pfc, pfcfile(1:last)
 1020      format (' * ', i4, ' records written to file ', a)
           close (unit=lunpfc)
        endif
        if (num_sol .gt. 0) then
           last = lastch (solfile)
           write (*, 1020) num_sol, solfile(1:last)
           close (unit=lunsol)
        endif
        if (num_swi .gt. 0) then
           last = lastch (swifilen)
           write (*, 1020) num_swi, swifilen(1:last)
           close (unit=lunswin)
        endif

        return
        end
