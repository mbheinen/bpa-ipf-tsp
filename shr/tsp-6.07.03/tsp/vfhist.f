C    %W% %G%
C=======================================================================
C     Subroutine VFHIST ( Bus Voltage and Frequency History Model )
C
C     This model scans bus voltage deviations and frequencies and 
C     reports the worst-case buses.
C
C     Variables:
C     IA     = type of report or data
C            1 - low voltage deviation
C            2 - low frequency
C            3 - low voltage
C            4 - high voltage deviation
C     IC     = counter for determining when to print heading
C     MXXBUS = maximum number of buses used throughout run
C     IS     = general index
C     IBUS   = bus number
C     IAR    = area number index
C     VALUE  = voltage deviation, frequency, or voltage for current bus
C     IBUSS(1,*)  = bus number
C     IBUSS(2,*)  = list of buses with lowest voltage deviation
C     IBUSS(3,*)  = list of buses with lowest frequency
C     IBUSS(4,*)  = list of buses with lowest voltage
C     IBUSS(5,*)  = list of buses with highest voltage
C     RBUSS(1,*)  = initial voltage
C     RBUSS(2,*)  = lowest voltage deviation
C     RBUSS(3,*)  = time of lowest voltage deviation
C     RBUSS(4,*)  = lowest frequency
C     RBUSS(5,*)  = time of lowest frequency
C     RBUSS(6,*)  = lowest voltage
C     RBUSS(7,*)  = time of lowest voltage
C     RBUSS(8,*)  = highest voltage deviation
C     RBUSS(9,*)  = time of highest voltage
C     PTRBUS(*)   = pointer to voltage/frequency DIP data
C     POINTER(*)  = pointer to next voltage/frequency DIP entity
C     IDIP(*)     = flag denoting type of DIP: 
C                 1 = low voltage (no-load), 
C                 2 = low voltage (load),
C                 3 = low frequency
C     DIP(1,*)    = starting time of violating threshold
C     DIP(2,*)    = ending (or current) time of violating threshold
C     DIP(3,*)    = minimum (or maximum) entity during dip
C     LASTPTR     = last vacated space of IDIP/DIP
C     NEXTPTR     = next available space for appending new entities 
C                   in IDIP/DIP
C         
C     VALMIN(1)   = minimum voltage deviation
C     VALMIN(2)   = minimum frequency
C     VALMIN(3)   = minimum voltage
C     VALMIN(4)   = maximum voltage
C
C     This routine is based upon a similar routine in PSS/E which was
c     extensively rewritten by BPA to accommdate voltage and frequency 
C     dips

      subroutine vfhist (mode, to)
      integer mode
      real to

      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/vfhistory.inc'
      include 'tspinc/newtab.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/out512.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/filter.inc'
      include 'tspinc/titles.inc'
      include 'tspinc/room.inc'
      include 'tspinc/wstequ.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/areanz.inc'
      include 'tspinc/fltim.inc' 
      include 'tspinc/wcom.inc' 

      logical finished, test1, test2, start_flag, debug, fault_on
      integer ptr, dipptr, diptyp, ibus, ib, ibr, find_bus, get_arno
      real    tlast
      character valuec*9, zn*2, own*3

      data tlast      / 0.0 /, 
     &     start_flag / .false. /, 
     &     debug      / .false. /

      save

      if (mxxbus .eq. 0 .and. mode .ne. 1) go to 900

      rintvl = max0 (nintrvl, 1)

      if (mode .eq. 0) then     ! debug

        do ib = 1, mxxbus       ! with caution
          ptr = ptrbus(ib)      ! search threaded list for IDIP entity
          do while (ptr .gt. 0) ! while in threaded list ...
            write (88, 10000) ib, ptr, idip(ptr), dip(1,ptr), dip(2,ptr)
10000       format (1x, 'DIP_POP, Bus = ', i4, ' ptr = ', i4, 
     &        ' IDIP() = ', i4, 2f8.3)
            dip_pop = dip_pop + 1
            ptr = pointer(ptr)  ! check for multiple entities for bus IB
          enddo
        enddo
        write (88, 10010) dip_pop
10010   format (1x, 'DIP_POP = ', i4)
        do diptyp = 1, 3        ! loop through DIPTYP entities
          do ii = 1, MAX_DIPS   ! loop through non-sorted 
c                                 DIP_POINT entities
            ib = idip_points(diptyp,ii)  ! IB is index to IBUSS/RBUSS
            if (ib .gt. 0) then
              ibus = ibuss(1,ib)
              write (88, 10020) diptyp, ii, ibus, newtbc(ibus),
     &          basekv(inwtb(ibus)),
     &          dip_points(diptyp,1,ii), 
     &          dip_points(diptyp,2,ii), 
     &          dip_points(diptyp,3,ii)
10020         format(' Dump of DIP_POIINTS ', 3i5, 1x, '[', a, f6.1, 
     &          ']', 3e12.5)
            endif
          enddo
        enddo

      endif

      if (mode .eq. 1) then               !initializaiton
c
c       Initialize vfhist with new valued loaded into common
c
        mxxbus = ntotd
        do i = 1, mxxbus
          nb = find_bus (newtbc(i), basekv(inwtb(i)))
c
c         Load base case voltages into eyr() + j eyi()
c
          if (nb .gt. 0) then
            call get_volt(nb, eyr(i), eyi(i), pload, qload, zn, own)
            if (ntotc .eq. 0) then
              iarea(i) = 1
            else
              iarea(i) = get_arno(nb)
            endif
            load_bus(i) = (abs(pload) .gt. 0.5 .or. abs(qload) .gt. 0.5)
            busowner(i) = own
          else
            iarea(i) = 1
            load_bus(i) = .false.
            busowner(i) = '***'
          endif
        enddo
        do i = 1, ntotc
          arcnam(i) = areanc(i)
        enddo
        do i = ntotc+1, MAX_AREA
          arcnam(i) = 'System '
        enddo

        dip_pop = 0

        do ii = 1, MAX_DIPS
          do jj = 1, 3
             idip_points(jj,ii) = 0       !initialize DIP points
             dip_points(1,jj,ii) = 0.0    !initialize DIP starting time
             dip_points(2,jj,ii) = 0.0    !initialize DIP ending time
             dip_points(3,jj,ii) = 0.0    !initialize DIP minimum value
          enddo
        enddo

        do ib = 1, mxxbus
          ibuss(1,ib) = ib      !bus number
          ptrbus(ib) = 0        !initialize bus pointer to DIP() 
          rbuss(1,ib) = sqrt (eyr(ib) ** 2 + eyi(ib) ** 2)
          rbuss(2,ib) = 0.5     !volt dev higher than anything expected
          rbuss(3,ib) = 0.0
          rbuss(4,ib) = 65.     !freq             "            "
          rbuss(5,ib) = 0.0
          rbuss(6,ib) = 2.      !low volt         "            "
          rbuss(7,ib) = 0.0
          rbuss(8,ib) = -0.5    !high volt lower  "            "   
          rbuss(9,ib) = 0.0
        enddo
c
c       Override nominal 0.0 voltage of faulted bus
c
        do i = 1, mxxbus
          if (rbuss(1,i) .eq. 0.0) rbuss(1,i) = 1.0
        enddo

        do ib = 1, MAX_POINTS   !for all buses... 
          pointer(ib) = ib + 1  !initialize next available space
        enddo
        nextptr = 1             !initialize current available space
        lastptr = MAX_POINTS    !initialize last available space 
        pointer(MAX_POINTS) = 0 !flag end of list with a zero

        nvdlor = min(nvdlor, mxxbus)
        nfqlor = min(nfqlor, mxxbus)
        nvllor = min(nvllor, mxxbus)
        nvdhir = min(nvdhir, mxxbus)
        nvdlof = sign(min(abs(nvdlof),mxxbus),nvdlof)
        nfqlof = sign(min(abs(nfqlof),mxxbus),nfqlof)
        nvllof = sign(min(abs(nvllof),mxxbus),nvllof)
        nvdhif = sign(min(abs(nvdhif),mxxbus),nvdhif)
                                !initial teminal voltage
      endif
c
c     network calculation, TIME > TSTART
c
      if (mode .eq. 2) then
        fault_on = .false.
        do i = 1, ifltkt                                            
          if (to .eq. fstrt(i) .and. to .eq. tlast) fault_on = .true.
          if (to .eq. fstop(i) .and. to .ne. tlast) fault_on = .true.
          if (to .gt. fstrt(i) .and. to .lt. fstop(i)+tarm) 
     &      fault_on = .true.
        enddo
      endif
      if (mode .eq. 2 .and. .not. fault_on) then 
c                               ! time step debug dump
        if (.not. start_flag) then
          start_flag = .true.
          tstart = to
          cpu_f = 0.0
          clk_time_f = 0.0
          page_flts_f = 0
        endif
c
c       Store new values for final report
c
        ib = 0                  !initialize bus index
        finished = .false.
        do while (ib .lt. mxxbus .and. .not. finished)
c                               !look at all buses for run report...
          ib = ib + 1           !increment bus index
          ibus = ibuss(1,ib)    !bus sequence number
          if (ibus .gt. 0) then
            do ia = 1,4
c
c             ia = 1            !low volt dev array index
c             ia = 2            !freq array index
c             ia = 3            !volt array index
c             ia = 4            !high volt dev array index
              value = get_value (ib, ia)
              if (ia .le. 3 .and. value .lt. rbuss(ia*2,ib)) then
                rbuss(ia*2  ,ib) = value      !store new low
                rbuss(ia*2+1,ib) = to         !and time
              else if (ia .eq. 4 .and. value .gt. rbuss(ia*2,ib)) then
                rbuss(ia*2  ,ib) = value      !store new high
                rbuss(ia*2+1,ib) = to         !and time
              endif

c             store-new-dip-value-for-final-report

              if (ia .eq. 1 .or. ia .eq. 2) then
                call proc_dip (ib, ia, value, to, dipptr, diptyp)
              endif
            enddo
          endif
        enddo
c
c       Produce run report at each "n'th" time step
c
        t1 = amod(tlast-tstart, rintvl)
        t2 = amod(to-tstart, rintvl)
        if ((t1 .gt. t2  .and. t2 .ge. 0.0) .or. 
     &      (t1 .lt. 0.0 .and. t2 .ge. 0.0) .or. 
     &      (t1 .eq. 0.0 .and. t2 .eq. 0.0)) then
c                               !'N'th time step
          do ib = 1, nvdlor
            ibuss(4,ib) = 0     !initialize low volt dev array of IBUS's
          enddo
          do ib = 1, nfqlor
            ibuss(5,ib) = 0     !initialize low freq array of IBUS's
          enddo
          do ib = 1, nvllor
            ibuss(6,ib) = 0     !initialize low volt array of IBUS's
          enddo
          do ib = 1, nvdhir
            ibuss(7,ib) = 0     !initialize high volt array of IBUS's
          enddo
          ibr = 1               !initialize bus report index
          do while (ibr .le. max(nvdlor, nfqlor, nvllor, nvdhir))
            valmin(1) = vdlo    !reset min volt dev
            valmin(2) = fqlo    !reset min freq
            valmin(3) = vllo    !reset min volt
            valmin(4) = vdhi    !reset max volt
            ib = 0                !initialize bus index
            do while (ib .lt. mxxbus) 
              ib = ib + 1                !increment bus index
              ibus = ibuss(1,ib)         !bus sequence number
              if (ibus .gt. 0) then
                do ia = 1, 4
c
c                 ia = 1        !low volt dev array index
c                 ia = 2        !freq array index
c                 ia = 3        !volt array index
c                 ia = 4        !high volt dev array index

                  value = get_value (ib, ia)

                  test1 = (ia .lt. 4 .and. value .le. valmin(ia))
                  test2 = (ia .eq. 4 .and. value .ge. valmin(ia))
                  if (test1 .or. test2) then
                    is = 1        !increment index
                    do while (is .le. ibr .and. 
     &                        ibuss(ia+3,is) .ne. ibus)
                      is = is + 1 !search for repeat
                    enddo
                    if (is .gt. ibr) then  !no repeat found
                      valmin(ia) = value   !new minimum/maximum
                      ibuss(ia+3,ibr) = ib !bus number with minimum/
c                                           maximum
                    endif
                  endif
                enddo
              endif
            enddo
            icount = ibuss(4,ibr) + ibuss(5,ibr) + ibuss(6,ibr)
     &             + ibuss(7,ibr)
            finished = (icount .eq. 0) !no more found
            ibr = ibr + 1       !increment index
          enddo
 
          if (ibuss(4,1) .ne. 0) then   !some low volt dev's to report
            call skipln (1)
            write (outbuf, 10030) to, nvdlor, 100.0*vdlo
10030       format(' (VFHIST) at time = ',f7.1,' up to ',i4,
     &        ' Buses with low voltage deviation below ', f7.1, '% ')
            call prtout (1)
            call skipln (2)
            write (outbuf, 10031) 'VDEV', 'VDEV'
10031       format('    X ----- BUS ----- X     ', a, 6x, 
     &             ' X ----- BUS ----- X     ', a)
            call prtout (1)
            ia = 1              !low volt dev array index

            finished = .false.
            ibr = 0             !initialize index
            do while (.not. finished)  !look at all low values...
              ibr = ibr + 1     !increment index
              if (ibr .gt. nvdlor) then    
                finished = .true.
              else
                ib = ibuss(ia+3,ibr)   !bus number
                if (ib .le. 0) then
                  finished = .true.    !no more to report
                else
                  ibus = ibuss(1,ib)
                  value = get_value(ib, ia)
                  if (abs(value) .lt. 10.0) then
                    write (valuec, '(f9.3)') value
                  else if (abs(value) .lt. 100.0) then
                    write (valuec, '(f9.1)') value
                  else 
                    write (valuec, '(e9.2)') value
                  endif
                  if (mod(ibr,2) .eq. 1) then

c                   first entry
                    write (outbuf(1:35), 10040) ib, newtbc(ibus), 
     &                basekv(inwtb(ibus)), valuec 
10040               format(1x, i5, ' [', a, f6.1, ']', a, 5x)
                  else 
c                   second entry
                    write (outbuf(36:70), 10040) ibus, newtbc(ibus),
     &                basekv(inwtb(ibus)), valuec
                    call prtout (1)
                  endif
                endif
              endif
            enddo

c           last entry
            if (mod(ibr,2) .eq. 0) call prtout (1)
          endif

          if (ibuss(5,1) .ne. 0) then   !some low frequencies to report
            call skipln (1)
            write (outbuf, 10050) to, nfqlor, fqlo
10050       format(' (VFHIST) at time = ', f7.1, ' up to ', i4,
     &        ' Buses with low frequency below ', f6.2,' Hz ')
            call prtout (1)
            call skipln (2)
            write (outbuf, 10051) 'FREQ', 'FREQ'
10051       format('    X ----- BUS ----- X     ', a, 6x, 
     &             ' X ----- BUS ----- X     ', a)
            call prtout (1)

            ia = 2              !freq array index
            finished = .false.
            ibr = 0             !initialize index
            do while (.not. finished)  !look at all low values...
              ibr = ibr + 1     !increment index
              if (ibr .gt. nfqlor) then    
                finished = .true.
              else
                ib = ibuss(ia+3,ibr)  !bus number
                if (ib .le. 0) then
                  finished = .true.   !no more to report
                else
                  ibus = ibuss(1,ib)
                  value = get_value(ib, ia)
                  if (abs(value) .lt. 10.0) then
                    write (valuec, '(f9.3)') value
                  else if (abs(value) .lt. 100.0) then
                    write (valuec, '(f9.1)') value
                  else
                    write (valuec, '(e9.2)') value
                  endif
                  if (mod(ibr,2) .eq. 1) then
c                   first entry
                    write (outbuf(1:35), 10040) ib, newtbc(ibus), 
     &                basekv(inwtb(ibus)), valuec
                  else
c                   second entry
                    write (outbuf(36:70), 10040) ibus, newtbc(ibus), 
     &                basekv(inwtb(ibus)), valuec
                    call prtout (1)
                  endif
                endif
              endif
            enddo
c           last entry
            if (mod(ibr,2) .eq. 0) call prtout (1)
          endif

          if (ibuss(6,1) .ne. 0) then   !some low voltages to report
            call skipln (1)
            write (outbuf, 10060) to, nvllor, 100.0*vllo
10060       format(' (VFHIST) at time = ', f7.1, ' up to ', i4,
     &        ' Buses with low voltages below ', f7.1,'% ')
            call prtout (1)
            call skipln (2)
            write (outbuf, 10061) 'VOLT', 'VOLT'
10061       format('    X ----- BUS ----- X     ', a, 6x, 
     &             ' X ----- BUS ----- X     ', a)
            call prtout (1)

            ia = 3              !low volt array index
            finished = .false.
            ibr = 0             !initialize index
            do while (.not. finished)  !look at all low values...
              ibr = ibr + 1      !increment index
              if (ibr .gt. nvllor) then    
                finished = .true.
              else
                ib = ibuss(ia+3,ibr)  !bus number
                if (ib .le. 0) then
                  finished = .true.   !no more to report
                else
                  ibus = ibuss(1,ib)
                  value = get_value(ib, ia)
                  if (abs(value) .lt. 10.0) then
                    write (valuec, '(f9.3)') value
                  else if (abs(value) .lt. 100.0) then
                    write (valuec, '(f9.1)') value
                  else
                    write (valuec, '(e9.2)') value
                  endif
                  if (mod(ibr,2) .eq. 1) then
c                   first entry
                    write (outbuf(1:35), 10040) ib, newtbc(ibus), 
     &                basekv(inwtb(ibus)), valuec 
                  else
c                   second entry
                    write (outbuf(36:70), 10040) ibus, newtbc(ibus), 
     &                basekv(inwtb(ibus)), valuec
                    call prtout (1)
                  endif
                endif
              endif
            enddo
c           last entry
            if (mod(ibr,2) .eq. 0) call prtout (1)
          endif

          if (ibuss(7,1) .ne. 0) then   !some high voltages to report
            call skipln (1)
            write (outbuf, 10070) to, nvdhir, 100.0*vdhi
10070       format(' (VFHIST) at time = ',f7.1,' up to ',i4,
     &        ' Buses with high voltage deviation above ', f7.1,'% ')
            call prtout (1)
            call skipln (2)
            write (outbuf, 10071) 'VDEV', 'VDEV'
10071       format('    X ----- BUS ----- X     ', a, 6x, 
     &             ' X ----- BUS ----- X     ', a)
            call prtout (1)

            ia = 4              !high volt devarray index
            finished = .false.
            ibr = 0             !initialize index
            do while (.not. finished)  !look at all low values...
              ibr = ibr + 1     !increment index
              if (ibr .gt. nvdhir) then    
                finished = .true.
              else
                ib = ibuss(ia+3,ibr)  !bus number
                if (ib .le. 0) then
                  finished = .true.   !no more to report
                else
                  ibus = ibuss(1,ib)
                  value = get_value(ib, ia)
                  if (abs(value) .lt. 10.0) then
                    write (valuec, '(f9.3)') value
                  else if (abs(value) .lt. 100.0) then
                    write (valuec, '(f9.1)') value
                  else
                    write (valuec, '(e9.2)') value
                  endif
                  if (mod(ibr,2) .eq. 1) then
c                   first entry
                    write (outbuf(1:35), 10040) ib, newtbc(ibus), 
     &                basekv(inwtb(ibus)), valuec
                  else
c                   second entry
                    write (outbuf(36:70), 10040) ibus, newtbc(ibus),
     &                basekv(inwtb(ibus)), valuec
                    call prtout (1)
                  endif
                endif
              endif
            enddo
c           last entry
            if (mod(ibr,2) .eq. 0) call prtout (1)
          endif

        endif

      endif
      if (mode .eq. 2) tlast = to
      if (mode .eq. 3 .and. iflagr .le. 0) then

c       write final report

        iflagr = 1
        if (ibcode .eq. 0) outbuf(1:28) = '      NO BUSES OMITTED    '
        if (ibcode .eq. 1) outbuf(1:28) = '    DUMMY BUSES OMITTED   '
        if (ibcode .eq. 2) outbuf(1:28) = '     PQ=0 BUSES OMITTED   '
        if (ibcode .eq. 3) outbuf(1:28) = 'PQ=0 AND DUMMY BUSES OMITTED'

        call forbtm ()
        call fortop ()
        call skipln(2)
        write (outbuf, 10080)
10080   format(' *******************************************************
     &***************')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10081) 
10081   format(' ****', t68,'****')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10082)
10082   format(' ****',28x,'VFHIST',t68,'****')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10081)
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10083)
10083   format(' ****',7x, 'GLOBAL BUS VOLTAGE AND FREQUENCY',
     &    ' SCANNING REPORT',t68,'****')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10081)
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10084) tstart, tlast
10084   format(' ****', 12x, 'TIME INTERVAL = ', f7.1, ' TO ', f7.1,
     &    ' CYCLES', t68, '****')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10081)
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10081) 
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10086) 
10086   format(' **** BASE CASE TITLE:', t68, '****')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10087) pfcase
10087   format(' **** ', a, t68, '****')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10087) swcase
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10081)
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        write (outbuf, 10088)
10088   format(' *******************************************************
     &***************')
        call prtout (1)
        iwckt = iwckt + 1                                                 
        wrscom(iwckt) = outbuf(1:80)
        
        valmin(1) = vdlo        !RESET MIN VOLT DEV
        valmin(2) = fqlo        !RESET MIN FREQ
        valmin(3) = vllo        !RESET MIN VOLT
        valmin(4) = vdhi        !RESET MAX VOLT DEV
C
C       DIPTYP = 1 : No-load bus voltage dip
C                2 : Load bus voltage dip
C                3 : Frequency dip
        
        diptyp = 1              ! Retrieve longest no-load bus 
c                                   voltage dips
C
C       Prior to writing the DIP summary, process all active DIP 
C       entities of type DIPTYP.  For each active entity found,
C 
C       1. Find smallest Delta-t entity in DIP_TIME(DIPTYP,*) entities
C          (LOWJJ).
C       2. Overwrite that entity with the current active DIP entity 
C          (PTR),
C
c       Update-min-dip-entities ! Extract any min dip values still 
c                                   active
        call updt_dip (diptyp)
c       sort-dip-report-buses   ! Sort elapsed-time high-to-low
        call sort_dip (diptyp)

        if (idip_points(diptyp,1) .ne. 0) then
c                               ! At least one entity must be present
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (3)
          write (outbuf, 10090) 100.0 * vdipnl
10090     format(' ------ No-load bus relative voltage dip below ', 
     &       f7.1, '%', ' For entire system -------------')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10091) 
10091     format(
     &    '        Bus  name             start    end   elapsed   ',
     &    ' VLO%     area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10092) 
10092     format(
     &    '                               cycles  cycles  cycles ')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)
          ii = 1
          finished = .false.
          do while (ii .le. MAX_DIPS .and. .not. finished)
            jj = sort_index(ii)  
            ib = idip_points(diptyp,jj)  ! index stored points to 
c                                          IBUSS/RBUSS
            if (ib .eq. 0) then
              finished = .true.
            else
              ibus = ibuss(1,ib)
              rtemp2 = 100.0 * (1.0 + dip_points(diptyp,3,jj))
              rtemp3 = dip_points(diptyp,2,jj) 
     &               - dip_points(diptyp,1,jj)
              write (outbuf, 10100) busowner(ibus), newtbc(ibus), 
     &          basekv(inwtb(ibus)), 
     &          dip_points(diptyp,1,jj),
     &          dip_points(diptyp,2,jj),
     &          rtemp3, 
     &          rtemp2, iarea(ibus), arcnam(iarea(ibus))
10100         format(8x, a3, 2x, '[', a, f6.1, ']', 3f8.2, f8.2, 2x, 
     &          i2, 1x, a)
              call prtout (1)
              iwckt = iwckt + 1                                                 
              wrscom(iwckt) = outbuf(1:80)
              ii = ii + 1
            endif
          enddo
        endif
 
        diptyp = 2              ! Retrieve longest load bus voltage 
c                                 dips
c       Update-min-dip-entities ! Extract any min dip values still 
c                                 active
        call updt_dip (diptyp)
c       sort-dip-report-buses   ! Sort elapsed-time high-to-low
        call sort_dip (diptyp)
 
        if (idip_points(diptyp,1) .ne. 0) then ! At least one entity 
c                                                must be present
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (3)
          write (outbuf, 10110) 100.0 * vdipld
10110     format(
     &    ' ------ Load bus relative voltage dip below ', f7.1, '%',
     &    ' for entire system -------')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10111)
10111     format(
     &    '        Bus  name             start    end   elapsed   ',
     &    ' vlo%     area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10112)
10112     format(
     &    '                               cycles  cycles  cycles   ')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)

          ii = 1
          finished = .false.
          do while (ii .le. MAX_DIPS .and. .not. finished)
            jj = sort_index(ii)
            ib = idip_points(diptyp,jj)  ! index stored points to 
c                                          IBUSS/RBUSS
            if (ib .eq. 0) then
              finished = .true.
            else
              ibus = ibuss(1,ib)
              rtemp2 = 100.0 * (1.0 + dip_points(diptyp,3,jj))  
              rtemp3 = dip_points(diptyp,2,jj)
     &               - dip_points(diptyp,1,jj)
              write (outbuf, 10120) busowner(ibus), newtbc(ibus), 
     &          basekv(inwtb(ibus)),
     &          dip_points(diptyp,1,jj),
     &          dip_points(diptyp,2,jj),
     &          rtemp3, 
     &          rtemp2, iarea(ibus), arcnam(iarea(ibus))
10120         format(8x, a3, 2x, '[', a, f6.1, ']', 3f8.2, f8.2, 2x, 
     &          i2, 1x, a)
              call prtout (1)
              iwckt = iwckt + 1                                                 
              wrscom(iwckt) = outbuf(1:80)
              ii = ii + 1
            endif
          enddo
        endif
            
        diptyp = 3              ! Retrieve longest frequency dips
c       Update-min-dip-entities ! Extract any min dip values still 
c                                 active
        call updt_dip (diptyp)
c       sort-dip-report-buses   ! Sort elapsed-time high-to-low
        call sort_dip (diptyp)

        if (idip_points(diptyp,1) .ne. 0) then  ! At least one entity 
c                                                 must be present
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (3)
          write (outbuf, 10130) frqdip
10130     format(
     &    ' ------ Load bus frequency dip below ', f6.2, ' Hz',
     &    ' for entire system ------------')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10131) 
10131     format(
     &    '        Bus  name             start    end   elapsed   ',
     &    ' freq     area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10132)
10132     format(
     &    '                               cycles  cycles  cycles   ',
     &    ' (Hz) ')                   
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)
          ii = 1
          finished = .false.
          do while (ii .le. MAX_DIPS .and. .not. finished)
            jj = sort_index(ii)
            ib = idip_points(diptyp,jj)  ! index stored points to 
c                                          IBUSS/RBUSS
            if (ib .eq. 0) then
              finished = .true.
            else
              ibus = ibuss(1,ib)
              rtemp3 = dip_points(diptyp,2,jj) 
     &               - dip_points(diptyp,1,jj)
              write (outbuf, 10140) busowner(ibus), newtbc(ibus), 
     &          basekv(inwtb(ibus)),
     &          dip_points(diptyp,1,jj),
     &          dip_points(diptyp,2,jj),
     &          rtemp3, 
     &          dip_points(diptyp,3,jj), 
     &          iarea(ibus), arcnam(iarea(ibus))
10140         format(8x, a3, 2x, '[', a, f6.1, ']', 3f8.2, f8.2, 2x, 
     &          i2, 1x, a)
              call prtout (1)
              iwckt = iwckt + 1                                                 
               wrscom(iwckt) = outbuf(1:80)
              ii = ii + 1
            endif
          enddo
        endif
            
        ia = 1                  !low volt dev array index
        if (nvdlof .ne. 0) then
          call forbtm ()
          call fortop ()
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)
          write (outbuf, 10150) '----------', '----------'
10150     format(1x, a, 
     &      ' Low relative voltage % summary for entire system  ', a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          if (nvdlof .gt. 0) then
            write (outbuf, 10160) nvdlof, 'VDEV', vdlo, 'PU' 
10160       format(' ----------',
     &        6x, 'Up to ', i4, ' buses with ', a, ' < ', f5.2, 1x, 
     &        a, 7x, '----------')
            call prtout (1)
          else
            write (outbuf, 10170) -nvdlof
10170       format(' ----------',20x,i4,' buses',21x,'----------')
            call prtout (1)
          endif
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10171) 
10171     format('        Bus   name               %VLO %voltage    cycl
     &e    area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10172) 
10172     format('                                     deviation')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         sort-final-report-buses
          call sort_frpt(ia, nvdlof)

          call forbtm ()
          call fortop ()
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)
          write (outbuf, 10180) '----------', '----------'
10180     format(1x, a, 
     &    '      Low relative voltage % summary by area       ', a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10181) 
10181     format('        Bus   name               %vlo %voltage    cycl
     &e    area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10182) 
10182     format('                                     deviation')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         find-worst-bus-by-area
          call area_frpt(ia)
        endif
        
        ia = 2                  !freq array index
        if (nfqlof .ne. 0) then
          call forbtm ()
          call fortop ()
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)
          write (outbuf, 10190) '----------', '----------'
10190     format(1x, a, 
     &      '     Low frequency summary for entire system      ', a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          if (nfqlof .gt. 0) then
            write (outbuf, 10160) nfqlof, 'FREQ', fqlo, 'HZ'
            call prtout (1)
          else
            write (outbuf, 10170) -nfqlof
            call prtout (1)
          endif
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10191) 
10191     format('        Bus   name               freq    cycle    area
     &')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         sort-final-report-buses
          call sort_frpt(ia, nfqlof)

          call forbtm ()
          call fortop ()
          call skipln (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          write (outbuf, 10200) '----------', '----------'
10200     format(1x, a, 
     &      '          Low frequency summary by area           ', a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10201) 
10201     format('        Bus   name               freq    cycle    area
     &')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         find-worst-bus-by-area
          call area_frpt(ia)
        endif

        ia = 3                  !low volt array index
        if (nvllof .ne. 0) then
          call forbtm ()
          call fortop ()
          call skipln (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          write (outbuf, 10210) '----------', '----------'
10210     format(1x, a,
     &     '      Low voltage summary for entire system       ', a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          if (nvllof .gt. 0) then
            write (outbuf, 10160) nvllof, 'VOLT', vllo, 'PU'
            call prtout (1)
          else
            write (outbuf, 10170) -nvllof
            call prtout (1)
          endif
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         sort-final-report-buses

          write (outbuf, 10211) 
10211     format(10x,'Bus   name   baskv    volt     cycle   area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          call sort_frpt(ia, nvllof)

          call forbtm ()
          call fortop ()
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)
          write (outbuf, 10220) '----------', '----------'
10220     format(1x, a,
     &      '           Low voltage summary by area            ', a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         find-worst-bus-by-area
          write (outbuf, 10221) 
10221     format(10x,'Bus   name   baskv    volt     cycle    area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          call area_frpt(ia)
        endif
        
        ia = 4                  !high volt dev array index
        if (nvdhif .ne. 0) then
          call forbtm ()
          call fortop ()
          call skipln (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          write (outbuf, 10230) '----------', '----------'
10230     format(1x, a,
     &      '      High voltage deviation summary for entire system ', 
     &      a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          if (nvdhif .gt. 0) then
            write (outbuf, 10160) nvdhif, 'VOLT', vdhi, 'PU'
            call prtout (1)
          else
            write (outbuf, 10170) -nvdhif
            call prtout (1)
          endif
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         sort-final-report-buses
          write (outbuf, 10231) 
10231     format(10x,'Bus   name   baskv    vhi%   voltage   cycles    a
     &rea')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          call sort_frpt(ia, nvdhif)
 
          call forbtm ()
          call fortop ()
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = ' '
          call skipln (1)
          write (outbuf, 10240) '----------', '----------'
10240     format(1x, a,
     &      '           High voltage deviation summary by area  ', a)
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
          write (outbuf, 10231) 
10241     format(10x,'Bus   name   baskv    vhi%   voltage   cycles     
     & area')
          call prtout (1)
          iwckt = iwckt + 1                                                 
          wrscom(iwckt) = outbuf(1:80)
c         find-worst-bus-by-area
          call area_frpt(ia)
        endif

      endif

  900 continue
      return
      end
