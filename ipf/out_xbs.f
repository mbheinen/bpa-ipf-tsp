C    @(#)out_xbs.f	20.5 7/18/96
C****************************************************************
C
C   File: out_xbs.f
C   Purpose: Routine to obtain BCD image of OUTPUT X-bus data
C
C   Author: Walt Powell  Date: 14 April 1992
C                        Modified: 14 April 1992
C   Called by:
C
C****************************************************************
C
        subroutine out_xbs (nb, datarec)

        parameter (MAXDATAREC = 258)
        character datarec * (MAXDATAREC)
c
c       This subroutine returns WSCC-formated output data records.
c       Output parameter:
c
c       nb         - bus index
c       datarec - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnphs.inc'
        include 'ipfinc/tbx.inc'
        include 'ipfinc/tbxsrt.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/ordsta.inc'
 
      	common /xinit/ xinit(MAXXDT)
c
        double precision xinit
c
        dimension ustep(8), umvar(8), sstep(8), smvar(8)
        real match
c
c     *******************************************************
c
c       Set up pointers to X data (busxdtptr) and TBX data (ptrtbx)
c
        if (ordvlt .eq. 1) then
           kt = nb
        else
           kt = inp2opt(nb)
        endif
        if (.not. xdt_flag) then
           do i = 1, ntot
              busxdtptr(i)  = 0
           enddo
           do i = 1, kxtot
              kxd = xdata(1,i)
              if (kxd .gt. 0) then
                 if (ordtbx .eq. 2) kxd = opt2inp(kxd)
                 busxdtptr(kxd) = i
              endif
           enddo
           xdt_flag = .true.
        endif
                                                                    
        if (tbx_loaded .ne. ordtbx) then
           do i = 1, ntot
              ptrtbx(i)  = 0
           enddo

           do i = 1, ntotb
              kxd = tbx(2,i)
              if (kxd .gt. 0) then
                 if (ordtbx .eq. 2) kxd = opt2inp(kxd)
                 ptrtbx(kxd) = i
              endif
              if (kxd .gt. 0) ptrtbx(kxd) = i
           enddo
           tbx_loaded = ordtbx
        endif
C
C       If no BX buses exist, jump to bottom of subroutine.
C
C       NB = external(alphabetical) bus number
C       KT = internal(optimal) bus number
C

        kxd = busxdtptr(nb)
        if (kxd .eq. 0) then
           datarec = ' '
           go to 900
        endif
        call bcdxdt (kxd, datarec)
        dvdq = xsen(kxd)
        ntbx = ptrtbx(nb)
        if (ntbx .eq. 0) then
           write (errbuf(1), 100) bus(nb), base(nb)
  100      format (' OUT_XBS: BX bus ', a8, f7.1, 
     &        ' is not in TBX array ')
           call prterx ('W', 1)
           go to 900
        endif
        if (ltbxsl(1,ntbx) .gt. 0) then
          dvdq = tbxslp(5,ntbx)
        endif
C
C       TOTREK = total reactors availble (MVARS).
C       TOTCAP = total capacitors availble (MVARS).
C       USEREK = amount of reactors used (MVARS).
C       USECAP = amount of capacitors used (MVARS).
C
        vksq = e(kt)**2 + f(kt)**2
        totrek = xdata(3,kxd) * vksq
        totcap = xdata(4,kxd) * vksq
        userek = xdata(5,kxd) * vksq
        usecap = xdata(6,kxd) * vksq
C
C       ISW = 1, if reactors are used, or if no capacitors are
C       available and no reactors are used.
C       ISW = 2, if capacitors are used, or if no reactors are
C       available and no capacitors are used.
C       ISW = 0, if no capacitors or reactors are used.
C
        if ((userek .lt. 0.0 .and. totrek .le. userek) .or.
     1      (totrek .lt. 0.0 .and. totcap .eq. 0.0)) then
           isw = 1
        else if ((usecap .gt. 0.0 .and. totcap .ge. usecap) .or.
     1      (totrek .eq. 0.0 .and. totcap .gt. 0.0)) then
           isw = 2
        else
           isw = 0
        endif
C
C       Find index of first capacitor (ICAP), index of first reactor
C       (IREK), and total number of steps of capacitors and reactors
C       available (IS).
C
        icap = 0
        irek = 0
        do i = 1,8
          j = 2*i + 5
          n = xdata(j,kxd)
          if (n .eq. 0) go to 160
          if ((xdata(j+1,kxd) .gt. 0.0) .and. (icap .eq. 0)) icap = i
          if ((xdata(j+1,kxd) .lt. 0.0) .and. (irek .eq. 0)) irek = i
        enddo
        i = 9
  160   is = i - 1
        if (icap .eq. 0) icap = is + 1
        if (irek .eq. 0) irek = is + 1
C
C       Find Q-used and Q-scheduled.
C
        match = 0.0
        q2 = 0.0
        do 180 i = 1,is
          j = 2*i + 5
          n = xdata(j,kxd)
          qstep = xdata(j+1,kxd) * vksq
          ustep(i) = 0.0
          umvar(i) = 0.0
          sstep(i) = n
          smvar(i) = n * qstep
C
C         If Reactors are used .AND. QSTEP contains Capacitors, jump
C         to bottom of this loop and process the next Reactor/
C         Capacitor step.
C
          if ((isw .eq. 1) .and. (qstep .gt. 0.0)) go to 180
C
C         If Capacitors are used .AND. QSTEP contains Reactors, jump
C         to bottom of this loop and process the next Reactor/
C         Capacitor step.
C
          if ((isw .eq. 2) .and. (qstep .lt. 0.0)) go to 180
C
C         If no capacitors or reactors are used, jump to bottom of
C         this loop and process the next Reactor/Capacitor step.
C
          if (isw .eq. 0) go to 180
C
C         If capacitors are used in the solution and this QSTEP
C         contains capacitors, or if reactors are used and this QSTEP
C         contains reactors, determine number of steps and amount of
C         Q used.
C
          do 170 l = 1,n
C
C           Q1 = Previous value of Q
C           Q2 = Current value of Q
C
            q1 = q2
            q2 = q2 + qstep
C
C           If capacitors are used.
C
            if (isw .eq. 2) then
C
C             USTEP = Number of steps of capacitors used
C             UMVAR = Amount of capacitors used from current step
C
              if (usecap .gt. q1 .and. usecap .ge. q2) then
                ustep(i) = ustep(i) + 1.0
                umvar(i) = umvar(i) + qstep
                match = float(10*i + l)
              else if (usecap .ge. q1 .and. usecap .lt. q2) then
                ratio = (usecap - q1) / qstep + 0.001
                ustep(i) = ustep(i) + ratio
                umvar(i) = umvar(i) + ratio * qstep
                match = float(10*i + l - 1) + ratio
                go to 180
              else
                go to 180
              endif
C
C           If reactors are used.
C
            else
C
C             USTEP = Number of steps of reactors used
C             UMVAR = Amount of reactors used from current step
C
              if (userek .lt. q1 .and. userek .le. q2) then
                ustep(i) = ustep(i) + 1.0
                umvar(i) = umvar(i) + qstep
                match = float(10*i + l)
              else if (userek .le. q1 .and. userek .gt. q2) then
                ratio = (userek - q1) / qstep + 0.001
                ustep(i) = ustep(i) + ratio
                umvar(i) = umvar(i) + ratio * qstep
                match = float(10*i + l - 1) + ratio
                go to 180
              else
                go to 180
              endif
            endif
  170     continue
  180   continue
       
        write( datarec(21:), 190 )  (nint(sstep(i)), 
     1      nint(ustep(i)), smvar(i) / sstep(i), i = 1, is)
  190   format(8(2i1, e15.7))

  900   continue
	return
	end
