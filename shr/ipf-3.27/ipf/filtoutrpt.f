C    %W% %G%
c***********************************************************************
c
c     File: filtoutrpt.f
c
c     Purpose: Compute bus-branch output in WSCC (IPS) format.
c              Filtered by area, zone, etc.
c
c     Author: Walt Powell             Date: 7 December 1992
c
c     Modified: Merilyn George, 12/94
c          from busbrotrpt.f, to use std. filters instead of bus names
c
c     Called by: p_reports.f from the GUI
c
c***********************************************************************
c
      subroutine filtoutrpt (in_buffer, out_buffer, scrfil)
      character in_buffer *(*), out_buffer *(*)
      integer scrfil

      include 'ipfinc/parametr.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/sortuvov.inc'
      include 'ipfinc/pfstates.inc'

        character  temp_text * 120, ratec * 10, ratecx * 10, 
     &            header(6) * 80, null * 1,
     &            linefeed * 1, type * 2, bustyp * 2
        integer apdoutbuf, o2, ptr, oldptr, whichend1, whichend2,
     &            findstr, scrfilx, loop(3), lastloop(3)

        logical change_f, gtfltr, chkfltr, repeat, finished

        save

        header(1) = '  BUS NAME  VOLTS-PU  ...GENERATION.. ......LOAD' 
     &           // '..... .....SHUNT............ BUS'
        header(2) = '                                                '
     &           // '            AVAIL    USED  TYPE'
        header(3) = '    ACTUAL KV/ANGLE     MW    MVAR      MW    '
     &           // 'MVAR      MW    MVAR    MVAR'

        header(4) = ' ------------------ID TO BUS NAME     LINE FLOWS'
     &           // '       LINE LOSSES  ....LOADING'
        header(5) = '                                         MW    '
     &           // 'MVAR     MW    MVAR  PCT  ACTUAL'

c  There is a solved case, or p_reports would not have called this
c  Set the ostates flag as required for all the BPF data routines
        ostates = 6
 
        null = char(0)
        linefeed = char(10)
        last = index (in_buffer, null)
        if ( last .eq. 0 ) last = len(in_buffer)

        out_buffer(1:1) = null
        o2 = 1
        maxbuf_out = len( out_buffer ) - 400
c
c       Check for re-entry and continue
c
        ix = last
        if ( ix .gt. 50 ) i = 50
        if (findstr (in_buffer(1:ix), 'CONTINUE') .ne. 0) then
           do i = 1, 3
              lastloop(i) = loop(i)
           enddo
           repeat = .true.
           scrfilx = 0
        else
           do i = 1, 3
              lastloop(i) = 0
           enddo
           repeat = .false.
           scrfilx = scrfil
c
c          Search and align to "WHERE" ...
c
           ix = findstr (in_buffer(1:last), 'WHERE')
           if (ix .gt. 0) then
              ix = ix + len('WHERE')
              change_f = gtfltr(in_buffer(ix:last))
           else
              do i = 1, 7
                 filter(i) = 0
              enddo
           endif

c          Set up the page header

           if (scrfilx. gt. 0) then
              outbuf = 'Filtered Bus/branch Output'
              call rpnlod
    
              write (outbuf, 100) chase1(1), chase1(34), chase1(35)
  100         format('Case: ', a10, ' Project: ', 2a10)
              call hedlod

              outbuf = ' *'
              call shdlod(1)
              call shdlod(2)
              call shdlod(3)
              outbuf = ' '
              call shdlod(4)
              call shdlod(5)
              call comlod(1)
              call comlod(2)
              call forbtm()
              call fortop()
              call prnt_fltr (in_buffer(ix:))
           endif
        endif
 
        if (lastloop(2) .eq. 0 .and. lastloop(3) .eq. 0) then
           ib = lastloop(1) + 1
        else
           ib = lastloop(1)
        endif 
        finished = .false.
        do while (ib .le. ntot_alf .and. .not. finished)
           nb = alf2inp(ib)
           type = bustyp(nb)
           if (chkfltr( arcnam(jarzn(nb)), zone(nb), owner(nb), 
     &                  base(nb), type, nb)) then
                   
              do j = 1, 3
                 if (lastloop(2) .lt. j) then
                    if (scrfilx .gt. 0) then
                       outbuf = header(j)
                       call prtout(1)
                    endif
                    if (o2 .lt. maxbuf_out) then
                       length = apdoutbuf(o2, outbuf(1:80), 
     &                                    out_buffer(o2:))
                       o2 = o2 + length
                       loop(1) = ib
                       loop(2) = j
                       loop(3) = 0
                       lastloop(1) = 0
                       lastloop(2) = 0
                       lastloop(3) = 0
                    else if (repeat) then
                       finished = .true.
                    endif
                 endif
              enddo

              pnet = 0.0
              call bcdbus (nb, temp_text)
              kt = inp2opt(nb)
              vk = sqrt (e(kt) ** 2 + f(kt) ** 2)
              ang = 57.295 * atan2 (f(kt), e(kt))
              pload = ploadu(kt) * bmva + inetr(kt) * vk * bmva
              qload = qloadu(kt) * bmva - ineti(kt) * vk * bmva
              pgen = pnetu(kt) * bmva + pload
              qgen = qnetu(kt) * bmva + qload
              pshunt = busdta(5,nb) * vk ** 2
              ncb = kbsdta(15,nb)
              do while (ncb .gt. 0)
                 pnet = pnet + bctbl(6,ncb) - bctbl(2,ncb)
                 pshunt = pshunt + bctbl(4,ncb) * vk ** 2
                 ncb = bctbl_nxt(ncb)
              enddo

              call allocq (nb, qnetu(kt), qgen, qgnmax, qgnmin, qld, 
     &                     totcap, usecap, totrek, userek, unsked, 
     &                     qerr)
              if (totcap .gt. 0.0) then
                 qshunt = totcap 
                 qused = usecap + amax1(0.0, unsked)
              else if (totrek .lt. 0.0) then
                 qshunt = totrek
                 qused = userek + amin1 (0.0, unsked)
              else
                 qshunt = 0.0
                 qused = 0.0
              endif
              write (outbuf, 110) temp_text(7:18), vk, pgen, qgen, 
     &                      pload, qload, pshunt, qshunt, qused, 
     &                      temp_text(1:2)
  110         format (t2, a, t15, f5.3, f7.1, f8.1, f8.1, f8.1,
     &                3f8.1, 3x, a)
              if (lastloop(2) .lt. 4) then
                 if (scrfilx .gt. 0) call prtout(1)
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:80), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = ib
                    loop(2) = 4
                    loop(3) = 0
                    lastloop(1) = 0
                    lastloop(2) = 0
                    lastloop(3) = 0
                 else if (repeat) then
                    finished = .true.
                 endif
              endif
              write (outbuf, 112) vk * base(nb), ang
  112         format (t8, f6.1, '/', f5.1)
              if (lastloop(2) .lt. 5) then
                 if (scrfilx .gt. 0) call prtout(1)
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:80), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = ib
                    loop(2) = 5
                    loop(3) = 0
                    lastloop(1) = 0
                    lastloop(2) = 0
                    lastloop(3) = 0
                 else if (repeat) then
                    finished = .true.
                 endif
              endif
              if (totcap .gt. 0.0 .and. totrek .lt. 0.0) then
                 qshunt = totrek
                 qused = userek + amin1 (0.0, unsked)
                 write (outbuf, 120) qshunt, qused
  120            format (t51, f8.1, f8.1)
                 if (lastloop(2) .lt. 6) then
                    if (scrfilx .gt. 0) call prtout(1)
                    if (o2 .lt. maxbuf_out) then
                       length = apdoutbuf(o2, outbuf(1:80), 
     &                                    out_buffer(o2:))
                       o2 = o2 + length
                       loop(1) = ib
                       loop(2) = 6
                       loop(3) = 0
                       lastloop(1) = 0
                       lastloop(2) = 0
                       lastloop(3) = 0
                    else if (repeat) then
                       finished = .true.
                    endif
                 endif
              endif
              if (lastloop(2) .lt. 7) then
                 if (scrfilx .gt. 0) then
                    outbuf = header(5)
                    call prtout(1)
                 endif
                 if (o2 .lt. maxbuf_out) then
                    length = apdoutbuf(o2, outbuf(1:80), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    loop(1) = ib
                    loop(2) = 7
                    loop(3) = 0
                    lastloop(1) = 0
                    lastloop(2) = 0
                    lastloop(3) = 0
                 else if (repeat) then
                    finished = .true.
                 endif
              endif

              if (lastloop(3) .eq. 0) then
                 ptr = kbsdta(16,nb)
              else 
                 ptr = brnch_nxt(lastloop(3))
              endif
              do while (ptr .gt. 0 .and. .not. finished)
                 ltyp = brtype(ptr)
                 if (ltyp .eq. 1) then
                    call gtlfq (ptr, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    oldptr = ptr
                    nptr = brnch_nxt(ptr)
                    do while (nptr .gt. 0 .and.
     &                       (ky(nptr) .eq. ky(ptr) .and.
     &                        brid(nptr) .eq. brid(ptr)))
                       call gtlfq (nptr, pinx, qinx, plossx, qlossx, 
     &                             ovldx, ratecx, actual_amps,
     &                             whichend1, actual_mva, whichend2)
                       if (ovldx .gt. ovld) then
                          ovld = ovldx
                          ratec = ratecx
                       endif
                       oldptr = nptr
                       nptr = brnch_nxt(nptr)
                    enddo
                    call bcdbrn (brnch_nxt(ptr), temp_text)
                    write (outbuf, 244) temp_text(32:32), 
     &                    temp_text(20:31), pin, qin, ploss, qloss
                    if (ovld .ne. 0.0) then
                       iovld = ovld
                       write (outbuf(66:69), 248, err=121) iovld
                       go to 122
  121                  outbuf(66:69) = '****'
  122                  continue
                    endif
                    if (ratec .ne. ' ') then
                       read (ratec(3:6), 239) irate
  239                  format (i4)
                       rate = irate
                       irate = rate * ovld / 100.0
                       write (ratec(3:6), 239) irate
                       outbuf(71:80) = ratec
                    endif
                    if (scrfilx .gt. 0) call prtout(1)
                    if (o2 .lt. maxbuf_out) then
                       length = apdoutbuf(o2, outbuf(1:80), 
     &                                    out_buffer(o2:))
                       o2 = o2 + length
                       loop(1) = ib
                       loop(2) = 7
                       loop(3) = oldptr
                       lastloop(1) = 0
                       lastloop(2) = 0
                       lastloop(3) = 0
                    else if (repeat) then
                       finished = .true.
                    endif
                    ptr = oldptr
                    nptr = 0
                 else if (ltyp .eq. 4) then
                 else
                    call bcdbrn (ptr, temp_text)
                    call gtlfq (ptr, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    write (outbuf, 244) temp_text(32:32), 
     &                                temp_text(20:31), pin,
     &                                qin, ploss, qloss
  244               format (t20, a, 1x, a, f8.1, f8.1, f8.1, f8.1)
                    if (ltyp .eq. 5) then
                       nbr = brnch_ptr(ptr)
                       if (nbr .gt. 0) then
                          tap1 = brnch(9,nbr)
                          tap2 = brnch(10,nbr)
                       else
                          nbr = -nbr
                          tap1 = brnch(10,nbr)
                          tap2 = brnch(9,nbr)
                       endif
                       write (outbuf(1:19), 246) tap1, tap2
  246                  format (t5, f7.2, '/', f6.2)
c
c                      Research branch data for "R" record
c
                       nptr = kbsdta(16,nb)
                       do while (nptr .gt. 0)
                          if (ky(nptr) .eq. ky(ptr)) then
                             if (brtype(nptr) .eq. 4) then
                                outbuf(2:4) = 'LTC'
                             endif
                             nptr = 0
                          else
                             nptr = brnch_nxt(nptr)
                          endif
                       enddo
                    else if (ltyp .eq. 6) then
                       nbr = brnch_ptr(ptr)
                       if (nbr .gt. 0) then
                          tap1 = brnch(9,nbr)
                       else
                          nbr = -nbr
                          tap1 = -brnch(9,nbr)
                       endif
                       write (outbuf(1:19), 247) tap1
  247                  format (t5,f7.2, ' DEG')
c
c                      Research branch data for "R" record
c
                       nptr = kbsdta(16,nb)
                       do while (nptr .gt. 0)
                          if (ky(nptr) .eq. ky(ptr)) then
                             if (brtype(nptr) .eq. 4) then
                                outbuf(2:4) = 'LTC'
                             endif
                             nptr = 0
                          else
                             nptr = brnch_nxt(nptr)
                          endif
                       enddo
                    endif
                    if (ovld .ne. 0.0) then
                       iovld = ovld
                       write (outbuf(66:69), 248,err=249) iovld
  248                  format (i4)    
                       go to 250
  249                  outbuf(66:69) = '****'
  250                  continue
                    endif
                    if (ratec .ne. ' ') then
                       read (ratec(3:6), 239) irate
                       rate = irate
                       irate = rate * ovld / 100.0
                       write (ratec(3:6), 239) irate
                       outbuf(71:80) = ratec
                    endif
                    if (scrfilx .gt. 0) call prtout(1)
                    if (o2 .lt. maxbuf_out) then
                       length = apdoutbuf(o2, outbuf(1:80), 
     &                                    out_buffer(o2:))
                       o2 = o2 + length
                       loop(1) = ib
                       loop(2) = 7
                       loop(3) = ptr
                       lastloop(1) = 0
                       lastloop(2) = 0
                       lastloop(3) = 0
                    else if (repeat) then
                       finished = .true.
                    endif
                 endif
                 ptr = brnch_nxt(ptr)
              enddo  ! ptr gt 0
 
           endif  ! chkfltr
           ib = ib + 1
        enddo   ! ib lt ntot_alf
 
c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+9), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
