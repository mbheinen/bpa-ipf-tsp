C    @(#)busbrotrpt.f	20.7 2/28/00
c***********************************************************************
c
c     File: busbrotrpt.f
c
c     Purpose: Compute bus-branch output in WSCC (IPS) format.
c
c     Author: Walt Powell             Date: 7 December 1992
c
c     Modified: 
c
c
c     Called by: p_reports.f from the GUI
c
c***********************************************************************
c
      subroutine busbrotrpt (in_buffer, out_buffer, scrfil)
      character in_buffer *(*), out_buffer *(*)
      integer scrfil

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/pfstates.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/sortuvov.inc'
      include 'ipfinc/xdata.inc'

	character capital * 120, temp_text * 120, text * 120, 
     &            ratec * 10, ratecx * 10, header(6) * 80, null * 1,
     &            linefeed * 1
	integer apdoutbuf, o2, ptr, oldptr, whichend1, whichend2

        logical found, change_f, gtfltr

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

        maxbuf_out = len( out_buffer ) - 400

        o2 = 1
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
c
c       Search and align to "WHERE" ...
c
        text = in_buffer
        i = index (capital(text), 'WHERE') ! Search WHERE in text 
C                                         ! because capital >= text
        found = .false.
        if (i .gt. 0) then
           i = i + len('WHERE')
           change_f = gtfltr(in_buffer(i:))
           found = .true.
        endif
        if (found) then
           ix = 1
           do while (ix .le. filter(6) .and. o2. le. maxbuf_out)
              nb = bus_filter(ix)
                   
              do j = 1, 3
                 if (scrfil .gt. 0) write (scrfil, '(a)') header(j)
                 length = apdoutbuf(o2, header(j), 
     &                                  out_buffer(o2:))
                 o2 = o2 + length
              enddo

              pnet = 0.0
              call bcdbus (nb, temp_text)
              if (ordvlt .eq. 1) then
                 kt = nb
              else
                 kt = inp2opt(nb)
              endif
c
c	e() and f() have been converted to double precision.
c
              vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
              ang = 57.295 * datan2 (f(kt), e(kt))
              pload = ploadu(kt) * bmva + inetr(kt) * vk * bmva
              qload = qloadu(kt) * bmva - ineti(kt) * vk * bmva
              pgen = (pnetu(kt) - inetr(kt) * vk) * bmva + pload 
              qgen = (qnetu(kt) - ineti(kt) * vk) * bmva + qload
              gsched = busdta(5,nb) * vk ** 2
              ncb = kbsdta(15,nb)
              do while (ncb .gt. 0)
                 pnet = pnet + bctbl(6,ncb) - bctbl(2,ncb)
                 gsched = gsched + bctbl(4,ncb) * vk ** 2
                 ncb = bctbl_nxt(ncb)
              enddo

              call allocq (nb, sngl(qnetu(kt)), qgen, qgnmax, qgnmin, 
     &                     qld, totcap, usecap, totrek, userek,
     &                     unsked, qerr)
              if ( usecap + unsked .gt. 0.0 ) then
                 if ( totcap .gt. 0.0 ) then
                    bsched = totcap
                 else
                    bsched = totrek
                 end if
                 bused = usecap + unsked
              else if ( userek + unsked .lt. 0.0 ) then
                 if ( totrek .lt. 0.0 ) then
                    bsched = totrek
                 else
                    bsched = totcap
                 end if
                 bused = userek + unsked
              else if ( unsked .lt. 0.0 ) then
                 if ( totrek .lt. 0.0 ) then
                    bsched = totrek
                 else
                    bsched = totcap
                 end if
                 bused = unsked
              else if ( unsked .gt. 0.0 ) then
                 if ( totcap .gt. 0.0 ) then
                    bsched = totcap
                 else
                    bsched = totrek
                 end if
                 bused = unsked
              else if ( totrek .lt. 0.0 .and. totcap .eq. 0.0 ) then
                 bsched = totrek
                 bused = userek
              else
                 bsched = totcap
                 bused = usecap
              end if
              write (text, 100) temp_text(7:18), vk, pgen, qgen, pload, 
     &                      qload, gsched, bsched, bused, temp_text(1:2)
  100         format (t2, a, t15, f5.3, f7.1, f8.1, f8.1, f8.1,
     &                3f8.1, 3x, a)
              if (scrfil .gt. 0) write (scrfil, '(a)') text(1:80)
              length = apdoutbuf(o2, text(1:80), 
     &                               out_buffer(o2:))
              o2 = o2 + length
              if (ang .gt. -99.5) then
                write (text, 110) vk * base(nb), ang
  110           format (t8, f6.1, '/', f5.1)
              else
                write (text, 112) vk * base(nb), ang
  112           format (t8, f6.1, '/', f6.1)
              endif
              if (scrfil .gt. 0) write (scrfil, '(a)') text(1:80)
              length = apdoutbuf(o2, text(1:80), 
     &                               out_buffer(o2:))
              o2 = o2 + length
              if (totcap .gt. 0.0 .and. totrek .lt. 0.0) then
                 qshunt = totrek
                 qused = userek + amin1 (0.0, unsked)
                 write (text, 120) qshunt, qused
  120            format (t59, f8.1, f8.1)
                 if (scrfil .gt. 0) write (scrfil, '(a)') text(1:80)
                 length = apdoutbuf(o2, text(1:80), 
     &                                  out_buffer(o2:))
                 o2 = o2 + length
              endif
              if (scrfil .gt. 0) write (scrfil, '(a)') header(4)
              length = apdoutbuf(o2, header(4), 
     &                               out_buffer(o2:))
              o2 = o2 + length
              if (scrfil .gt. 0) write (scrfil, '(a)') header(5)
              length = apdoutbuf(o2, header(5), 
     &                               out_buffer(o2:))
              o2 = o2 + length

              ptr = kbsdta(16,nb)
              do while (ptr .gt. 0)
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
                    write (text, 244) temp_text(32:32), 
     &                    temp_text(20:31), pin, qin, ploss, qloss
                    if (ovld .ne. 0.0) then
                       iovld = ovld
                       write (text(66:69), 248, err=121) iovld
                       go to 122
  121                  text(66:69) = '****'
  122                  continue
                    endif
                    if (ratec .ne. ' ') then
                       read (ratec(3:6), 239) irate
  239                  format (i4)
                       rate = irate
                       irate = rate * ovld / 100.0
                       write (ratec(3:6), 239) irate
                       text(71:80) = ratec
                    endif
                    if (scrfil .gt. 0) write (scrfil, '(a)') text(1:80)
                    length = apdoutbuf(o2, text(1:80),      
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    ptr = oldptr
                    nptr = 0
                 else if (ltyp .eq. 4) then
                 else
                    call bcdbrn (ptr, temp_text)
                    call gtlfq (ptr, pin, qin, ploss, qloss, ovld, 
     &                          ratec, actual_amps, whichend1, 
     &                          actual_mva, whichend2)
                    write (text, 244) temp_text(32:32), 
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
                       write (text(1:19), 246) tap1, tap2
  246                  format (t5, f7.2, '/', f6.2)
c
c                      Research branch data for "R" record
c
                       nptr = kbsdta(16,nb)
                       do while (nptr .gt. 0)
                          if (ky(nptr) .eq. ky(ptr)) then
                             if (brtype(nptr) .eq. 4) then
                                text(2:4) = 'LTC'
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
                       write (text(1:19), 247) tap1
  247                  format (t5,f7.2, ' DEG')
c
c                      Research branch data for "R" record
c
                       nptr = kbsdta(16,nb)
                       do while (nptr .gt. 0)
                          if (ky(nptr) .eq. ky(ptr)) then
                             if (brtype(nptr) .eq. 4) then
                                text(2:4) = 'LTC'
                             endif
                             nptr = 0
                          else
                             nptr = brnch_nxt(nptr)
                          endif
                       enddo
                    endif
                    if (ovld .ne. 0.0) then
                       iovld = ovld
                       write (text(66:69), 248,err=249) iovld
  248                  format (i4)    
                       go to 250
  249                  text(66:69) = '****'
  250                  continue
                    endif
                    if (ratec .ne. ' ') then
                       read (ratec(3:6), 239) irate
                       rate = irate
                       irate = rate * ovld / 100.0
                       write (ratec(3:6), 239) irate
                       text(71:80) = ratec
                    endif
                    if (scrfil .gt. 0) write (scrfil, '(a)') text(1:80)
                    length = apdoutbuf(o2, text(1:80), 
     &                                     out_buffer(o2:))
                    o2 = o2 + length
                 endif
                 ptr = brnch_nxt(ptr)
              enddo
              ix = ix + 1
           enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
           if (o2 .gt. maxbuf_out) then
              write (out_buffer(o2:o2+8), 820) linefeed, null
  820         format (a, '*[MORE]', a)
              o2 = o2 + 9
           endif
        endif
  900   continue
        return
        end
