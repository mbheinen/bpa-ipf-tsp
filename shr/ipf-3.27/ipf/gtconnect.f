C    @(#)gtconnect.f	20.4 8/20/98
C****************************************************************
C
C   File: gtconnect.f
C   Purpose: Routine to connection data for case in residence.
C
C   Author: Walt Powell  Date: 18 November 1992
C   Called by:
C
C****************************************************************
C
        integer function gtconnect (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       out_buffer - a character string for storing data
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/lfiles.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/ikk.inc'
        include 'ipfinc/alpha.inc'
 
        character bus_1*8, text*120, record*120, code*4, idx*1, 
     &            base_1c*4, base_2c*4, null*1, linefeed*1, tag*2
        logical eof, initialize_ikk
        integer find_bus, apdoutbuf, chkerr
        integer oldk2, p, o1, o2, count

        save initialize_ikk

        data initialize_ikk / .false. /

        if (.not. initialize_ikk) then

C        ikk(1,*) = 0/1   : No generation/generation
C           (2,*) = 0/1   : No load/load
C           (3,*) = 0/1/2 : No bus shunt/capacitors/reactors

          do nb = 1, ntot_alf
            ikk(1,nb) = 0
            ikk(2,nb) = 0
            ikk(3,nb) = 0  
            kt = inp2opt(alf2inp(nb))
            if (ntypu(kt) .ne. 5 .and. ntypu(kt) .ne. 12) then
              pgen = pnetu(kt) + ploadu(kt)
              if (pgen .gt. 0.005) ikk(1,nb) = 1
              if (ploadu(kt) .gt. 0.005 .or. qloadu(kt) .gt. 0.005)
     &          ikk(2,nb) = 1
              qgen = qnetu(kt) + qloadu(kt)           
              call allocq (nb, sngl(qnetu(kt)), qgen, qgnmax,
     &                qgnmin, qld, totcap, usecap, totrek, 
     &                userek, unsked, qerr)

              if (totcap .gt. 0.005) then
                ikk(3,nb) = 1
              else if (totrek .lt. -0.005) then
                ikk(3,nb) = 2
              endif
            endif
          enddo
          initialize_ikk = .true.
       endif
c
c       Read bus data from in_buffer
c
        max_buf = len( out_buffer) - 100
        null = char(0)
        linefeed = char(10)

        istatus = 0
        i1 = nxt_term(in_buffer)
        if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1
        i2 = index (in_buffer, null)

        out_buffer(1:1) = null
        o2 = index (out_buffer, null)
        text = '( end )'

        do while (i1 .lt. i2)
           next_i1 = nxt_term(in_buffer(i1+1:)) + i1
           text = in_buffer(i1:next_i1-1)
           if (text(1:1) .eq. 'B') then
c
c             Obtain all data associated with type "B" record.
c
              read (text, 110)  bus_1, base_1
  110         format (bz, t7, a8, f4.0)
              k1 = find_bus (bus_1,base_1)
              if (k1 .gt. 0) then
                 base_1c = code (base(k1),4,0)
                 tag = ' '
                 if (ikk(1,k1) .eq. 1) tag(1:1) = 'G'
                 if (ikk(3,k1) .eq. 1) tag(2:2) = 'C'
                 if (ikk(3,k1) .eq. 2) tag(2:2) = 'R'
                 write (record, 111) bus(k1), base_1c, tag
  111            format ('B', 1x, a8, a4, 1x, a)
                 last = lastch (record)
                 length = apdoutbuf(o2, record(1:last),
     1                                  out_buffer(o2:))
                 o2 = o2 + length
                 if (length .eq. 0) then
                    write (errbuf(1), 112) bus(k1), base(k1)
  112               format (' Output buffer overflowed at bus ', a8, 
     1                 f6.1) 
                    call prterx ('W', 1)
                    go to 900
                 endif
                 p = kbsdta(16,k1)
                 do while (p .gt. 0)
                    k2 = ky(p)
                    idx = brid(p)
                    oldk2 = ky(p)
                    npar = 0
                    ntxs = 0
                    nlines = 0
                    do while (p .gt. 0 .and. (k2 .eq. oldk2))
                       ltyp = brtype(p)
                       nbr = iabs (brnch_ptr(p))
                       if (ltyp .eq. 1) then
                          p = brnch_nxt(p)
                          count = 0                          
                          do while (p .gt. 0 .and.
     &                             (ky(p) .eq. k2 .and.
     &                              brid(p) .eq. idx .and.
     &                              brsect(p) .gt. 0))
                             count = count + 1
                             p = brnch_nxt(p)
                          enddo
                          if (count .gt. 0) then
                             nlines = amax1 (1.0, brnch(16,nbr))
                             npar = npar + nlines
                          endif
                          if (p .gt. 0) then
                             k2 = ky(p)
                             idx = brid(p)
                          endif
                       else if (ltyp .eq. 4) then
                          p = brnch_nxt(p)
                          if (p .gt. 0) then
                             k2 = ky(p)
                             idx = brid(p)
                          endif
                       else if (ltyp .eq. 5 .or. ltyp .eq. 5) then
                          nbr = iabs (brnch_ptr(p))
                          ntxs = ntxs + 1
                          nlines = amax1 (1.0, brnch(16,nbr))
                          npar = npar + nlines
                          p = brnch_nxt(p)
                          if (p .gt. 0) then
                             k2 = ky(p)
                             idx = brid(p)
                          endif
                       else
                          nbr = iabs (brnch_ptr(p))
                          if (ltyp .eq. 2 .or. ltyp .eq. 7) then
                             nlines = 1
                          else
                             nlines = amax1 (1.0, brnch(16,nbr))
                          endif
                          npar = npar + nlines
                          p = brnch_nxt(p)
                          if (p .gt. 0) then
                             k2 = ky(p)
                             idx = brid(p)
                          endif
                       endif
                    enddo
                    if (ntxs .eq. 0 .and. npar .gt. 0) then
                       base_1c = code (base(k1),4,0)
                       base_2c = code (base(oldk2),4,0)
                       write (record, 114) bus(k1), 
     1                     base_1c, bus(oldk2), 
     2                     base_2c, npar
  114                  format ('L', 1x, a8, a4, 1x, a8, a4, i2)
                       last = lastch (record)
                       length = apdoutbuf(o2, record(1:last),
     1                                    out_buffer(o2:))
                       o2 = o2 + length
                    else if (npar .gt. 0) then
                       base_1c = code (base(k1),4,0)
                       base_2c = code (base(oldk2),4,0)
                       write (record, 116) bus(k1), 
     1                     base_1c, bus(oldk2), 
     2                     base_2c, npar
  116                  format ('T', 1x, a8, a4, 1x, a8, a4, i2)
                       last = lastch (record)
                       length = apdoutbuf(o2, record(1:last),
     1                                    out_buffer(o2:))
                       o2 = o2 + length
                    endif
                 enddo
              else
c*** get rid of "adjacent bus" warning messages for now
c                 i = iabs (k1)
c                 write (errbuf(1), 140) bus_1, base_1
c  140            format (' Bus (', a, f6.1, ') is not in system. ')
c                 ierr = 1
c                 do j = i-1,i+1
c                    if (j .gt. 0 .and. j .le. ntot) then
c                       ierr = ierr + 1
c                       write (errbuf(ierr), 142) bus(j),base(j)
c  142                  format (' Adjacent bus names > ', a8, f6.1)
c                    endif
c                 enddo
c                 call prterx ('W', ierr)
                  istatus = 1
              endif
           else if (text(1:1) .ne. '(') then
              write (errbuf(1), 144) text(1:40)
  144         format (' Record not recognized (', a, ')')
              call prterx ('W', 1)
           endif

           i1 = next_i1
           if (in_buffer(i1:i1) .eq. linefeed) i1 = i1 + 1

        enddo
  900   continue
        buf = text
        if (chkerr('W')+chkerr('E')+chkerr('F') .gt. 0) istatus = 1
        gtconnect = istatus
        return
        end
