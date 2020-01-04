C    %W% %G%
C****************************************************************
C
C     File: ld_gegen.f
C
C     Purpose: Routine to load GE generator data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 " Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gegen (xbuf, file, options, error)
      integer file, error, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'

      character word(60)*10, busname*8, type*1, cbown*3, cbtyp*1, 
     &          cbkyr*2, bus_own*3, gen_own*3, gen_id*2
      integer fnd_ptin, find_bus, ftn_atoi, status, own_bus, ptr,
     &        add_cbs, read_ge_file
      logical found

      ld_gegen = 0
      error = 0
      last = lastch0 (xbuf)
      do while (xbuf(last:last) .eq. '/') 
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      enddo
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   
 
      npti1 = ftn_atoi(word(1))
      gen_id = 'G' // word(4)
      status = ftn_atoi(word(7))
      npti2 = ftn_atoi(word(8))
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gegen = 1
      else if (status .eq. 0) then
        go to 900
      else
        num1 = fnd_ptin (npti1)
        if (num1 .le. 0) then
          write (errbuf(1), 10000)
10000     format (' Generator record is not preceded with a Bus record i
     &n raw data file.')
          errbuf(2)= ' (' // xbuf(1:60) // ')'
          call prterx ('W',2)
          error = 1
          go to 900
        endif
        busname = pti_name(num1)
        basekv = pti_base(num1)
        nb = find_bus (busname, basekv)
        if (nb .lt. 0) then
          write (errbuf(1), 10010) busname, basekv
10010     format (' Bus (', a8, f7.1, ') in Generator record is not in s
     &ystem.')
          call prterx ('W', 1)
          error = 1
          go to 900
        endif
        gen_own = word(39)
        bus_own = owner(nb)
        busdta(7,nb) = busdta(7,nb) + ftn_atof(word(16))      ! Pmax
        if (gen_own .eq. bus_own) then
          busdta(8,nb) = busdta(8,nb) + ftn_atof(word(15))    ! PG
          busdta(9,nb) = busdta(9,nb) + ftn_atof(word(19))    ! Qmax
          busdta(10,nb) = busdta(10,nb) + ftn_atof(word(20))  ! Qmin
        else
          ptr = add_cbs (nb, 'A', gen_own, gen_id)
          if (ptr .eq. 0) go to 900
          bctbl(6,ptr) = bctbl(6,ptr) + ftn_atof(word(15))       ! PG
          bctbl(11,ptr) = bctbl(11,ptr) + ftn_atof(word(19))     ! Qmax
          bctbl(12,ptr) = bctbl(12,ptr) + ftn_atof(word(20))     ! Qmin
        endif
        num2 = fnd_ptin (npti2)
        if (num2 .le. 0) then
          write (errbuf(1), 10040)
10040     format (' Remotely controlled bus is not in raw data file.')
          errbuf(2)= ' (' // xbuf(1:60) // ')'
          call prterx ('W',2)
          error = 1
          go to 900
        endif
        busname = pti_name(num2)
        basekv = pti_base(num2)
        kc = find_bus (busname, basekv)
        if (kc .lt. 0) then
          write (errbuf(1), 10050) busname, basekv
10050     format (' Remotely controlled bus (', a8, f7.1, 
     &      ') in Generator record is not in system.')
          call prterx ('W', 1)
          error = 1
          go to 900
        endif
        if (kbsdta(1,nb) .eq. 3) then
          ntype = 3
        else if (kc .ne. nb) then
          ntype = 8
          kbsdta(1,nb) = ntype                     ! Change to type BG
          busdta(11,nb) = 0.0
          busdta(12,nb) = 0.0
          if (kbsdta(1,kc) .eq. 1) then
            kbsdta(1,kc) = 4
            busdta(11,kc) = vstart(kc)
            busdta(12,kc) = vstart(kc)
          endif
        else
          if (kbsdta(1,nb) .ne. 8) then
            kbsdta(1,nb) = 7
            busdta(11,nb) = vstart(kc)
            busdta(12,nb) = vstart(kc)
          else if (kc .ne. nb) then
            kbsdta(1,nb) = 8                       ! Change to type BG
            busdta(11,nb) = 0.0
            busdta(12,nb) = 0.0
          else
            kbsdta(1,nb) = 7
            busdta(11,nb) = vstart(kc)
            busdta(12,nb) = vstart(kc)
          endif
        endif
c
c       Expand vmin,vmax limits 100% if RELAX_BG option selected
c
        if (kbsdta(1,nb) .eq. 8 .and. options(9) .eq. 1) then
          call vltlim (nb, vmin, vmax, vstrt)
          vave = 0.5 * (vmin + vmax) 
          vmin = amin1 (vave, vmin, vave + 2.0 * (vmin - vave))
          vmax = amax1 (vave, vmax, vave + 2.0 * (vmax - vave))
          busdta(11,nb) = vmax
          busdta(12,nb) = vmin
        endif
        kbsdta(13,nb) = kc
        pct = ftn_atof(word(12)) * 100.0           ! %-var
        busdta(14,nb) = pct
      endif
      npti3 = ftn_atoi(word(27))
      if (npti3 .gt. 0) then
        write (errbuf(1), 10070) 
10070   format (' Generator step-up transformer data is ignored.')
        errbuf(2) = ' [' // xbuf(1:80)  // ']'
        call prterx ('W', 2)                
        error = 1                           
      endif
      npti4 = ftn_atoi(word(29))
      if (npti4 .gt. 0) then
        write (errbuf(1), 10080) 
10080   format (' Generator terminal bus data is ignored.')
        errbuf(2) = ' [' // xbuf(1:80)  // ']'
        call prterx ('W', 2)                
        error = 1                           
      endif
      go to 900

  340 write (errbuf(1), 10090) 
10090 format (' Premature E-O-F encountered processing Generator data')
      errbuf(2)= ' (' // xbuf(1:60) // ')'
      call prterx ('W', 2)
      ld_gegen = 1

  900 continue
      return
      end
