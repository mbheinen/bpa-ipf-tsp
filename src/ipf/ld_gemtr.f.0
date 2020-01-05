C    %W% %G%
C****************************************************************
C
C     File: ld_gemtr.f
C
C     Purpose: Routine to load GE motor data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 " Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gemtr (xbuf, file, options, error)
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

      character word(60)*10, busname*8, ld_id*1, cbown*3, cbtyp*1, 
     &          cbkyr*2, ld_type*2, ld_own*3, getid*1
      integer fnd_ptin, find_bus, ftn_atoi, status, ptr, add_cbs,
     &        read_ge_file
      logical found

      ld_gemtr = 0
      error = 0
      last = lastch0 (xbuf)
      do while (xbuf(last:last) .eq. '/') 
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      enddo
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   
 
      npti1 = ftn_atoi(word(1))
      status = ftn_atoi(word(7))
      npti2 = ftn_atoi(word(8))
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gemtr = 1
      else if (status .eq. 0) then
      else
        num1 = fnd_ptin (npti1)
        if (num1 .le. 0) then
          write (errbuf(1), 10000)
10000     format (' Motor record is not preceded with a Bus record in ra
     &w data file.')
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
10010     format (' Bus (', a8, f7.1, ') in Motor record is not in syste
     &m.')
          call prterx ('W', 1)
          error = 1
          go to 900
        endif
        ld_id = getid (word(4))
        ld_own = word(10)

        pload = ftn_atof (word(14))
        qload = ftn_atof (word(15))
        ld_type = '*P'
        ptr = add_cbs (nb, ld_id, ld_own, ld_type)
        if (ptr .eq. 0) go to 900
        bctbl(2,ptr) = bctbl(2,ptr) + pload
        bctbl(3,ptr) = bctbl(3,ptr) + qload
      endif
      go to 900

  340 write (errbuf(1), 10090) 
10090 format (' Premature E-O-F encountered processing Motor data')
      errbuf(2)= ' (' // xbuf(1:60) // ')'
      call prterx ('W', 2)
      ld_gemtr = 1

  900 continue
      return
      end
