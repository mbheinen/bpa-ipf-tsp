C    @(#)gtptinam.f	20.7 3/29/99
C****************************************************************
C
C     File: gtptinam.f
C
C     Purpose: Function to parse PTI bus number or name
C
c     Input parameters:
c
c	text         : character string of PTI number or name
c      
C     Output parameters:
c
c       numpti       : PTI number if pertinent
c       num1         : PTI number hash index
c       num2         : +1/-1
c       bus1         : PTI character name
c       basekv       : PTI base kv
c
c     Return code:  n = 0 : Success
c                   n = 1 " Error
c
C     Author: Walt Powell  Date: 29 Dec 1998
C     Called by: load_pti.f
C
C****************************************************************
      integer function gtptinam (text, numpti, num1, num2, busname, 
     &                           basekv)
      integer numpti, num1, num2
      real basekv
      character *(*) text, busname

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'

      integer ftn_atoi, fnd_ptin
      character tempc*8
      logical extended_name

      gtptinam = 0
      numpti = ftn_atoi(text)
      if (numpti .eq. 0 .and. 
     &   (text(1:1) .eq. '0' .or. text(1:2) .eq. ' 0')) then
        busname = ' '
        basekv = 0.0
        num1 = 0
        num2 = +1
        gtptinam = 1
        extended_name = .false.
      else if (numpti .eq. 0 .and. text(1:1) .eq. '-') then
        busname = text(2:9)
        basekv = ftn_atof (text(10:))
        num1 = 0
        num2 = -1
        extended_name = .true.
      else if (numpti .eq. 0) then
        busname = text(1:8)
        basekv = ftn_atof (text(9:))
        num1 = 0
        num2 = +1
        extended_name = .true.
      else
        num2 = isign (1, numpti)
        numpti = iabs (numpti)
        num1 = fnd_ptin (numpti)
        if (num1 .le. 0) then
          write (errbuf(1), 10000) numpti
10000     format (' PTI bus number ', i6, 
     &      ' is not defined in raw data file')
          call prterx ('W',1)
          busname = ' '
          basekv = 0.0
          gtptinam = 1
        else
          busname = pti_name(num1)
          basekv = pti_base(num1)
        endif
        extended_name = .false.
      endif
      if (extended_name .and. gtptinam .eq. 0 .and. 
     &    basekv .ge. 100.0) then
c
c       Round basekv to nearest 1.0 kv.
c
        x = amod (basekv, 1.0)
        if (abs (x) .gt. 0.05) then
          write (tempc, fmt='(f8.0)', err=100) basekv
          basekv = ftn_atof (tempc)
          last = lastch (text)
          last = min0 (last, 18)
          write (errbuf(1),10010) text(1:last), basekv
10010     format (' PTI bus ', a, ' rounded to ', f8.2)
          call prterx ('W',1)
          go to 110

  100     last = lastch (text)
          last = min0 (last, 18)
          write (errbuf(1),10020) text(1:last)
10020     format (' Error decoding PTI basekv ', a)
          call prterx ('W',1)
          basekv = 100.0

  110     continue
        endif
      else if (extended_name .and. gtptinam .eq. 0 .and. 
     &    basekv .ge. 10.0) then
c
c       Round basekv to nearest 0.1 kv.
c
        x = amod (10.0 * basekv, 1.0)
        if (abs (x) .gt. 0.05) then
          write (tempc, fmt='(f8.1)', err=112) basekv
          basekv = ftn_atof (tempc)
          last = lastch (text)
          last = min0 (last, 18)
          write (errbuf(1),10010) text(1:last), basekv
          call prterx ('W',1)
          go to 114

  112     last = lastch (text)
          last = min0 (last, 18)
          write (errbuf(1),10020) text(1:last)
          call prterx ('W',1)
          basekv = 100.0

  114     continue
        endif
      else if (extended_name .and. gtptinam .eq. 0 .and. 
     &    basekv .eq. 0.0) then
        basekv = 100.0
        last = lastch (text)
        last = min0 (last, 18)
        write (errbuf(1),10030) text(1:last), basekv
10030   format (' PTI bus ', a, ' has 0.0 basekv set to ', 
     &    f8.2)
        call prterx ('W',1)

      endif 

      return
      end
