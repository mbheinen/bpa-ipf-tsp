C    @(#)hsh_ptib.f	20.5 3/29/99
C****************************************************************
C
C     File: hsh_ptib.f
C
C     Purpose: Routine to hash PTI bus names in hash tables from *.TRN 
C              file
C
c     Return code:  n = 0 : Success
c                   n = 1 " Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: saveptid.f
C
C****************************************************************
      integer function hsh_ptib ()

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'

      integer        add_ptib, fnd_ptic
      character      tempc*10

      hsh_ptib = 0
      numdup = 0
      do i = 1, num_hashn
        iptia = fnd_ptic(pti_area(i))
        if (iptia .eq. 0) then
          tempc = ' '
          write (errbuf(1), 10000) pti_name(i), pti_base(i), pti_area(i)
10000     format ('Invalid area for PTI bus name (', a8, f7.1, 
     &            ') area no. (', i6, ')')
          call prterx ('W', 1)
        else
          tempc = pti_anam(iptia)
        endif
        ind = add_ptib (pti_name(i), pti_base(i), tempc, i)
        if (ind .lt. 0) then
          write (errbuf(1), 10010) pti_name(i), pti_base(i), pti_area(i)
10010     format ('Duplicate PTI bus name (', a8, f7.1, i6, ')')
          call prterx ('W', 1)
        endif
      enddo

      return
      end
