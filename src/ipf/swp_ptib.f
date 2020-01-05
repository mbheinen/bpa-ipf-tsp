C    @(#)swp_ptib.f	20.8 5/3/00
C****************************************************************
C
C     File: swp_ptib.f
C
C     Purpose: Routine to swap PTI bus names index for qiksrt.
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: chk_ptib.f
C
C****************************************************************
      subroutine swp_ptib ( p, q)
      integer p, q

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/qksrt.inc'

      integer MAXPTIRECORDS
      parameter (MAXPTIRECORDS = 16000)
      common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                 nextptr_2(MAXBUS), count_newbus, 
     &                 newbusno(MAXBUS), count_newzone, 
     &                 newzoneno(MAXCZN), count_newown, 
     &                 newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                 sort_tempc(MAXPTIRECORDS)
      integer array, count, htable_2, count_newbus, newbusno, 
     &        count_newzone, newzoneno, count_newown, newownno, 
     &        sort_tempc
      character tempc*80

      character tempc2*8

      if (key .eq. 1) then

        ip = sort(p)
        sort(p) = sort(q)
        sort(q) = ip

      else if (key .eq. 2) then

        ip = newbusno(p)
        newbusno(p) = newbusno(q)
        newbusno(q) = ip

      else if (key .eq. 3) then

        ip = newzoneno(p)
        newzoneno(p) = newzoneno(q)
        newzoneno(q) = ip

      else if (key .eq. 4) then

        ip = newownno(p)
        newownno(p) = newownno(q)
        newownno(q) = ip

      else if (key .eq. 5) then

        ip = pti_anum(p)
        pti_anum(p) = pti_anum(q)
        pti_anum(q) = ip

        tempc2 = pti_anam(p)
        pti_anam(p) = pti_anam(q)
        pti_anam(q) = tempc2

      else if (key .ge. 5 .and. key .le. 8) then

        ip = sort(p)
        sort(p) = sort(q)
        sort(q) = ip

      else if (key .ge. 101 .and. key .le. 204) then

        ip = sort_tempc(p)
        sort_tempc(p) = sort_tempc(q)
        sort_tempc(q) = ip

      else if (key .ge. 205 .and. key .le. 301) then

        ip = sort(p)
        sort(p) = sort(q)
        sort(q) = ip

      endif
       
      return
      end
