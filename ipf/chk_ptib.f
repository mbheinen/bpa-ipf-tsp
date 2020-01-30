C    @(#)chk_ptib.f	20.2 11/11/97
C****************************************************************
C
C     File: chk_ptib.f
C
C     Purpose: Routine to check for duplicate PTI bus names from
c              established hash tables 
C
c     Return code:  n = 0 : Success
c                   n = 1 " Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function chk_ptib ()

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/qksrt.inc'

      integer        p, q
      external       kmp_ptib, swp_ptib
c
c     Build sorted cross-reference array
c
      if (num_hashn .gt. 0) then
        do i = 1, num_hashn
           sort(i) = i
        enddo
        key = 1
        call qiksrt (1, num_hashn, kmp_ptib, swp_ptib)
      endif

      numdup = 0

c     Flag duplicate records       
                                                 
      j = 1                                   
      k = 2                                   
      do while (k .le. num_hashn) 
        if ( kmp_ptib(j,k) .eq. 0 ) then       
          if (j .lt. k) then
            p = sort(j)
            q = sort(k)
            write (errbuf(1), 100) pti_name(p), pti_base(p), pti_zone(p)
  100       format ('Duplicate PTI bus name (', a8, f7.1, 1x, i3,
     &      ')')
            call prterx ('I', 1)
            numdup = numdup + 1
          endif
          k = k + 1
        else
          j = j + 1                                            
          k = k + 1
        endif
      enddo

      chk_ptib = 0
      return
      end
