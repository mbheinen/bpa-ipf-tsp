C    @(#)fnd_ptin.f	20.1 7/18/96
C****************************************************************
C
C     File: fnd_ptin.f
C
C     Purpose: Routine to determin PTI number hash index given PTI numbe
C
c     Return code:  n > 0 : PTI number hash index
c                   n < 0 : PTI number hash index (duplicate entity)
c                   n = 0 : Error or termination flag
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function fnd_ptin (numpti)
      integer numpti

      include 'ipfinc/parametr.inc'

      include 'ipfinc/pti_data.inc'

      integer        num, h, p
      logical        found

      num = iabs (numpti)
      if (num .gt. 0) then
        h = mod (num, PTI_HASHSIZE) + 1
        p = htable_n(h)
        found = .false.
        do while (p .gt. 0 .and. .not. found)
          if (num .ne. pti_num(p)) then
            p = nextptr_n(p)
          else
            found = .true.
          endif
        enddo
        fnd_ptin = p   
      else
        fnd_ptin = 0
      endif
      return
      end
