C    @(#)add_ptin.f	20.3 5/27/98
C****************************************************************
C
C     File: add_ptin.f
C
C     Purpose: Routine to define PTI number hash index given PTI number

C
c     Return code:  n > 0 : PTI number hash index
c                   n < 0 : PTI number hash index (duplicate entity)
c                   n = 0 : Error or termination flag
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function add_ptin (numpti)
      integer numpti

      include 'ipfinc/parametr.inc'

      include 'ipfinc/pti_data.inc'
      include 'ipfinc/prt.inc'

      common /is_batch / is_batch       ! 0 = interactive, 1 = batch

      integer  num, h, p

      num = iabs (numpti)
      if (num .gt. 0) then
        h = mod (num, PTI_HASHSIZE) + 1
        p = htable_n(h)
        do while (p .gt. 0)            !search for existing entities
          if (num .ne. pti_num(p)) then
            p = nextptr_n(p)
          else
            p = -p                   
          endif
        enddo
        if (p .eq. 0) then
          if (num_hashn .eq. PTI_MAXBUS) then
            write ( errbuf(1), 10000) PTI_MAXBUS
10000       format (' More than ', i5, ' entities in PTI-number hash tab
     &les.')
            call prterx ('F', 1)
            call exit ()
          else
            num_hashn = num_hashn + 1
            nextptr_n(num_hashn) = htable_n(h)
            htable_n(h) = num_hashn
            pti_num(num_hashn) = num
            add_ptin = num_hashn
          endif
        else
          add_ptin = p                  ! p < 0 flags duplicate entity!

        endif
      else
        add_ptin = 0
      endif
      return
      end
