C    @(#)add_ptim.f	20.1 11/11/97
C****************************************************************
C
C     File: add_ptim.f
C
C     Purpose: Routine to define PTI number hash index given PTI number,
c              bus name, basekv, and area number.
C
c     Return code:  n > 0 : PTI number hash index
c                   n < 0 : PTI number hash index (duplicate entity)
c                   n = 0 : Error or termination flag
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function add_ptim (numpti, busname, basekv, areanum)
      integer numpti, areanum
      character *(*) busname
      real basekv

      include 'ipfinc/parametr.inc'

      include 'ipfinc/pti_data.inc'

      integer  num, h, p, pold, find_bus

      num = iabs (numpti)
      if (num .gt. 0) then
        h = mod (num, PTI_HASHSIZE) + 1
        pold = 0
        p = htable_n(h)
        do while (p .gt. 0)            !search for existing entities
          if (num .ne. pti_num(p)) then
            pold = p
            p = nextptr_n(p)
          else
            do while (p .gt. 0)
              if (pti_name(p) .eq. busname .and. 
     &            pti_base(p) .eq. basekv  .and.
     &            pti_area(p) .eq. areanum) then
                p = -p                   
              else
                p = nextptr_n(p)
              endif
            enddo
          endif
        enddo
c
c       If duplicate PTI numbers are encountered, insert buses 
c       existing in system to head of list.
c
        if (p .eq. 0) then
          num_hashn = num_hashn + 1
          pti_num(num_hashn) = num
          add_ptim = num_hashn
          nb = find_bus (busname, basekv)
          if (nb .gt. 0) then
            if (pold .eq. 0) then
              nextptr_n(num_hashn) = htable_n(h)
              htable_n(h) = num_hashn
            else
              nextptr_n(num_hashn) = nextptr_n(pold)
              nextptr_n(pold) = num_hashn
            endif
          else
            if (pold .eq. 0) then
              nextptr_n(num_hashn) = htable_n(h)
              htable_n(h) = num_hashn
            else
              if (nextptr_n(pold) .gt. 0) pold = nextptr_n(pold)
              nextptr_n(num_hashn) = nextptr_n(pold)
              nextptr_n(pold) = num_hashn
            endif
          endif
        else
          add_ptim = p                  ! p < 0 flags duplicate entity!
        endif
      else
        add_ptim = 0
      endif
      return
      end
