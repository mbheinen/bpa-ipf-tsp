C    @(#)find_br.f	20.3 2/13/96
	integer function find_br (k1, k2, id, sect, type)

        integer k1, k2, sect, type
        character *1 id
c                                                                       
c       This function locates the branch pointer specified
c       in the input arguments.
c                                                                       
c       output result:                                                    
c                                                                       
c           find_br = +/-ptr (branch pointer)
c                     0      error
c                                                                       
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/tran.inc'
c                                                                       
        integer p, compare
        logical found

        find_br = 0
        found = .false.
        p = kbsdta(16,k1)
        do while (p .ne. 0 .and. .not. found)
           compare = inp2alf(ky(p)) - inp2alf(k2)
           if (compare .eq. 0) then
C
C             Defer parallel ID check for following changes:
C             LM, LD, and R
C
              if (brtype(p) .eq. 2 .or. brtype(p) .eq. 7) then
                 if (type .gt. 0 .and. type .ne. brtype(p)) compare = -1
                 found = .true.
              else if (brtype(p) .eq. 4) then
                 if (type .gt. 0 .and. type .ne. brtype(p)) then
                    compare = -1
                 else
                    found = .true.
                 endif
              else
C
C                Compare parallel IDs
C
                 compare = kompr (brid(p), id, junk)
                 if (compare .eq. 0) then
                    compare = brsect(p) - sect
                    if (compare .ne. 0) then
                       compare = sign (compare, brnch_ptr(p))
                    else
                       if (type .gt. 0 .and. type .ne. brtype(p)) 
     &                    compare = -1
                       found = .true.
                    endif
                 endif
              endif
           endif
           if (compare .eq. 0) then
              find_br = p
              found = .true.
           else if (compare .gt. 0) then
              found = .true.
           else
              p = brnch_nxt(p)
           endif
        enddo
        return
        end
