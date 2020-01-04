C    @(#)find_cmde.f	20.3 2/13/96
C****************************************************************
C
C       File: find_cmde.f
C       Purpose: Routine to locate original > MODE change record
C                pertaining to orig_type() index "p"
c
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: chk_cmde
C
C****************************************************************
	integer function find_cmde (mode, q)
        integer q
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/comm_mode.inc'
 
        logical found
        integer p1, q1
 
        find_cmde = 0
 
        p1 = comm_ptr(mode)     ! p1 points to change_rcd()
        found = .false.
        do while (p1 .gt. 0 .and. .not. found)
           q1 = change_ptr(p1)  ! q1 points to orig_type
           do while (q1 .gt. 0 .and. .not. found) 
              if (q1 .eq. q) then
                 find_cmde = p1
                 found = .true.                   
              else
                 q1 = orig_nxt(q1)
              endif
           enddo
           if (.not. found) p1 = change_nxt(p1)
        enddo
        return
        end
