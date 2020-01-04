C    @(#)chek_lcmd2.f	20.2 11/12/98
C****************************************************************
C
C       File: chek_lcmd2.f
C       Purpose: Routine to check whether an overload should be exempt
C                from an outage.
C
C       Parameters:
C
C          jout = outage index.  If jout > nout, jout-nout = common_mode
C                 outage.
C          ovld = overload index.
C
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: check
C
C****************************************************************
	integer function chek_lcmd2 (jout, jovl)
        integer jout, jovl
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/blank.inc'
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/cmde_com.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/datainit.inc'
 
        integer comm_out(MAXBUS), ptr, qtr
 
        save 
 
        chek_lcmd2 = 0
        if (jout .ne. last_out2) then
           do i = 1, ntot
              comm_out(i) = 0
           enddo
        endif
        last_out2 = jout
       
        k = klno(1,jovl)
        m = klno(2,jovl)
 
        chek_lcmd2 = 0
        if (comm_out(k) .ne. 0 .and. comm_out(m) .ne. 0) then
c
c          Both terminal pairs are flagged. Check for specific
c          branch and parallel.
c
           ptr = comm_ptr(jout-nout)
           do while (ptr .gt. 0) 
              qtr = change_ptr(ptr)
              do while (qtr .gt. 0)
                 if ((orig_type(1,qtr) .eq. 3 .or. 
     &                orig_type(1,qtr) .eq. 4) .and.
     &              orig_type(2,qtr) .eq. 1) then
                    kc = orig_type(3,qtr)
                    mc = orig_type(4,qtr)
                    if (kc .eq. k .and. mc .eq. m .and.
     &                  klno(5,jovl) .eq. orig_type(5,qtr)) then
                       chek_lcmd2 = qtr 
                       ptr = 0
                       qtr = 0
                    endif
                 endif
                 if (qtr .gt. 0) qtr = orig_nxt(qtr)
              enddo
              if (ptr .gt. 0) ptr = change_nxt(ptr)
           enddo
        endif
 
        return
        end
