C    @(#)chek_lout.f	20.3 2/13/96
C****************************************************************
C
C       File: chek_lout.f
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
	integer function chek_lout (jout, jovl)
        integer jout, jovl
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/apcom.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/datainit.inc'
 
        integer comm_out(MAXBUS), ptr, qtr
 
        save 
 
        if (jout .ne. last_out2) then
           if (jout .le. nout) then
              i = klnc(1,jout)  
              j = klnc(2,jout)
              ic = mod(klnc(4,jout),1000)
              kc = min0(i,j)
              mc = max0(i,j)
           else
              do i = 1, nbus
                 comm_out(i) = 0
              enddo
              ptr = comm_ptr(jout-nout)
              do while (ptr .gt. 0) 
                 qtr = change_ptr(ptr)
                 do while (qtr .gt. 0)
c
c                   Flag all terminal pair of outaged lines
c
                    if ((orig_type(1,qtr) .eq. 3 .or. 
     &                   orig_type(1,qtr) .eq. 4) .and.
     &                 orig_type(2,qtr) .eq. 1) then
                       kc = orig_type(3,qtr)
                       mc = orig_type(4,qtr)
                       comm_out(kc) = qtr 
                       comm_out(mc) = qtr 
                    endif
                    qtr = orig_nxt(qtr)
                 enddo
                 ptr = change_nxt(ptr)
              enddo
           endif
           last_out2 = jout
        endif
       
        k = klno(1,jovl)
        m = klno(2,jovl)
 
        chek_lout = 0
        if (jout .le. nout) then
 
           if (min0(k,m) .eq. kc .and. 
     &         max0(k,m) .eq. mc .and.
     &         ic .eq. klno(5,jovl)) then
              chek_lout = jovl
           endif
        else if (comm_out(k) .ne. 0 .and. comm_out(m) .ne. 0) then
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
                       chek_lout = qtr 
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
