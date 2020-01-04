C    @(#)chek_bout.f	20.3 2/13/96
C****************************************************************
C
C       File: chek_bout.f
C       Purpose: Routine to check whether an voltage check should be 
C                exempt from an outage.  This is applicable only for
C                common mode outages. 
C
C       Parameters:
C
C          jout = outage index.  If jout > nout, jout-nout = common_mode
C                 outage.
C          jbus = voltage bus
C
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: check
C
C****************************************************************
	integer function chek_bout (jout, jbus)
        integer jout, jbus
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/apcom.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/datainit.inc'
 
        common /scratch/ kolum(MAXBUS), net(200), mtrx(MAXBUS), 
     &                   comm_out(MAXBUS)
        integer comm_out

        integer ptr, qtr
 
        save 
 
        if (jout .ne. last_out1) then
           if (jout .le. nout) then
           else
              do i = 1, nbus
                 comm_out(i) = 0
              enddo
              ptr = comm_ptr(jout-nout)
              do while (ptr .gt. 0) 
                 qtr = change_ptr(ptr)
                 do while (qtr .gt. 0)
                    if (orig_type(1,qtr) .eq. 1 .and.
     &                 orig_type(2,qtr) .eq. 1) then
                       kc = orig_type(3,qtr)
                       comm_out(kc) = qtr 
                    endif
                    qtr = orig_nxt(qtr)
                 enddo
                 ptr = change_nxt(ptr)
              enddo
           endif
           last_out1 = jout
        endif
       
        chek_bout = 0
        if (jout .le. nout) then
        else if (comm_out(jbus) .ne. 0) then
           chek_bout = jbus
        endif
 
        return
        end
