C    @(#)ymod_cmde.f	20.6 2/9/98
C****************************************************************
C
C       File: ymod_cmde.f
C       Purpose: Routine to perform the network changes pertaining
c                to COMMON-MODE-OUTAGE 
C
c       Calling/returned arguments:
c
c       jout     = outage -> number pointing to cline
c       isw      = -1     -> take out a line
c       isw      =  1     -> restore a line taken out previously 
c                            by isw=-1
c       noconv   = 0      -> power flow converged,
c                = 1      -> power flow didn't converge
c 
C       This routine builds a handy array of deleted buses,
C       sort(num_sort), which is used in cknt_cmde.for to test for
c       isolated subsystems.
C
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: out_cmde
C
C****************************************************************
	integer function ymod_cmde (jout, isw, noconv)
	integer jout
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/apcom.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/datainit.inc'
 
        common /sort_cmde/ num_sort, sort(100)
        integer num_sort, sort

        integer ptr, qtr, ptrl, ptru, flag(MAXBUS), switched(100),
     &          delete(100), num_del, br_count
        logical found
 
        save
 
        if (.not. ymod_cmde_flag) then
           do i = 1, nbus
              flag(i) = 0
           enddo
           do i = 1, igenq
              k = iqlim(i)
              if (k .gt. 0) flag(k) = i
           enddo
           ymod_cmde_flag = .true.
        endif
 
        if (isw .lt. 0) then
           num_switched = 0
           num_sort = 0
           num_del = 0
        endif
        ymod_cmde = 0
        ptr = comm_ptr(jout)
        do while (ptr .gt. 0) 
           qtr = change_ptr(ptr)
           do while (qtr .gt. 0)
              if (orig_type(1,qtr) .eq. 1 .or. 
     &            orig_type(1,qtr) .eq. 2) then
c
c                Process B D, B M, + D, or + M changes
c
                 kt = orig_type(3,qtr)
                 if (isw .eq. -1) then
                    pnet1(kt) = pnet1(kt) - orig_val(1,qtr)
                    qnet1(kt) = qnet1(kt) - orig_val(2,qtr)
                    cykk(1,kt) = cykk(1,kt) - orig_val(5,qtr)
                    cykk(2,kt) = cykk(2,kt) + orig_val(6,qtr)
                 else
                    pnet1(kt) = pnet1(kt) + orig_val(1,qtr)
                    qnet1(kt) = qnet1(kt) + orig_val(2,qtr)
                    cykk(1,kt) = cykk(1,kt) + orig_val(5,qtr)
                    cykk(2,kt) = cykk(2,kt) - orig_val(6,qtr)
                 endif
                 if (orig_type(1,qtr) .eq. 1 .and. 
     &               orig_type(2,qtr) .eq. 1) then
                    num_sort = num_sort + 1
                    sort(num_sort) = kt
                    ix = flag(kt)
                    if (isw .lt. 0 .and. ix .gt. 0) then
                       if (iqlim(ix) .gt. 0) then
                          num_switched = num_switched + 1
                          switched(num_switched) = kt
                          iqlim(ix) = -iqlim(ix)
                       endif
                    else if (isw .gt. 0 .and. ix .gt. 0) then
                       do i = 1, num_switched
                          if (switched(i) .eq. kt .and.
     &                        iqlim(ix) .lt. 0) then
                             iqlim(ix) = -iqlim(ix)
                          endif
                       enddo
                    endif
                 else if ((orig_type(1,qtr) .eq. 1 .and. 
     &                     orig_type(2,qtr) .eq. 1) .or.
     &                     orig_type(1,qtr) .eq. 2) then
                    ix = flag(kt)
                    if (isw .lt. 0 .and. ix .gt. 0) then
                       num_switched = num_switched + 1
                       switched(num_switched) = kt
                       shunt(ix) = shunt(ix) - orig_val(6,qtr)
                       qlow(ix) = qlow(ix) - orig_val(2,qtr)
                       qhi(ix) = qhi(ix) - orig_val(2,qtr)
                    else if (isw .gt. 0 .and. ix .gt. 0) then
                       do i = 1, num_switched
                          if (switched(i) .eq. kt .and.
     &                        iqlim(ix) .lt. 0) then
                             shunt(ix) = shunt(ix) + orig_val(6,qtr)
                             qlow(ix) = qlow(ix) + orig_val(2,qtr)
                             qhi(ix) = qhi(ix) + orig_val(2,qtr)
                          endif
                       enddo
                    endif
                 endif
 
              else if ((orig_type(1,qtr) .eq. 3 .or. 
     &                  orig_type(1,qtr) .eq. 4) .and.
     &                 (orig_type(3,qtr) .lt. orig_type(4,qtr))) then
c
c                Process L D changes (assume symmetric)
c
                 kt = orig_type(3,qtr)
                 mt = orig_type(4,qtr)
                 if (isw .eq. -1) then
                    cykk(1,kt) = cykk(1,kt) - orig_val(3,qtr)
                    cykk(2,kt) = cykk(2,kt) + orig_val(4,qtr)
                    if (cykk(1,kt) .eq. 0.0) cykk(1,kt) = 1.0e-4
                    if (cykk(2,kt) .eq. 0.0) cykk(2,kt) = 1.0e-4
                    cykk(1,mt) = cykk(1,mt) - orig_val(7,qtr)
                    cykk(2,mt) = cykk(2,mt) + orig_val(8,qtr)
                    if (cykk(1,mt) .eq. 0.0) cykk(1,mt) = 1.0e-4
                    if (cykk(2,mt) .eq. 0.0) cykk(2,mt) = 1.0e-4
                 else
                    cykk(1,kt) = cykk(1,kt) + orig_val(3,qtr)
                    cykk(2,kt) = cykk(2,kt) - orig_val(4,qtr)
                    cykk(1,mt) = cykk(1,mt) + orig_val(7,qtr)
                    cykk(2,mt) = cykk(2,mt) - orig_val(8,qtr)
                 endif
c
c                Change element y(kt,mt) in lower diagonal matrix
c
                 ptrl = ipyl(1,mt)
                 kl = ptrl + ipyl(2,mt) 
                 found = .false.
                 do while (ptrl .lt. kl .and. .not. found)
                    if (mfarl(ptrl) .eq. kt) then
                       if (isw .eq. -1) then
                          ykml(1,ptrl) = ykml(1,ptrl) 
     &                                 + 0.999999 * orig_val(1,qtr)
                          ykml(2,ptrl) = ykml(2,ptrl) 
     &                                 - 0.999999 * orig_val(2,qtr)
                          if (ykml(1,ptrl) .eq. 0.0) 
     &                       ykml(1,ptrl) = 1.0e-5
                          if (ykml(2,ptrl) .eq. 0.0) 
     &                       ykml(2,ptrl) = 1.0e-5
                       else
                          ykml(1,ptrl) = ykml(1,ptrl) 
     &                                 - 0.999999 * orig_val(1,qtr)
                          ykml(2,ptrl) = ykml(2,ptrl) 
     &                                 + 0.999999 * orig_val(2,qtr)
                       endif
                       found = .true.
                    else
                       ptrl = ptrl + 1
                    endif
                 enddo
                 if (.not. found) go to 900
c
c                Change element y(kt,mt) in upper diagonal matrix
c
                 ptru = ipyu(1,kt)
                 ku = ptru + ipyu(2,kt) 
                 found = .false.
                 do while (ptru .lt. ku .and. .not. found)
                    if (mfaru(ptru) .eq. mt) then
                       if (isw .eq. -1) then
                          ykmu(1,ptru) = ykmu(1,ptru) 
     &                                 + 0.999999 * orig_val(1,qtr)
                          ykmu(2,ptru) = ykmu(2,ptru) 
     &                                 - 0.999999 * orig_val(2,qtr)
                          if (ykmu(1,ptru) .eq. 0.0) 
     &                       ykmu(1,ptru) = 1.0e-5
                          if (ykmu(2,ptru) .eq. 0.0) 
     &                       ykmu(2,ptru) = 1.0e-5
                       else
                          ykmu(1,ptru) = ykmu(1,ptru) 
     &                                 - 0.999999 * orig_val(1,qtr)
                          ykmu(2,ptru) = ykmu(2,ptru) 
     &                                 + 0.999999 * orig_val(2,qtr)
                       endif
                       found = .true.
                    else
                       ptru = ptru + 1
                    endif
                 enddo
                 if (.not. found) go to 900
c
c                Perform topological test for deleted bus kt
c
                 if (isw .eq. -1) then
                    ptrl = ipyl(1,kt)
                    kl = ptrl + ipyl(2,kt) 
                    br_count = 0
                    do while (ptrl .lt. kl)
                       if (abs (ykml(1,ptrl)) .ge. 1.0e-5 .or.
     &                     abs (ykml(2,ptrl)) .ge. 1.0e-5)
     &                   br_count = br_count + 1
                       ptrl = ptrl + 1
                    enddo
                    ptru = ipyu(1,kt)
                    ku = ptru + ipyu(2,kt) 
                    do while (ptru .lt. ku)
                       if (abs (ykmu(1,ptru)) .ge. 1.0e-5 .or.
     &                     abs (ykmu(2,ptru)) .ge. 1.0e-5)
     &                   br_count = br_count + 1
                       ptru = ptru + 1
                    enddo
                    if (br_count .eq. 0) then
                       num_del = num_del + 1
                       delete(num_del) = kt
                    endif
                 endif
c
c                Perform topological test for deleted bus mt
c
                 if (isw .eq. -1) then
                    ptrl = ipyl(1,mt)
                    kl = ptrl + ipyl(2,mt) 
                    br_count = 0
                    do while (ptrl .lt. kl)
                       if (abs (ykml(1,ptrl)) .ge. 1.0e-5 .or.
     &                     abs (ykml(2,ptrl)) .ge. 1.0e-5)
     &                   br_count = br_count + 1
                       ptrl = ptrl + 1
                    enddo
                    ptru = ipyu(1,mt)
                    ku = ptru + ipyu(2,mt) 
                    do while (ptru .lt. ku)
                       if (abs (ykmu(1,ptru)) .ge. 1.0e-5 .or.
     &                     abs (ykmu(2,ptru)) .ge. 1.0e-5)
     &                   br_count = br_count + 1
                       ptru = ptru + 1
                    enddo
                    if (br_count .eq. 0) then
                       num_del = num_del + 1
                       delete(num_del) = mt
                    endif
                 endif
              endif
              qtr = orig_nxt(qtr)
           enddo
           ptr = change_nxt(ptr)
        enddo
c
c       Assure that any deleted buses have a small residual value of
c       shunt admittance.
c
        if (isw .eq. -1) then
           do i = 1, num_del
              kt = delete(i)
              num_sort = num_sort + 1
              sort(num_sort) = kt
              ix = flag(kt)
              if (ix .gt. 0) then
                 if (iqlim(ix) .gt. 0) then
                    num_switched = num_switched + 1
                    switched(num_switched) = kt
                    iqlim(ix) = -iqlim(ix)
                 endif
              endif
           enddo
           do i = 1, num_sort
              kt = sort(i)
              if (cykk(1,kt) .eq. 0.0) cykk(1,kt) = 1.0e-3
              if (cykk(2,kt) .eq. 0.0) cykk(2,kt) = 1.0e-3
           enddo
        else
           do i = 1, num_del
              kt = delete(i)
              ix = flag(kt)
              if (ix .gt. 0) then
                 do j = 1, num_switched
                    if (switched(j) .eq. kt .and. iqlim(ix) .lt. 0) then
                       iqlim(ix) = -iqlim(ix)
                    endif
                 enddo
              endif
           enddo
        endif
        go to 920
 
  900   write (errbuf(1), 910) intbus(kt), intbas(kt), intbus(mt),
     &                         intbas(mt)
  910   format (' Could not locate branch ', a8, f6.1, 1x, a8, f6.1,
     &          ' ykml() and ykmu()')
        call prterx ('E', 1)
        ymod_cmde = 1
  920   continue
        return
        end
