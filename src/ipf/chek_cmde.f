C    @(#)chek_cmde.f	20.4 8/20/98
C****************************************************************
C
C       File: chek_cmde.f
C       Purpose: Routine to check validity of an > MODE change
C                records.
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: get_cmde
C
C****************************************************************
        subroutine chek_cmde(mode, error)
        integer mode, error
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/comm_mode.inc'
 
        common /sort_cmde/ num_sort, sort(100)
        integer num_sort, sort
 
        integer last_type, this_type, last_k1, last_k2,
     &          this_chg, find_cmde, p, q, q1, q2, qold
        character rec_type(4) * 1, chg_type(2) * 1, text1 * 120,
     &            text2 * 120
        logical finished
 
        data rec_type / 'B', '+', 'L', 'T' /,
     &       chg_type / 'D', 'M' /
 
        error = 0
c
c       Check for duplicates
c
        last_ix = 1
        p = sort(last_ix)
        last_type = orig_type(1,p)
        last_chg = orig_type(2,p)
        last_k1 = orig_type(3,p)
        last_k2 = orig_type(4,p)
        ix = 2
        do while (ix .le. num_sort)
           q = sort(ix)
           this_type = orig_type(1,q)
           this_chg = orig_type(2,q)
           k1 = orig_type(3,q)
           k2 = orig_type(4,q)
           if (k1 .eq. last_k1) then
              q1 = orig_type(6,p)
              q2 = orig_type(6,q)
              if (this_type .eq. 1) then
c
c                Duplicate 'B D' change records or
c                duplicate 'B M' change records or
c                incompatible B D' - 'B M' change records
c
                 if (orig_type(7,p) .eq. 0) then
                    i1 = find_cmde (mode, p)
                    text1 = change_rcd(i1)
                    text1(3:3) = chg_type(last_chg)
                 else
                    nb = opt2inp(k1)
                    call bcdbus (nb, text1)
                    text1(3:3) = chg_type(last_chg)
                 endif
                 if (orig_type(7,q) .eq. 0) then
                    i2 = find_cmde (mode, q)
                    text2 = change_rcd(i2)
                    text2(3:3) = chg_type(this_chg)
                 else
                    nb = opt2inp(k1)
                    call bcdbus (nb, text2)
                    text2(3:3) = chg_type(this_chg)
                 endif
 
                 write (errbuf(1), 100)
  100            format (' Incompatible change sequence; second record i
     &gnored.')
                 write (errbuf(2), 110) text1(1:80)
  110            format (' Record 1 (', a, ')')
                 write (errbuf(3), 120) text2(1:80)
  120            format (' Record 2 (', a, ')')
                 if (orig_type(7,p) .eq. 0 .or.
     &               orig_type(7,q) .eq. 0) call prterx ('W', 3)
 
                 sort(ix) = -sort(ix)
 
              else if (this_type .eq. 2) then
 
                 if (last_type .eq. 1) then
c
c                   'B *' - '+ *' sequence. Invalid combination is:
c                   'B D' - '+ *'
c
                    if (last_chg .eq. 1) then
                       if (orig_type(7,p) .eq. 0) then
                          i1 = find_cmde (mode, p)
                          text1 = change_rcd(i1)
                          text1(3:3) = chg_type(last_chg)
                       else
                          nb = opt2inp(k1)
                          call bcdbus (nb, text1)
                          text1(3:3) = chg_type(last_chg)
                       endif
                       if (orig_type(7,q) .eq. 0) then
                          i2 = find_cmde (mode, q)
                          text2 = change_rcd(i2)
                          text2(3:3) = chg_type(this_chg)
                       else
                          call bcdcbs (q2, text2)
                          text2(3:3) = chg_type(this_chg)
                       endif
 
                       write (errbuf(1), 100)
                       write (errbuf(2), 110) text1(1:80)
                       write (errbuf(3), 120) text2(1:80)
                       if (orig_type(7,p) .eq. 0 .or.
     &                    orig_type(7,q) .eq. 0) call prterx ('W', 3)
 
                       sort(ix) = -sort(ix)
 
                    endif
 
                 else if (last_type .eq. 2 .and. q1 .eq. q2) then
c
c                   '+ *' - '+ *' sequence. 
c
                    if (orig_type(7,p) .eq. 0) then
                       i1 = find_cmde (mode, p)
                       text1 = change_rcd(i1)
                       text1(3:3) = chg_type(last_chg)
                    else
                       call bcdcbs (q1, text1)
                       text1(3:3) = chg_type(last_chg)
                    endif
                    if (orig_type(7,q) .eq. 0) then
                       i2 = find_cmde (mode, q)
                       text2 = change_rcd(i2)
                       text2(3:3) = chg_type(this_chg)
                    else
                       call bcdcbs (q2, text2)
                       text2(3:3) = chg_type(this_chg)
                    endif
 
                    write (errbuf(1), 100)
                    write (errbuf(2), 110) text1(1:80)
                    write (errbuf(3), 120) text2(1:80)
                    if (orig_type(7,p) .eq. 0 .or.
     &                 orig_type(7,q) .eq. 0) call prterx ('W', 3)
   
                    sort(ix) = -sort(ix)
                 endif
 
              else if (this_type .eq. 3 .or. this_type .eq. 4) then
 
                 if (last_type .eq. 1) then
c
c                   'B *' - 'L *' sequence. Invalid combination is:
c                   'B D' - 'L M'
c
                    if (last_chg .eq. 1 .and. this_chg .ne. 1) then
                       if (orig_type(7,p) .eq. 0) then
                          i1 = find_cmde (mode, p)
                          text1 = change_rcd(i1)
                          text1(3:3) = chg_type(last_chg)
                       else
                          nb = opt2inp(k1)
                          call bcdbus (nb, text1)
                          text1(3:3) = chg_type(last_chg)
                       endif
                       if (orig_type(7,q) .eq. 0) then
                          i2 = find_cmde (mode, q)
                          text2 = change_rcd(i2)
                          text2(3:3) = chg_type(this_chg)
                       else
                          call bcdbrn (q2, text2)
                          text2(3:3) = chg_type(this_chg)
                       endif
 
                       write (errbuf(1), 100)
                       write (errbuf(2), 110) text1(1:80)
                       write (errbuf(3), 120) text2(1:80)
                       if (orig_type(7,p) .eq. 0 .or.
     &                    orig_type(7,q) .eq. 0) call prterx ('W', 3)
 
                       sort(ix) = -sort(ix)
                    endif
 
                 else if ((last_type .eq. 3 .or. last_type .eq. 4) .and. 
     &              iabs (brnch_ptr(q1)) .eq. iabs (brnch_ptr(q2))) then
c
c                   'L *' - 'L *' or 'T *' - 'T *' duplicate.
c
                    if (orig_type(7,p) .eq. 0) then
                       i1 = find_cmde (mode, p)
                       text1 = change_rcd(i1)
                       text1(3:3) = chg_type(last_chg)
                    else
                       call bcdbrn (q1, text1)
                       text1(3:3) = chg_type(last_chg)
                    endif
                    if (orig_type(7,q) .eq. 0) then
                       i2 = find_cmde (mode, q)
                       text2 = change_rcd(i2)
                       text2(3:3) = chg_type(this_chg)
                    else
                       call bcdbrn (q2, text2)
                       text2(3:3) = chg_type(this_chg)
                    endif
 
                    write (errbuf(1), 100)
                    write (errbuf(2), 110) text1(1:80)
                    write (errbuf(3), 120) text2(1:80)
                    if (orig_type(7,p) .eq. 0 .or.
     &                 orig_type(7,q) .eq. 0) call prterx ('W', 3)
 
                    sort(ix) = -sort(ix)
                 endif
              endif
           endif
           if (sort(ix) .gt. 0) then
              last_ix = ix
              p = sort(last_ix)
              last_type = orig_type(1,p)
              last_chg = orig_type(2,p)
              last_k1 = orig_type(3,p)
              last_k2 = orig_type(4,p)
           else
c
c             Delete entity "q" in orig_type(*,q) 
c
              qold = 0
              i2 = find_cmde (mode, q)
              q1 = change_ptr(i2)
              finished = .false.
              do while (q1 .gt. 0 .and. .not. finished)
                 if (q1 .eq. q) then
                    if (qold .eq. 0) then
                       change_ptr(i2) = orig_nxt(q)
                    else
                       orig_nxt(qold) = orig_nxt(q)
                    endif
                    finished = .true.
                 else
                    qold = q1
                    q1 = orig_nxt(q1)
                 endif
              enddo
              orig_type(1,q) = 0
           endif
           ix = ix + 1
        enddo
        return
        end
