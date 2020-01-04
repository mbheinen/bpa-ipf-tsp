C    @(#)get_cmde.f	20.7 5/27/99
C****************************************************************
C
C       File: get_cmde.f
C       Purpose: Routine to get COMMON-MODE-OUTAGE data
C
C       Invoked by:
C              / OUTAGE_SIMULATION
C              > COMMON_MODE, FILE = <filename>
C          
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: OTEXT
C
C****************************************************************
	subroutine get_cmde (error)
        integer error
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/blank.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/com006.inc'
        include 'ipfinc/coment.inc'
        include 'ipfinc/cont.inc'
        include 'ipfinc/dflrat.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/comm_mode.inc'
        include 'ipfinc/qksrt.inc'
 
        common /sort_cmde/ num_sort, sort(100)
        integer num_sort, sort, in_zn, in_zn_cnt
 
        logical finished 
        integer findstr, status, p, pold, q, qold, proc_cmde, pbr
        external komp_cmde, swap_cmde
        character rec_type(4) * 1, chg_type(2) * 1, br_own * 3
 
        data rec_type / 'B', '+', 'L', 'T' /
        data chg_type / 'D', 'M' /
 
c       Initialize these counters in OTEXT rather than here to allow the 
c       data from multiple COMMON_MODE  commands/files to be appended
c
c        num_comm_mode = 0
c        num_changes = 0
c        num_types = 0

        error = 0              ! 0 = normal
c                              ! 1 = e-o-f on unit inp
c                              ! 2 = error condition
 
        if (buf(1:1) .ne. '>' .or.
     &      findstr(buf(2:), 'COMMON') .eq. 0) go to 900
 
        read (inp, 110, end=160) buf   
  110   format (a)
 
        finished = .false.
        do while (.not. finished)
           card = buf(1:1)   
           call space (1)
           write (outbuf,120 ) buf(1:80) 
  120      format (' OUTAGE TEXT (', a, ')')
           call prtout (1)   
           if (card .eq. '.') then   
              read (inp, 110, end=160) buf   
           else if (card .eq. '>' .and.
     &              findstr(buf(2:), 'MODE') .gt. 0) then
              if (num_comm_mode .eq. MAXCOMMODE) then
                 write (errbuf(1), 130) MAXCOMMODE
  130            format (' More than ', i3, ' > COMMON_MODE records')
                 write (errbuf(2), 140) buf(1:80)
  140            format (' Overflow at (', a, ')')
                 call prterx ('W', 2)
                 error = 2
                 go to 900
              else 
                 num_comm_mode = num_comm_mode + 1
                 ix = findstr(buf(2:), 'MODE') + 6
                 do i = 1, num_comm_mode-1
                    if ( buf(ix:ix+39) .eq. comm_mode(i)(1:40) ) then
                       outbuf = ' > > > Duplicate MODE -- skipped'
     &                 // ' --  (' // buf(1:50) // ')  < < <'
                       call prtout (1) 
                       errbuf(1) = ' Duplicate MODE -- skipped --  ('
     &                   // buf(1:50) // ')'
                       call prterx ('W', 1)
                       num_comm_mode = num_comm_mode - 1
                       read (inp, 110, end=160) buf   
                       goto 10155
                    endif
                 enddo
                 comm_mode(num_comm_mode) = buf(ix:)
                 comm_ptr(num_comm_mode) = 0
                 comm_status(num_comm_mode) = 0
                 read (inp, 110, end=150) buf   
                 last_chg = 0
                 in_zn_cnt = 0
                 do while (index('>/(',buf(1:1)) .eq. 0)
                    card = buf(1:1)   
                    write (outbuf,120 ) buf(1:80) 
                    call prtout (1)   
                    if (card .eq. 'E') then
                       card = 'L'
                       buf(1:1) = 'L'
                    else if (buf(1:2) .eq. 'RZ') then
                       card = 'L'
                       buf(1:2) = 'L '
                    endif
                    if (index ('B+LT', card) .ne. 0) then
                       status = proc_cmde (last_chg, in_zn)
                       if (status .ne. 0) then
                         comm_status(num_comm_mode) = 1
                       else
                         in_zn_cnt = in_zn_cnt + in_zn
                       endif
                    else if (card .eq. '.') then
                    else
                       outbuf = ' > > > MODE record not recognized  ('
     &                    // buf(1:50) // ')  < < <'
                       call prtout (1) 
                       errbuf(1) = ' > MODE record not recognized  ('
     &                    // buf(1:50) // ')'
                       call prterx ('W', 1)
                       error = 1   
                    endif
                    read (inp, 110, end=150) buf   
                 enddo
                 goto 155
  150            error = 1         ! Return status set to e-o-f
                 buf = '.'
                 card = buf
                 finished = .true.
  155            continue
                 if (comm_ptr(num_comm_mode) .eq. 0) then
                    outbuf = ' > > > MODE record is missing change' //
     &                ' records  (' // comm_mode(num_comm_mode)(1:40)
     &                // ')  < < <'
                    call prtout (1) 
                    errbuf(1) = ' > MODE record is missing change' //
     &                ' records  (' // comm_mode(num_comm_mode)(1:40)
     &                // ')'
                    call prterx ('W', 1)
                    error = 1   
                    num_comm_mode = num_comm_mode - 1
                 else if ( in_zn_cnt .eq. 0 ) then
                    outbuf = ' > > > MODE has no elements in' //
     &                ' outage list -- skipped --  (' //
     &                comm_mode(num_comm_mode)(1:40) // ')  < < <'
                    call prtout (1) 
                    errbuf(1) = ' > MODE has no elements in' //
     &                ' outage list -- skipped --  (' //
     &                comm_mode(num_comm_mode)(1:40) // ')'
                    call prterx ('I', 1)
                    go to 10150
                 endif
              endif
           else
              finished = .true.
           endif         
           go to 10160
c    
c          Errors encountered.  Nullify all entities pertaining to 
c          current entity.
c
10150      if (num_comm_mode .gt. 0) then
              p = comm_ptr(num_comm_mode)
              write (outbuf, 910) comm_mode(num_comm_mode)(1:40)
              call prtout (1)   
              comm_ptr(num_comm_mode) = 0
              comm_status(num_comm_mode) = 0
              do while (p .gt. 0)
                 write (outbuf, 910) change_rcd(p)(1:40)
                 call prtout (1)   
                 q = change_ptr(p)
                 change_ptr(p) = 0
                 do while (q .gt. 0)
                    qold = q
                    q = orig_nxt(q)
                    orig_nxt(qold) = 0
                    orig_type(1,qold) = 0
                 enddo
                 pold = p
                 p = change_nxt(p)
                 change_nxt(pold) = 0
              enddo
              num_comm_mode = num_comm_mode - 1
           endif
c
c          Skip all records pertaining to current entity.
c
10155      do while (index ('.B+LTER', buf(1:1)) .ne. 0)
              write (outbuf, 910) buf(1:40) 
              call prtout (1)   
              read (inp, 110, end=160) buf   
           enddo

10160      continue
        enddo
        go to 170
 
  160   error = 1             ! Return status set to e-o-f
        buf = '.'
        card = buf
 
  170   continue
c
        go to 920
c
c       Error exit - skip all > COMMON_MODE records
c
  900   num_comm_mode = 0
        finished = .false.
        do while (.not. finished)
           read (inp, 110, end=160) buf   
           card = buf(1:1)   
           if (index ('>.B+LT ', card) .ne. 0) then
              call space (1)
              write (outbuf, 910) buf(1:40) 
  910         format (' OUTAGE TEXT skipped (', a, ')')
              call prtout (1)   
           else if (card .eq. '>' .and. 
     &              findstr (buf(2:), 'MODE') .ne. 0) then
              call space (1)
              write (outbuf, 910) buf(1:40) 
              call prtout (1)   
           else
              finished = .true.
           endif
        enddo
  920   continue
  930   continue
 
        call forbtm()

        return

        end  
