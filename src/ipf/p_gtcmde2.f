C    @(#)p_gtcmde2.f	20.2 5/27/99
C****************************************************************
C
C       File: p_gtcmde2.f
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
	subroutine p_gtcmde2 (inpfil)

        integer inpfil
 
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
 
        logical finished, eof
        integer status, p, pold, q, qold, cmde2_proc
        external komp_cmde, swap_cmde
        character chg_type(2)*1, tempc*120
 
        data chg_type / 'D', 'M' /
 
        error = 0              ! 0 = normal
c                              ! 1 = e-o-f on unit inp
c                              ! 2 = error condition
        tempc = buf 
        last_chg = 0
        in_zn_cnt = 0
        comm_ptr(num_comm_mode) = 0
        eof = .false.
        rewind inpfil
        do while (.not. eof)
          read (inpfil, fmt='(a)', end=900) buf
          card = buf(1:1)
          if (index ('B+LTE', card) .ne. 0) then
            if (buf(1:1) .eq. 'E') then
              buf(1:1) = 'L'
              card = buf(1:1)
            else if (buf(1:2) .eq. 'RZ') then
              buf(1:2) = 'L '
              card = buf(1:1)
            endif
  	    status = cmde2_proc (last_chg, in_zn)
            if (status .ne. 0) then
              comm_status(num_comm_mode) = 1
            else
              in_zn_cnt = in_zn_cnt + 1
            endif
          else if (index ('./', card) .ne. 0) then
          else if (card .eq. '(') then
            eof = .true.
          else
            write (errbuf(1), 10000) buf(1:50)
10000       format (' MODE record not recognized  (', a, ')')
            call prterx ('W', 1)
            error = 1   
          endif
        enddo
  900   continue

        if (comm_ptr(num_comm_mode) .eq. 0) then
          write (errbuf(1), 10010) comm_mode(num_comm_mode)(1:40)
10010     format (' MODE record is missing change (', a, ')')
          call prterx ('W', 1)
          error = 1   
          num_comm_mode = num_comm_mode - 1
        else if ( in_zn_cnt .eq. 0 ) then
          write (errbuf(1), 10020) comm_mode(num_comm_mode)(1:40)
10020     format (' MODE record has no elements in outage list (', 
     &      a, ')')
          call prterx ('W', 1)
          go to 150
        endif
        go to 160
c    
c       Errors encountered.  Nullify all entities pertaining to 
c       current entity.
c
  150   if (num_comm_mode .gt. 0) then
          p = comm_ptr(num_comm_mode)
          write (outbuf, 10030) comm_mode(num_comm_mode)(1:40)
10030     format (' OUTAGE TEXT skipped (', a, ')')
          call prtout (1)   
          comm_ptr(num_comm_mode) = 0
          comm_status(num_comm_mode) = 0
          do while (p .gt. 0)
            write (outbuf, 10030) change_rcd(p)(1:40)
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

  160   continue
        rewind inpfil
        buf = tempc
        card = tempc(1:1)

        call forbtm()
        return
        end  
