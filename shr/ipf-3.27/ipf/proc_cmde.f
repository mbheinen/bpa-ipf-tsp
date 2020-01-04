C    @(#)proc_cmde.f	20.10 3/29/99
C****************************************************************
C
C       File: proc_cmde.f
C       Purpose: Routine to process COMMON-MODE-OUTAGE data
C
C       Invoked by:
C              > MODE <modename>
C          
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: get_cmde
C
C****************************************************************
	integer function proc_cmde (last_chg, in_zn)
	integer last_chg, in_zn
 
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
        include 'ipfinc/zbdata.inc'
 
        logical finished, first, finished2
        integer rec_type, chg_type, find_bus, find_cbs, find_br,
     &          p, pold, q, error, sect, old_num_types, old_last_orig
        character bus1*8, bus2*8, text*120, id*1
        complex*16 y(2,2)
 
        k1 = 0
        k2 = 0
        kt = 0
        mt = 0
        last_orig = 0
        error = 0
        in_zn = 0
 
        if (index ('B+LT', card) .ne. 0) then  
           read (buf, 100, err=900) bus1, base1 
  100      format (bz, t7, a8, f4.0)  
           k1 = find_bus (bus1, base1)  
           if (k1 .le. 0) then
              write (errbuf(1), 110) bus1, base1   
  110         format ('> MODE bus ', a8, f6.1, ' is not in system.')   
              call prterx ('W', 1)
              error = 1
              go to 920
           endif
           kt = inp2opt(k1)
        endif
 
        if (index ('LT', card) .ne. 0) then  
           read (buf, 120, err=900) bus2, base2
  120      format (bz, t20, a8, f4.0)  
           k2 = find_bus (bus2, base2)  
           if (k2 .le. 0) then
              write (errbuf(1), 110) bus2, base2
              call prterx ('W', 1)
              error = 1
              go to 920
           endif
           mt = inp2opt(k2)
           read (buf, 203) id, sect
           p = find_br (k1, k2, id, sect, 0)
           finished2 = (p .le. 0)
           do while (.not. finished2)
             if (p .le. 0) then
               finished2 = .true.
             else if (brtype(p) .eq. 4) then
               p = brnch_nxt(p)
             else if (ky(p) .eq. k2 .and. brid(p) .ne. id) then
               p = brnch_nxt(p)
             else
               finished2 = .true.
             endif
           enddo
           if (ky(p) .ne. k2 .or. brid(p) .ne. id) then
             write (errbuf(1), 204) bus(k1), base(k1), bus(k2), 
     &          base(k2), id
             write (errbuf(2), 205) buf(1:80)
             call prterx ('W', 2)
             error = 1
             go to 920
           endif
        endif
 
        if (index ('DM', buf(3:3)) .eq. 0) then  
           write (errbuf(1), 130) buf(1:32)
  130      format (' Illegal change type (', a, ')')
           call prterx ('W', 1)
           error = 1
           go to 920
        endif
 
        if (num_changes .eq. 3*MAXCOMMODE) then
           write (errbuf(1), 160) 3*MAXCOMMODE
  160      format (' More than ', i4, 
     &             ' changes associated with > MODE')
           write (errbuf(2), 170) buf(1:80)
  170      format (' Overflow at (', a, ')')
           call prterx ('W', 2)
           error = 1   
           go to 920
        endif

        if (nc .gt. 0) then
           if (base(k1) .ge. vcl .and. base(k1) .le. vch) then
              do i=1,nc
                 if (zone(k1) .eq. znc(i)) then
                    in_zn = 1
                    go to 175
                 endif
              enddo
           endif
           if (index('LT', card) .ne. 0) then  
              if (base(k2) .ge. vcl .and. base(k2) .le. vch) then
                 do i=1,nc
                    if (zone(k2) .eq. znc(i)) then
                       in_zn = 1
                       go to 175
                    endif
                 enddo
              endif
           endif
        else
           if (base(k1) .ge. vcl .and. base(k1) .le. vch) in_zn = 1
           if (index('LT', card) .ne. 0) then  
             if (base(k2) .ge. vcl .and. base(k2) .le. vch) in_zn = 1
           endif
        endif
  175   continue

        num_changes = num_changes + 1
        change_rcd(num_changes) = buf
        if (last_chg .eq. 0) then
           comm_ptr(num_comm_mode) = num_changes
        else
           change_nxt(last_chg) = num_changes
        endif
        last_chg = num_changes
 
        if (num_types .eq. 8*MAXCOMMODE) then
           write (errbuf(1), 180) 8*MAXCOMMODE
  180      format (' More than ', i4, 
     &             ' temporary records associated with > MODE')
           write (errbuf(2), 170) buf(1:80)
           call prterx ('W', 2)
           error = 1   
           go to 920
        endif
 
        old_num_types = num_types
        old_last_orig = last_orig
        num_types = num_types + 1
        if (last_orig .eq. 0) then
           change_ptr(num_changes) = num_types
        else
           orig_nxt(last_orig) = num_types
        endif
        last_orig = num_types
        orig_nxt(num_types) = 0
 
        rec_type = index('B+LT',buf(1:1))
        orig_type(1,num_types) = rec_type
        chg_type = index('DM',buf(3:3))
        orig_type(2,num_types) = chg_type
        orig_type(3,num_types) = kt
        orig_type(4,num_types) = mt
        orig_type(5,num_types) = ichar(' ')
        orig_type(6,num_types) = 0
        orig_type(7,num_types) = 0
        orig_type(8,num_types) = 0
 
        if (rec_type .eq. 1 .and. chg_type .eq. 1) then
c
c          Process   B D   deletions. 
c
           orig_val(1,num_types) = pnetu(kt)
           orig_val(2,num_types) = qnetu(kt)
           orig_val(3,num_types) = 0.0
           orig_val(4,num_types) = 0.0
           orig_val(5,num_types) = 0.0
           orig_val(6,num_types) = 0.0
           orig_val(7,num_types) = 0.0
           orig_val(8,num_types) = 0.0
           orig_type(6,num_types) = k1
 
           p = kbsdta(16,k1)
           do while (p .gt. 0)
              k2 = ky(p)
              mt = inp2opt(k2)
              ltyp = brtype(p)
              pold = p
              if (ltyp .eq. 1) then
                 call pieqiv (p, y, error)
                 p = brnch_nxt(p)
                 finished = .false.
                 do while (p .gt. 0 .and. .not. finished)
                    if (ky(p) .eq. ky(pold) .and. 
     &                  brid(p) .eq. brid(pold)) then
                       p = brnch_nxt(p)
                    else
                       finished = .true.
                    endif
                 enddo                       
              else if (ltyp .eq. 2 .or. ltyp .eq. 4 .or. ltyp .eq. 7) 
     &           then
                 p = brnch_nxt(p)
                 go to 198
              else
                 call pieqiv (p, y, error)
                 p = brnch_nxt(p)
              endif
              if (num_types .eq. 8*MAXCOMMODE) then
                 write (errbuf(1), 160) 8*MAXCOMMODE
                 write (errbuf(2), 170) buf(1:80)
                 call prterx ('W', 2)
                 error = 1
                 go to 920
              endif
 
              num_types = num_types + 1
              if (last_orig .eq. 0) then
                 change_ptr(num_changes) = num_types
              else
                  orig_nxt(last_orig) = num_types
              endif
              last_orig = num_types
              orig_nxt(num_types) = 0
 
              if (in_zn .ne. 0) goto 195
              if (nc .gt. 0) then
                 if (base(k2) .ge. vcl .and. base(k2) .le. vch) then
                    do i=1,nc
                       if ( zone(k2) .eq. znc(i) ) then
                          in_zn = 1
                          go to 195
                       endif
                    enddo
                 endif
              else if (base(k2) .ge. vcl .and. base(k2) .le. vch) then
                 in_zn = 1
              endif
  195         continue

              if (ltyp .eq. 5 .or. ltyp .eq. 6) then
                 orig_type(1,num_types) = 4   ! T - record
              else
                 orig_type(1,num_types) = 3   ! L - record
              endif
              orig_type(2,num_types) = 1   ! Delete record
              orig_type(3,num_types) = kt
              orig_type(4,num_types) = mt
              orig_type(5,num_types) = ichar(brid(pold))
              orig_type(6,num_types) = pold
              orig_type(7,num_types) = 1          ! Flag pseudo-change
              orig_val(1,num_types) = -dreal (y(1,2))
              orig_val(2,num_types) = -dimag (y(1,2))
              orig_val(3,num_types) = dreal (y(1,1))
              orig_val(4,num_types) = dimag (y(1,1))
              orig_val(5,num_types) = -dreal (y(2,1))
              orig_val(6,num_types) = -dimag (y(2,1))
              orig_val(7,num_types) = dreal (y(2,2))
              orig_val(8,num_types) = dimag (y(2,2))
c
c             Get transpose values
c
              q = find_br (k2, k1, brid(pold), 0, brtype(pold))
              finished2 = (q .le. 0)
              do while (.not. finished2)
                if (q .le. 0) then
                  finished2 = .true.
                else if (brtype(q) .eq. 4) then
                  q = brnch_nxt(q)
                else if (ky(q) .eq. k1 .and. brid(q) .ne. brid(pold))
     &            then
                  q = brnch_nxt(q)
                else
                  finished2 = .true.
                endif
              enddo
              if (ky(q) .ne. k1 .or. brid(q) .ne. brid(pold)) then
                write (errbuf(1), 204) bus(k2), base(k2), bus(k1), 
     &            base(k1), brid(pold)
                write (errbuf(2), 205) buf(1:80)
                call prterx ('W', 2)
                error = 1
                go to 920
              endif
              call pieqiv (q, y, error)
 
              if (num_types .eq. 8*MAXCOMMODE) then
                 write (errbuf(1), 160) 8*MAXCOMMODE
                 write (errbuf(2), 170) buf(1:80)
                 call prterx ('W', 2)
                 error = 1
                 go to 920
              endif
 
              num_types = num_types + 1
              if (last_orig .eq. 0) then
                 change_ptr(num_changes) = num_types
              else
                  orig_nxt(last_orig) = num_types
              endif
              last_orig = num_types
              orig_nxt(num_types) = 0
 
              if (ltyp .eq. 5 .or. ltyp .eq. 6) then
                 orig_type(1,num_types) = 4   ! T - record
              else
                 orig_type(1,num_types) = 3   ! L - record
              endif
              orig_type(2,num_types) = 1   ! Delete record
              orig_type(3,num_types) = mt
              orig_type(4,num_types) = kt
              orig_type(5,num_types) = ichar(brid(q))
              orig_type(6,num_types) = q
              orig_type(7,num_types) = 1          ! Flag pseudo-change
              orig_val(1,num_types) = -dreal (y(1,2))
              orig_val(2,num_types) = -dimag (y(1,2))
              orig_val(3,num_types) = dreal (y(1,1))
              orig_val(4,num_types) = dimag (y(1,1))
              orig_val(5,num_types) = -dreal (y(2,1))
              orig_val(6,num_types) = -dimag (y(2,1))
              orig_val(7,num_types) = dreal (y(2,2))
              orig_val(8,num_types) = dimag (y(2,2))
 
  198         continue
           enddo
        else if (rec_type .le. 2 .and. chg_type .eq. 2) then
c
c          Process   B M   or   + M   modification
c
           read (buf, 200) pload, qload, gshunt, bshunt, pgen, qgen
  200      format( t21, 2f5.0, 2f4.0, t43, 2f5.0)
           orig_val(1,num_types) = (pgen - pload) / bmva
           orig_val(2,num_types) = (qgen - qload) / bmva
           orig_val(3,num_types) = pload / bmva
           orig_val(4,num_types) = qload / bmva
           orig_val(5,num_types) = gshunt / bmva
           orig_val(6,num_types) = bshunt / bmva
           if (rec_type .eq. 1) then
              orig_type(6,num_types) = k1
           else
              ncb = find_cbs (buf)
              orig_type(6,num_types) = ncb
           endif
 
        else if (rec_type .eq. 2 .and. chg_type .eq. 1) then
c
c          Process   + D   deletions
c
           ncb = find_cbs (buf)
           if (ncb .gt. 0) then
              call bcdcbs (ncb, text)
              read (text, 200) pload, qload, gshunt, bshunt, pgen,
     &                         qgen
              orig_val(1,num_types) = (pgen - pload) / bmva
              orig_val(2,num_types) = (qgen - qload) / bmva
              orig_val(3,num_types) = pload / bmva
              orig_val(4,num_types) = qload / bmva
              orig_val(5,num_types) = gshunt / bmva
              orig_val(6,num_types) = bshunt / bmva
              orig_type(6,num_types) = ncb
           else
              write (errbuf(1), 202) text(1:21)
  202         format (' + record not in system (', a, ')')
              call prterx ('W', 1)
              num_types = old_num_types
              last_orig = old_last_orig
              error = 1
              go to 920
           endif
 
        else if (rec_type .gt. 2 .and. chg_type .eq. 1) then
c
c          Process   L D   or   T D   deletions
c
           read (buf, 203) id, sect
  203      format (t32, a, i1)
           
           p = find_br (k1, k2, id, 0, 0)
           finished2 = (p .le. 0)
           do while (.not. finished2)
             if (p .le. 0) then
               finished2 = .true.
             else if (brtype(p) .eq. 4) then
               p = brnch_nxt(p)
             else if (ky(p) .eq. k2 .and. brid(p) .ne. id) then
               p = brnch_nxt(p)
             else
               finished2 = .true.
             endif
           enddo
           if (ky(p) .ne. k2 .or. brid(p) .ne. id) then
             write (errbuf(1), 204) bus(k1), base(k1), bus(k2), 
     &          base(k2), id
  204        format (' > mode branch ', a8, f6.1, 1x, a8, f6.1,
     &          1x, a, ' is not in system')
             write (errbuf(2), 205) buf(1:80)
  205        format (' Change record (', a, ')')
             call prterx ('W', 2)
             num_types = old_num_types
             last_orig = old_last_orig
             error = 1
             go to 920
           endif

           first = .true.

           if (brtype(p) .eq. 2 .or. brtype(p) .eq. 7) go to 206
           call pieqiv (p, y, error)
           if (num_types .eq. 8*MAXCOMMODE) then
              write (errbuf(1), 160) 8*MAXCOMMODE
              write (errbuf(2), 170) buf(1:80)
              call prterx ('W', 2)
              error = 1
              go to 920
           endif
  
           last_orig = num_types
           orig_nxt(num_types) = 0
           if (brtype(p) .eq. 5 .or. brtype(p) .eq. 6) then
              orig_type(1,num_types) = 4   ! T - record
           else
              orig_type(1,num_types) = 3   ! L - record
           endif
           orig_type(2,num_types) = 1   ! Delete record
           orig_type(3,num_types) = kt
           orig_type(4,num_types) = mt
           orig_type(5,num_types) = ichar(brid(p))
           orig_type(6,num_types) = p
           if (.not. first) then
               orig_type(7,num_types) = 1   ! Flag pseudo-change
           endif
           orig_val(1,num_types) = -dreal (y(1,2))
           orig_val(2,num_types) = -dimag (y(1,2))
           orig_val(3,num_types) = dreal (y(1,1))
           orig_val(4,num_types) = dimag (y(1,1))
           orig_val(5,num_types) = -dreal (y(2,1))
           orig_val(6,num_types) = -dimag (y(2,1))
           orig_val(7,num_types) = dreal (y(2,2))
           orig_val(8,num_types) = dimag (y(2,2))
 
           if (.not. first) then
              num_types = num_types + 1
              if (last_orig .eq. 0) then
                 change_ptr(num_changes) = num_types
              else
                 orig_nxt(last_orig) = num_types
              endif
           else
              first = .false.
           endif
c
c          Retrieve transpose
c
           q = find_br (k2, k1, id, 0, 0)
           finished2 = (q .le. 0)
           do while (.not. finished2)
             if (q .le. 0) then
               finished2 = .true.
             else if (brtype(q) .eq. 4) then
               q = brnch_nxt(q)
             else if (ky(q) .eq. k1 .and. brid(q) .ne. id) then
               q = brnch_nxt(q)
             else
               finished2 = .true.
             endif
           enddo
           if (ky(q) .ne. k1 .or. brid(q) .ne. id) then
             write (errbuf(1), 204) bus(k2), base(k2), bus(k1), 
     &         base(k1), id
             write (errbuf(2), 205) buf(1:80)
             call prterx ('W', 2)
             error = 1
             go to 920
           endif

           call pieqiv (q, y, error)
           if (num_types .eq. 8*MAXCOMMODE) then
              write (errbuf(1), 160) 8*MAXCOMMODE
              write (errbuf(2), 170) buf(1:80)
              call prterx ('W', 2)
              error = 1
              go to 920
           endif
 
           num_types = num_types + 1
           if (last_orig .eq. 0) then
              change_ptr(num_changes) = num_types
           else
              orig_nxt(last_orig) = num_types
           endif
           last_orig = num_types
 
           orig_nxt(num_types) = 0
           if (brtype(q) .eq. 5 .or. brtype(q) .eq. 6) then
              orig_type(1,num_types) = 4   ! T - record
           else
              orig_type(1,num_types) = 3   ! L - record
           endif
           orig_type(2,num_types) = 1   ! Delete record
           orig_type(3,num_types) = mt
           orig_type(4,num_types) = kt
           orig_type(5,num_types) = ichar(brid(p))
           orig_type(6,num_types) = q
           orig_type(7,num_types) = 1        ! Flag pseudo-change
           orig_val(1,num_types) = -dreal (y(1,2))
           orig_val(2,num_types) = -dimag (y(1,2))
           orig_val(3,num_types) = dreal (y(1,1))
           orig_val(4,num_types) = dimag (y(1,1))
           orig_val(5,num_types) = -dreal (y(2,1))
           orig_val(6,num_types) = -dimag (y(2,1))
           orig_val(7,num_types) = dreal (y(2,2))
           orig_val(8,num_types) = dimag (y(2,2))
 
  206      continue

        else
 
           write (errbuf(1), 210) buf(1:33)
  210      format (' Illegal change record (', a, ')')
           call prterx ('W', 1)
           error = 1
           go to 920
 
        endif
        go to 920
 
  900   write (errbuf(1), 910) buf(1:33)
  910   format (' Error decoding change record (', a, ')')
        call prterx ('W', 1)
  
  920   proc_cmde = error
        return
        end
