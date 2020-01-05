C****************************************************************
C
C       File: cmde2_proc.f
C       Purpose: Routine to pre-process COMMON-MODE-OUTAGE data
C
C       Invoked by:
C              > MODE <modename>
C          
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: p_gtcmde2
C
C****************************************************************
	integer function cmde2_proc (last_chg, in_zn)
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
 
        logical finished, first
        integer rec_type, chg_type, find_bus, find_cbs, find_br,
     &          p, pold, q, error, sect
        character bus1*8, bus2*8, text*120, id*1
        complex*16 y(2,2)
 
        cmde2_proc = 0

        k1 = 0
        k2 = 0
        kt = 0
        mt = 0
        last_orig = 0
        error = 0
        in_zn = 0
 
        rec_type = index('B+LT',buf(1:1))
        chg_type = index('DM R',buf(3:3))

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
        endif
 
        if (index ('DM R', buf(3:3)) .eq. 0) then  
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
           write (errbuf(2), 190) buf(1:80)
  190      format (' Overflow at (', a, ')')
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
 
        orig_type(1,num_types) = rec_type
        orig_type(2,num_types) = chg_type
        orig_type(3,num_types) = k1
        orig_type(4,num_types) = k2
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
 
              if (ltyp .eq. 5 .or. ltyp .eq. 6) then
                 orig_type(1,num_types) = 4   ! T - record
              else
                 orig_type(1,num_types) = 3   ! L - record
              endif
              orig_type(2,num_types) = chg_type
              orig_type(3,num_types) = k1
              orig_type(4,num_types) = k2
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
              if (brtype(q) .eq. 4) q = brnch_nxt(q)
              do while (q .gt. 0 .and. brid(q) .ne. brid(pold))
                q = brnch_nxt(q)
              enddo
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
              orig_type(2,num_types) = chg_type
              orig_type(3,num_types) = k2
              orig_type(4,num_types) = k1
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
              error = 1
              go to 920
           endif
 
        else if (rec_type .gt. 2) then
c
c          Process   L or T  changes
c
           read (buf, 203) id, sect
  203      format (t32, a, i1)
           
           p = find_br (k1, k2, id, 0, 0)
           if (brtype(p) .eq. 4) p = brnch_nxt(p)
           do while (p .gt. 0 .and. brid(p) .ne. id)
             p = brnch_nxt(p)
           enddo

           first = .true.
           if (p .eq. 0) then
              write (errbuf(1), 204) bus(k1), base(k1), bus(k2), 
     &           base(k2)
  204         format (' > mode branch ', a8, f6.1, 1x, a8, f6.1,
     &           ' is not in system')
              write (errbuf(2), 170) buf(1:80)
              call prterx ('W', 2)
              error = 1
              go to 920
           endif

           if (brtype(p) .eq. 4) p = brnch_nxt(p)
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
           orig_type(2,num_types) = chg_type
           orig_type(3,num_types) = k1
           orig_type(4,num_types) = k2
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
           if (brtype(q) .eq. 4) q = brnch_nxt(q)
           do while (q .gt. 0 .and. brid(q) .ne. id)
             q = brnch_nxt(q)
           enddo
           if (brtype(q) .eq. 4) q = brnch_nxt(q)
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
           orig_type(2,num_types) = chg_type
           orig_type(3,num_types) = k2
           orig_type(4,num_types) = k1
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

        endif
        go to 920
 
  900   write (errbuf(1), 910) buf(1:33)
  910   format (' Error decoding change record (', a, ')')
        call prterx ('W', 1)
        error = 1
  
  920   cmde2_proc = error
        return
        end
