C    @(#)find_cbs.f	20.3 2/13/96
C****************************************************************
C
C       File: find_cbs.f
C       Purpose: Routine to locate + records
C
C       Author: Walt Powell  Date: 7 Mar 1995
C                            Modified:
C       Called by: proc_cmde
C
C****************************************************************
	integer function find_cbs (text)
        character text *(*)
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/prt.inc'
 
        character cbtype * 1, cbown * 3, cbyear * 2, bus1 * 8
        integer find_bus
        logical found
 
        find_cbs = 0
        read (text, 100, err=130) bus1, base1
  100   format (bz, t7, a8, f4.0)
        k1 = find_bus (bus1, base1)
        if (k1 .le. 0) then
           write (errbuf(1), 110) bus1, base1
  110      format (' + bus ', a8, f6.1, ' is not in system.')
           call prterx ('W', 1)
           error = 1
        else
           ncb = kbsdta(15,k1)
           found = .false.
           do while (ncb .gt. 0 .and. .not. found)
              call getchr (1, cbtype, kbctbl(8,ncb))
              call getchr (3, cbown, kbctbl(10,ncb))
              call getchr (2, cbyear, kbctbl(9,ncb))
              if (cbtype .eq. text(2:2) .and.
     &            cbown  .eq. text(4:6) .and.
     &            cbyear .eq. text(19:20)) then
                 found = .true.
                 find_cbs = ncb
              else
                 ncb = bctbl_nxt(ncb)
              endif
           enddo
           if (.not. found) then
              write (errbuf(1), 120) text(1:21)
  120         format (' + record (', a, ') is not in system.')
              call prterx ('W', 1)
              error = 1
           endif
        endif            
        go to 150
 
  130   write (errbuf(1), 140) text(1:21)
  140   format (' Error decoding + record (', a, ')')
        call prterx ('W', 1)
        error = 1
 
  150   return
        end
