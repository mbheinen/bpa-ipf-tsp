C    @(#)ld_gezones.f	20.4 5/27/98 
C**************************************************************** 
C 
C     File: ld_gezones.f 
C 
C     Purpose: Routine to load GE zone numbers and names 
C              from a translation file 
C 
C     Author: Walt Powell  Date: 22 Feb 2001 
C     Called by: load_ge.f 
C 
C**************************************************************** 
        integer function ld_gezones (options, filename)
        character *(*) filename 
        integer options(*)
 
        include 'ipfinc/parametr.inc' 
 
        include 'ipfinc/blank.inc' 
        include 'ipfinc/prt.inc' 
        include 'ipfinc/lfiles.inc' 
  
        common /ge_zones/ num_gezones, htable_geznum(MAXCZN),
     &                    nextptr_geznum(MAXCZN), 
     &                    htable_geznam(MAXCZN), 
     &                    nextptr_geznam(MAXCZN),
     &                    ge_znum(MAXCZN), 
     &                    ge_znam(MAXCZN)
        integer num_gezones, htable_geznum, nextptr_geznum, 
     &          htable_geznam, nextptr_geznam, ge_znum
        character ge_znam*2

        character xbuf*120, zonename*2, longname*32, word(10)*60
        logical finished, found 
        integer status, ftn_atoi, start, h, p, oldp, error, open_file
 
        error = 0           ! initialize error count 
        numrec = 0 
        ld_gezones = 0 
        num_gezones = 0

        do i = 1, MAXCZN
          htable_geznam(i) = 0
          nextptr_geznam(i) = 0
          htable_geznum(i) = 0
          nextptr_geznum(i) = 0
        enddo

        if (options(10) .ne. 1) then
          go to 900
        else if (filename .ne. ' ') then 
          write (*, 10000) 
10000     format(1x, '* Loading GE Translation File - this will take a f
     &ew minutes') 
          status = open_file (brndta, filename, 'F', 'R', iostat)
          if (status .ne. 0) then
            write (*, 10012) 
10012       format(1x, '* Error opening translation file ', a)
            ld_gezones = 1
            go to 900
          endif
        else
          write (*, 10010) 
10010     format(1x, '* No translation file specified')
          ld_gezones = 1
          go to 900
        endif
c 
c       Skip area data in *.TRN file 
c 
        start = numrec
        finished = .false. 
        do while (.not. finished) 
          read (brndta, fmt='(a)', end=100) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            num = ftn_atoi (word(1))
            if (num .eq. 0) then 
              finished = .true. 
            else
              numrec = numrec + 1
            endif 
          endif
        enddo 

        write (*, 10020) numrec-start-1
10020   format (' * Skipped ', i5, ' area records')
        go to 110 
 
  100   write (errbuf(1), 10030)  
10030   format ('E-O-F encountered processing area records in *.TRN file
     & ') 
        call prterx ('W', 1) 
        error = error + 1 
        go to 900
 
  110   continue
c 
c       Read in and hash zone data in *.TRN file 
C 
        start = numrec
        finished = .false. 
        do while (.not. finished) 
          read (brndta, fmt='(a)', end=120) xbuf 
          if (xbuf(1:1) .eq. '#' .or. xbuf(1:2) .eq. ' #') then
          else
            last = lastch (xbuf)
            call uscan (xbuf(1:last), word, nwrd, '=',  ' ,')   
            numzone = ftn_atoi (word(1))
            zonename = word(2)
            longname = word(3)
            numrec = numrec + 1 
            if (numzone .eq. 0) then 
              finished = .true. 
            else 
c
c             Hash zone number
c
              h = numzone
              h = mod (h, MAXCZN-1) + 1
              p = htable_geznum(h)
              oldp = 0
              found = .false.
              do while (p .gt. 0 .and. .not. found)
                if (numzone .ne. ge_znum(p)) then
                  oldp = p
                  p = nextptr_geznum(p)
                else
                  found = .true.
                endif
              enddo
              if (found) then
                write (errbuf(1), 10050) numzone, zonename,
     &            ge_znam(p) 
10050           format ('Duplicate zone number - ', i3,
     &            ' names - ', a2, 1x, a2, '. Last name selected.')
                call prterx ('W', 1) 
                error = error + 1 
              endif
              num_gezones = num_gezones + 1
              htable_geznum(h) = num_gezones
              nextptr_geznum(num_gezones) = p
              ge_znum(num_gezones) = numzone
              ge_znam(num_gezones) = zonename
c
c             Hash zone name (Note: name is already in array)
c
              h = 0
              do i = 1, 2
                h = h + h + ichar (zonename(i:i))
              end do
              h = mod (h, MAXCZN) + 1
              p = htable_geznam(h)
              oldp = 0
              found = .false.
              do while (p .gt. 0 .and. .not. found)
                if (zonename .ne. ge_znam(p)) then
                  oldp = p
                  p = nextptr_geznam(p)
                else
                  found = .true.
                endif
              enddo
              if (found) then
                write (errbuf(1), 10060) zonename, numzone, 
     &            ge_znum(p)
10060           format ('Duplicate zone name - ', a2,
     &            ' numbers - ', i3, 1x, i3, '. Last number selected.')
                call prterx ('W', 1) 
                error = error + 1 
              endif
              htable_geznam(h) = num_gezones
              nextptr_geznam(num_gezones) = p
            endif 
          endif
        enddo 
        write (*, 10070) numrec-start-1
10070   format (' * Processed ', i5, ' Zone records')

        go to 130 
 
  120   write (errbuf(1), 10080)  
10080   format ('E-O-F encountered processing zone records in *.TRN file
     & ') 
        call prterx ('W', 1) 
        error = error + 1 
 
  130   call close_file (brndta)
  900   continue
        return 
        end 
