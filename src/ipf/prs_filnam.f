C    @(#)prs_filnam.f	20.3 2/13/96
c
c  parses filspec to get filnam and filext
c
        subroutine prs_filnam( filnam, filext, filspec )
        character  filnam * (*), filext * (*), filspec * (*)

        character name * 120, tname * 120
        character * 1  backslash
        logical found, slash_found

        backslash = char(92)
        slash_found = .false.
        if ( index( filspec, backslash ) .ne. 0  .or.
     &       index( filspec, '/' ) .ne. 0 ) slash_found = .true.

        name = filspec

c                       get rid of any DOS or VMS device name
        i = index( name, ':' )
        tname = name(i+1:)
        name = tname

c                       get rid of any VMS directory
        i = index( name, ']' )
        tname = name(i+1:)
        name = tname

c                       get rid of any DOS or UNIX directory
        if ( slash_found ) then
           i = len(name)
           found = .false.
           do while ( i .gt. 1  .and.  .not. found )
              i = i - 1
              if ( name(i:i) .eq. '/'  .or.
     &             name(i:i) .eq. backslash ) found = .true.
           end do
           if ( found ) then
              tname = name(i+1:)
              name = tname
           endif
        endif

        i = index( name, '.' )
        if ( i .eq. 0 ) then
           filext = ' '
           filnam = name
        else if ( i .eq. 1 ) then
           filext = name(i+1:)
           filnam = ' '
        else
           filext = name(i+1:)
           filnam = name(1:i-1)
        endif

c                       get rid of any VMS version
        i = index( filext, ';' )
        if ( i .eq. 1 ) then
        filext = ' '
        else if ( i .gt. 1 ) then
        tname = filext(1:i-1)
        filext = tname
        endif

        return
        end
c
c
c        program test
c        character * 60 filnam, filext, filspec
c
c        do while (.true.)
c           print *, 'enter filespec  (RETURN to exit)'
c           read(*,'(a)') filspec
c           if ( filspec .eq. ' ' ) call exit
c           call prs_filnam( filnam, filext, filspec )
c           print *, 'filspec = ', filspec
c           print *, 'filnam  = ', filnam
c           print *, 'filext  = ', filext
c        enddo
c        end
