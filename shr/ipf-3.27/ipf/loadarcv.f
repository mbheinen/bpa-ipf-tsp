C    @(#)loadarcv.f	20.3 2/13/96
C****************************************************************
C
C   File: loadarcv.f
C   Purpose: IPF subroutine to archive all commands which effect the
C            the base data
C
C   Author: Walt Powell  Date: 24 November 1992 
C   Called by: 
C
C****************************************************************
C
	subroutine loadarcv
        return
        end

c### arcvfile ###      disable archive logging

c        include 'ipfinc:parametr.inc'
c
c        include 'ipfinc:arcvfile.inc'
c        include 'ipfinc:lfiles.inc'
c
c        character text * 120
c        logical eof
c        integer findex
c
c        eof = .false.
c        rewind inp
c        do while (.not. eof)
c           read (inp, 100, end = 120) text
c  100      format (a)
c           if (text(1:1) .eq. '(' .and. 
c     &         findex (text(2:), 'end') .ne. 0) then
c              eof = .true.
c           else
c              last = lastchln (text)
c              if (last .gt. 0) write (arcvfile, 100) text(1:last)
c           endif
c        enddo
c  120   rewind inp
c        return
c        end
