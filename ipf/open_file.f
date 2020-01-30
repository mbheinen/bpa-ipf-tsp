C    @(#)open_file.f	20.1 4/27/95
C****************************************************************
C
C       File: open_file.f
C       Purpose: Opens file given unit and name, and tests validity.
C
C       Author: Walt Powell  Date: 26 February 1993
C
C       Input parameters:
C               unit   - integer unit number
c               file   - file name
c               form   - "F" for FORMATTED (carriagecontrol='list'),
c                        "FF" for FORMATTED (carriagecontrol='FORTRAN'),
C                        "U" for UNFORMATTED
c               type   - "R" read, "W" write
C       Output parameters:
C               iostat - i/o status of open (0 = no errors)
c
c       Return status:
c               0 - successful
c               1 - opened, but could not write
c               2 - could not open for writing
c               3 - opened, but could not read
c               4 - could not open for reading
c
C       Called by: 
C
C****************************************************************
C
	integer function open_file (unit, file, form, type, iostat)
        character file *(*), form *(*), type *(*)
        integer unit, iostat

        include 'ipfinc/lfiles.inc'

        logical does_exist, is_formatted, is_writable
        character c*4, capital*1, option(4)*12

        if (is_it_vms() .eq. 1) then
           option(1) = 'NEW'
        else
           option(1) = 'UNKNOWN'
        endif
c
c       is_formatted = .TRUE. if FORMATTED
c                      .FALSE. if BINARY
c
c       is_writable  = .TRUE. if write-enabled
c                      .FALSE. if read-only
c
	is_formatted = (capital(form(1:1)) .eq. 'F')
	is_writable = (capital(type) .eq. 'W') 

        open_file = 0  ! Set default return status "successful"
        iostat = 0

        last = lastch(file)
        if (file .ne. ' ') then
           inquire (file=file(1:last), exist=does_exist)
        else
           inquire (unit=unit, exist=does_exist)
        endif

	if (does_exist) then

           if (.not. is_writable) option(1) = 'OLD'
           il = lastch(option(1))
           write (dbug, 110) file(1:last), unit, option(1)(1:il)
  110      format (' File ', a, ' exists, opened on unit ', i2, 
     &             ' with status "', a, '"') 
           call close_file(unit)

        else if (is_writable) then

           il = lastch(option(1))
           write (dbug, 120) file(1:last), unit, option(1)(1:il)
  120      format (' File ', a, ' does not exist, opened on unit ', 
     &             i2, ' with status "', a, '"') 
        else

           write (*, 122) file(1:last)
  122      format (' File ', a, ' does not exist')
           open_file = 4
           go to 300

        endif
        if (is_formatted) then
           option(2) = 'FORMATTED'
           if (len(form) .eq. 1) then
              option(3) = 'LIST'
           else
              option(3) = 'FORTRAN'
           endif
c
c          "carriagecontrol" = 'list' is non-ANSI! Comment out that 
c          line if compilation or execution problems are encountered!
c
           if (file .eq. ' ' .and. is_writable) then
              open(unit=unit,
     &             status=option(1),
     &             form=option(2),
c    &             carriagecontrol = option(3),
     &             iostat = iostat, 
     &             err=152)
           else if (file .eq. ' ' .and. .not. is_writable) then
              open(unit=unit,
     &             status=option(1),
     &             form=option(2),
c    &             carriagecontrol = option(3),
c    &             readonly, shared,
     &             iostat = iostat, 
     &             err=140)
           else if (file .ne. ' ' .and. is_writable) then
              open(unit=unit,
     &             file=file(1:last),
     &             status=option(1),
     &             form=option(2),
c    &             carriagecontrol = option(3),
     &             iostat = iostat, 
     &             err=152)
           else if (file .ne. ' ' .and. .not. is_writable) then
              open(unit=unit,
     &             file=file(1:last),
     &             status=option(1),
     &             form=option(2),
c    &             carriagecontrol = option(3),
c    &             readonly, shared,
     &             iostat = iostat, 
     &             err=140)
           endif

        else

           option(2) = 'UNFORMATTED'
           option(3) = ' '

           if (file .eq. ' ' .and. is_writable) then
              open(unit=unit,
     &             status=option(1),
     &             form=option(2),
     &             iostat = iostat, 
     &             err=152)
           else if (file .eq. ' ' .and. .not. is_writable) then
              open(unit=unit,
     &             status=option(1),
     &             form=option(2),
c    &             readonly, shared,
     &             iostat = iostat, 
     &             err=140)
           else if (file .ne. ' ' .and. is_writable) then
              open(unit=unit,
     &             file=file(1:last),
     &             status=option(1),
     &             form=option(2),
     &             iostat = iostat, 
     &             err=152)
           else if (file .ne. ' ' .and. .not. is_writable) then
              open(unit=unit,
     &             file=file(1:last),
     &             status=option(1),
     &             form=option(2),
c    &             readonly, shared,
     &             iostat = iostat, 
     &             err=140)
           endif      
        endif

        rewind unit

        if (is_formatted .and. is_writable) then
           c = '****'
           write (unit, 124, err=160) c
  124      format (a)
           rewind unit
           write (dbug, 132) file(1:last)
  132      format (' File ', a, ' successfully opened and written to')
           rewind unit
           go to 300

        else if (.not. is_formatted .and. is_writable) then

           c = '****'
           write (unit, err=160) c
           rewind unit
           write (dbug, 132) file(1:last)
           rewind unit
           go to 300

        else if (is_formatted .and. .not. is_writable) then

           read (unit, 124, err=180) c
           rewind unit
           write (dbug, 134) file(1:last)
  134      format (' File ', a, ' successfully opened and read from')
           rewind unit
           go to 300

        else if (.not. is_formatted .and. .not. is_writable) then

           read (unit, err=180) c
           rewind unit
           write (dbug, 134) file(1:last)
           rewind unit
           go to 300

        endif

  140   write (*, 150) file(1:last)
  150   format (' Open failure on opened file ', a)
        open_file = 2
        go to 300

  152   write (*, 150) file(1:last)
        open_file = 4
        go to 300

  160   write (*, 170) file(1:last)
  170   format (' Write failure on opened file ', a)
        open_file = 1
        go to 300
 
  180   write (*, 190) file(1:last)
  190   format (' Read failure on opened file ', a)
        open_file = 3

  300   continue
        return
        end

