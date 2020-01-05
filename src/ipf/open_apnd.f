C    @(#)open_apnd.f	20.1 4/27/95
C****************************************************************
C
C       File: open_apnd.f
C       Purpose: Opens file for append given unit and name.
c                if file does not exist, opens file for write.
C
C       Author: Jay Coleman  Date: 21 APR 94
C
c       Return status:
c               0 - successful
c               1 - unsuccessful
C****************************************************************
C
	integer function open_apnd (unit, file)
        character file *(*)
        integer unit

        logical does_exist
        character * 5   line

        integer open_file

        does_exist = .false.
        open_apnd = 0  ! Set default return status "successful"

        last = lastch(file)
        if (file .ne. ' ') then
           inquire (file=file(1:last), exist=does_exist)
        endif

	if (does_exist) then
           if (is_it_vms() .eq. 1) then
              open( unit = unit, file = file(1:last),
     &              status = 'UNKNOWN', form   = 'FORMATTED',
     &              access = 'APPEND',
c    &              carriagecontrol = 'LIST',
     &              iostat = iostat, err = 910 )
           else
              open( unit = unit, file = file(1:last),
     &              status = 'UNKNOWN', form   = 'FORMATTED',
     &              iostat = iostat, err = 910 )
              do while ( .true. )
                 read( unit, '(a)', err = 920, end = 100 ) line
              enddo
  100         continue
           endif
           write( unit, 111, err = 930 )
  111      format('.',/,'. ************** APPENDING NEXT DATA SET',
     &            ' **********************',/,'.' )
        else
           istat = open_file (unit, file, 'F', 'W', iostat)
           if ( istat .ne. 0 ) open_apnd = 1
        endif

        return


  910   write (*, 911) file(1:last)
  911   format (' Open failure on file ', a)
        open_apnd = 1
        return

  920   write (*, 921) file(1:last)
  921   format (' Read failure on opened file ', a)
        open_apnd = 1
        return

  930   write (*, 931) file(1:last)
  931   format (' Write failure on opened file ', a)
        open_apnd = 1
        return
        end

