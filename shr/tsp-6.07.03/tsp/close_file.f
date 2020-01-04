C    %W% %G%
        subroutine close_file (unit)
        integer unit

        logical status
        character file*60

        inquire (unit=unit, opened=status)
	if (status) then
           inquire (unit=unit, name=file)
           close (unit)
           last = lastch(file)
           if (keybrd(1) .ne. 0) then
              write (*, 100) file(1:last), unit
  100         format (' File ', a, ' closed on unit No. ', i4)
           endif
        endif
        return
	end
