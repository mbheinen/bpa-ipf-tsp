C    @(#)close_file.f	20.3 2/13/96
        subroutine close_file (unit)
        integer unit

	include 'ipfinc/lfiles.inc'

        logical status
        character file*60

        inquire (unit=unit, opened=status)
	if (status) then
           inquire (unit=unit, name=file)
           close (unit)
           last = lastch(file)
           write (dbug, 100) file(1:last), unit
  100      format (' File ', a, ' closed on unit No. ', i4)
        endif
        return
	end
