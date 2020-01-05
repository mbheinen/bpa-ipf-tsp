C    %W% %G%
C*************************************************************
      subroutine outreport (in_buffer, out_buffer, scrfil)
      character in_buffer *(*), out_buffer *(*)
      integer scrfil

      include 'ipfinc/parametr.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/sortuvov.inc'

	character null * 1, linefeed *1, text * 80, capital * 80
        logical gtfltr, found, change_f
	integer o2

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
c
c       Search and align to "WHERE" ...
c
        text = in_buffer
        i = index (capital(text), 'WHERE')  ! Search for WHERE in text
c                                           ! because capital >= argument
        found = .false.
        if (i .gt. 0) then
           i = i + len('WHERE')
           change_f = gtfltr(in_buffer(i:))
           found = .true.
        endif
        if (.not .found) then
           do i = 1, 7
              filter(i) = 0
           enddo
        endif
c
c       Note: expand here when feature added
c
        o2 = index (out_buffer,null)
        return
        end
