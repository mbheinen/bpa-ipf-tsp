C    @(#)mvxdde.f	20.3 2/13/96
        subroutine mvxdde(ibus, ic, ix)
        integer ibus, ix
c
c       Delete xdata(*,ix) record and store in array delete(ndelete)
c
c       Input parameters:
c
c          ibus : bus index
c          ic   : index to chgcrd()
c          ix   : index to xdata(*,ix)
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/delete.inc'
c	Global variables used:
c		None
        include 'ipfinc/xdata.inc'
c	Global variables used:
c		xdata(r*8), xdt_flag

        character record * 120

        xdt_flag = .false.   ! Reset X-data flag for interactive
C                            ! status

        call bcdxdt (ix, record)    ! obtain x-data record
        call stor_del (record, ic)  ! store this record in delete()
c
c       Flag ix as deleted
c
        xdata(1,ix) = 0.0d0
        return
        end
