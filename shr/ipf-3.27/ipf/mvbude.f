C    @(#)mvbude.f	20.3 2/13/96
        subroutine mvbude (ibus, ic)
        integer ibus, ic
c 
c       Delete bus record(ibus) and store in array delete(ndelete)
c
c       Input parameters:
c
c          ibus : current bus index
c          ic   : index to chgcrd()
c 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/prt.inc'

        integer   status, rename_bus
        character record * 120, bus_name * 8

        call bcdbus (ibus, record)  ! obtain bus record
        call stor_del (record, ic)  ! store this record in delete()
c
c       Remove bus from bus hash table by renaming
c
        bus_name = srtlst
        bus_base = 9999.0
        status = rename_bus (ibus, bus_name, bus_base)
c
c       Flag ibus as deleted
c
        do i = 1, 11
           kbsdta(i,ibus) = 0
        enddo
        capcor(1,ibus) = 0.0
        capcor(2,ibus) = -9.0e10

        return
        end
