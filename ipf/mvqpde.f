C    @(#)mvqpde.f	20.4 11/12/98
        subroutine mvqpde(ibus, type, ix)
        integer ibus, ix
        character type *(*)
c
c       Delete pqcurve(ix) record and store in array delete(ndel)
c
c       Input parameters:
c
c          ibus : bus index
c          type : 'QP', 'QX', or 'QN'
c          ix   : index to pqcurves(ix)
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/delete.inc'
        include 'ipfinc/pqcurves.inc'

        character record*120

        call bcdqpd (ix, record) ! obtain QP-data record
        call stor_del (record, ic)  ! store this record in delete()
        call bcdqxd (ix, record) ! obtain QX-data record
        call stor_del (record, ic)  ! store this record in delete()
        call bcdqnd (ix, record) ! obtain QN-data record
        call stor_del (record, ic)  ! store this record in delete()
c
c       Flag ix as deleted
c
        pqbusptr(ix) = 0
        return
        end
