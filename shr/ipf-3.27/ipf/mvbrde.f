C    @(#)mvbrde.f	20.3 2/13/96
        subroutine mvbrde (ibus, ic, pold, ptr)
        integer ibus, ic, pold, ptr
 
c       Delete branch record and store in array delete(ndelete)
c
c       Input parameters:
c
c          ibus : bus index
c          ic   : index to chgcrd()
c          pold : pointer TO current pointer
c          ptr  : current branch pointer
c 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/delete.inc'

        character record * 120

        if (brtype(ptr) .ne. 1) then
           call bcdbrn (ptr, record)   ! obtain branch bus record
c
c          Flag transposed records with a "T" in column 3
c
           if (brnch_ptr(ptr) .lt. 0) record(3:3) = 'T'

           call stor_del (record, ic)  ! store this record in delete()
        endif
c
c       Remove ptr from pointer scheme pold -> ptr -> brnch_nxt(ptr)
c 
        if (pold .eq. 0) then
           kbsdta(16,ibus) = brnch_nxt(ptr)
        else
           brnch_nxt(pold) = brnch_nxt(ptr)
        endif
c
c       Flag ptr as deleted
c
        brnch_ptr(ptr) = 0
        brnch_nxt(ptr) = -1
        return
        end
