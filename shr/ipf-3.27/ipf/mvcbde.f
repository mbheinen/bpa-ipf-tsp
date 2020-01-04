C    @(#)mvcbde.f	20.3 2/13/96
        subroutine mvcbde (ibus, ic, pold, ncb)
        integer ibus, ic, pold, ncb
 
c       Delete continuation bus record(ncb) and store in array 
c       delete(ndelete)
c
c       Input parameters:
c
c          ibus : current bus index
c          ic   : index to chgcrd()
c          pold : pointer TO current pointer
c          ncb  : index to bctbl(*,ncb)
c 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/cbus.inc'
        include 'ipfinc/delete.inc'

        character record * 120

        call bcdcbs (ncb, record)   ! obtain continuation bus record
        call stor_del (record, ic)  ! store this record in delete()
c
c       Remove ncb from pointer scheme pold -> ncb -> bctbl_nxt(ncb)
c 
        if (pold .eq. 0) then
           kbsdta(15,ibus) = bctbl_nxt(ncb)
        else
           bctbl_nxt(pold) = bctbl_nxt(ncb)
        endif
c
c       Flag ncb as deleted
c
        kbctbl(1,ncb) = 0
        bctbl_nxt(ncb) = -1
        return
        end
