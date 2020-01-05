C    @(#)getoldtb.f	20.3 2/13/96
        subroutine getoldtb (busnam, basekv, oldjtb, oldltyp, oldityp, 
     &                       status)
        integer oldjtb, oldltyp, oldityp, status
        character busnam *(*)
        real basekv
c                                                                       
c       This routine locates the entity in OLDTBX and returns its
c       operating state.
c
c       Input: busnam  - bus name for hash location
c              basekv  - base kv for hash location
c              oldltyp - original ltyp
c              oldityp - original ityp
c              status  - 0 means bus not found in OLDTBX
c                        1 means bus found

        include 'ipfinc/parametr.inc'
        include 'ipfinc/oldtbx.inc'

        integer find_tbx
c         
c       Reset to original state                                         
c                                                                       
        oldjtb = find_tbx (busnam,basekv)
        if (oldjtb .gt. 0) then
           status = 1
           oldltyp = oldtbx(1,oldjtb)
           oldityp = oldtbx(7,oldjtb)                                    
        else
           status = 0
        endif
        return
        end
