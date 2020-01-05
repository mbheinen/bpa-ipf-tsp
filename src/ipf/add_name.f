C    @(#)add_name.f	20.3 2/13/96
c
c This routine is part of IPS2IPF
c
        integer function add_name (name)
        character*(*)  name

        include 'ipfinc/ips2ipf.inc'

        integer h, hashname, p

        h = hashname (name)
        p = htable(h)
        do while (p .gt. 0)         !search for existing entities
           if (name .ne. rename(1,p)) then
              p = nextptr(p)
           else
              p = -p
           endif
        enddo
        if (p .eq. 0) then
           numcvt = numcvt + 1
           rename(1,numcvt) = name
           nextptr(numcvt) = htable(h)
           htable(h) = numcvt
           add_name = numcvt
        else
           add_name = p                 ! p < 0 flags duplicate entity!
        endif
        return
        end

