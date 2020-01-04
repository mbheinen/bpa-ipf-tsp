C    @(#)find_name.f	20.3 2/13/96
c
c This routine is part of IPS2IPF
c
        integer function find_name (name)
        character name *(*)

        include 'ipfinc/ips2ipf.inc'

        integer h, hashname, p

        h = hashname (name)
        p = htable(h)
        do while (p .gt. 0 .and. (name .ne. rename(1,p)))
           p = nextptr(p)
        enddo

        find_name = p

        return
        end
