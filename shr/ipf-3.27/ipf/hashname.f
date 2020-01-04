C    @(#)hashname.f	20.3 2/13/96
c
c This routine is part of IPS2IPF
c
        integer function hashname (name)
        character*(*)  name

c       compute hash value of name

        include 'ipfinc/ips2ipf.inc'

        integer        k, i

        last = len(name)
        k = 0
        do i = 1, last
           k = mod (k + k + ichar (name(i:i)), HASHSIZE)
        end do
        hashname = mod (k, HASHSIZE) + 1

        return
        end

