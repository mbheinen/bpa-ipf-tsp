C    @(#)check_name.f	20.3 2/13/96
c
c This routine is part of IPS2IPF
c
        integer function check_name (name)
        character*(*)  name

        include 'ipfinc/ips2ipf.inc'

        integer h, hashbskv, p

        read (name(9:12), '(bz, f4.0)') basekv
        h = hashbskv (name(1:8), basekv)
        p = htable(h)
        do while (p .gt. 0)         !search for existing entities
           if (name(1:8) .ne. oldbus(p)(1:8) .or. 
     &         basekv .ne. oldbase(p)) then
              p = nextptr(p)
           else
              p = -p                   
           endif
        enddo
        if (p .eq. 0) then
           numbus = numbus + 1
           oldbus(numbus) = name
           oldbase(numbus) = basekv
           nextptr(numbus) = htable(h)
           htable(h) = numbus
           check_name = numbus
        else
           check_name = p               ! p < 0 flags duplicate entity!
        endif
        return
        end
