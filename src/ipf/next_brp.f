C    @(#)next_brp.f	20.3 2/13/96
c***********************************************************
c***********************************************************
c***  "baseinit" now initializes as follows:
c***  brnch_nxt(0) = 0
c***  brtype(0) = 0
c***  so "if" tests are not needed, e.g.
c        if ( np .gt. 0 ) np = brnch_nxt(np)
c        if ( np .gt. 0 ) then
c           if ( brtype(np) .eq. 1 ) np = brnch_nxt(np)
c        endif
c***********************************************************

        integer function next_brp(next_p)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/branch.inc'

        save

        np = next_p
        np = brnch_nxt(np)
        if ( brtype(np) .eq. 1 ) np = brnch_nxt(np)
        next_brp = np
        return
        end
