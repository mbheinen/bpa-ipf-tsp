C    @(#)next_brp_alt.f	20.3 2/13/96
c***********************************************************
c***********************************************************
c***  "baseinit" now initializes as follows:
c***  obrnch_nxt(0) = 0
c***  obrtype(0) = 0
c***  so "if" tests are not needed, e.g.
c        if ( np .gt. 0 ) np = obrnch_nxt(np)
c        if ( np .gt. 0 ) then
c           if ( obrtype(np) .eq. 1 ) np = obrnch_nxt(np)
c        endif
c***********************************************************

        integer function next_brp_alt(next_p)
        implicit none

        include 'ipfinc/parametr.inc'
        include 'ipfinc/alt_case.inc'

        integer np, next_p

        save

        np = next_p
        np = obrnch_nxt(np)
        if ( obrtype(np) .eq. 1 ) np = obrnch_nxt(np)
        next_brp_alt = np
        return
        end
