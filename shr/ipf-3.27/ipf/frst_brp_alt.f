C    @(#)frst_brp_alt.f	20.3 2/13/96
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

        integer function frst_brp_alt(ifirst_p)
        implicit none

        include 'ipfinc/parametr.inc'
        include 'ipfinc/alt_case.inc'

        integer ip, ifirst_p

        save

        ip = ifirst_p
        if ( obrtype(ip) .eq. 1 ) ip = obrnch_nxt(ip)
        frst_brp_alt = ip
        return
        end
