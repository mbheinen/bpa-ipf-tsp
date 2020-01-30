C    @(#)link_own.f	20.3 2/13/96
	integer function link_own (iown1, iown2, numtiexx)
c
c       This subroutine links iown2 and iown2 in threaded ownership
c       lists.
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/ownhash.inc'
        include 'ipfinc/owntie.inc'

        integer q, oldq, p, oldp
c
c       link owner1-owner2
c
c       own1_inx(q)     -> points to first own1 entity in OWN1_OWN2. 
c                          Length is NUMOWN.
c       own1_nxt(q)     -> points to next own1 entity in OWN1_OWN2. 
c                          Length is NUMREF.
c       own1_ref(q)     -> points to first own1-own2 entity in 
c                          OWN1_TIE_PTR. Length is NUMREF.
c       own1_own2(q)    -> stores own1-own2 entity. Length is NUMREF.
c       own1_tie_ptr(q) -> points to first own1-own2 entity in OWNTIE. 
c                          Length is NUMPTR.
c       own1_tie_nxt(q) -> points to next own1_own2 entity in OWNTIE. 
c                          Length is NUMPTR.
        oldq = 0
        q = own1_inx(iown1)
        do while (q .gt. 0 .and. (own1_own2(q) .lt. iown2))
           oldq = q
           q = own1_nxt(q)
        enddo
        if (q .eq. 0 .or. (own1_own2(q) .ne. iown2)) then
           numref = numref + 1
           if (oldq .eq. 0) then
              own1_inx(iown1) = numref
           else
              own1_nxt(oldq) = numref
           endif
           own1_own2(numref) = iown2
           own1_nxt(numref) = q
           own1_ref(numref) = 0
           q = numref
        endif
c
c       Link with OWNTIE
c
        numptr = numptr + 1
        own1_tie_ptr(numptr) = numtiexx
        oldp = 0
        p = own1_ref(q)
        do while (p .ne. 0)
           oldp = p
           p = own1_tie_nxt(p)
        enddo
        if (oldp .eq. 0) then
           own1_ref(q) = numptr 
        else
           own1_tie_nxt(oldp) = numptr 
        endif
        link_own = 0
        return 
        end
