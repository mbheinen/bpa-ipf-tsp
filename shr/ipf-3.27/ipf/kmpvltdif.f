C    @(#)kmpvltdif.f	20.3 2/13/96
C****************************************************************
C
C   File: kmpvltdif.f
C
C   Purpose: Routine to compares kdiff(p) with kdiff(q)
c
c            "key" denotes the interpretation of kdiff(*,*)
c              1  = interpret as bus indices.
c              2  = interpret as branch indices.
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: p_report.f
C
C****************************************************************
	integer function kmpvltdif (p, q)
        integer p, q

	include 'ipfinc/parametr.inc'
	include 'ipfinc/qksrt.inc'
	include 'ipfinc/bus.inc'
	include 'ipfinc/branch.inc'
	include 'ipfinc/alt_case.inc'
        
        common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS), 
     &                   fdiff(6,MAXBUS), cdiff(2,MAXBUS)
        character cdiff * 1

        integer p1, p2, op1, op2
        if (p .eq. q) then
           kmpvltdif = 0
        else
           i = ixref(p)
           j = ixref(q)
           idv1 = fdiff(1,i)
           idv2 = fdiff(1,j)
           kmpvltdif = idv2 - idv1        ! Sort dv high to low
           if (kmpvltdif .eq. 0) then
              p1 = kdiff(1,i)
              p2 = kdiff(1,j)
              if (key .eq. 1) then
                 op1 = kdiff(2,i)
                 op2 = kdiff(2,j)
                 if (p1 .gt. 0 .and. p2 .gt. 0) then
                    kmpvltdif = kompr (bus(p1), bus(p2), junk)
                    if (kmpvltdif .eq. 0) then
                       vltdif = 100.0 * (base(p1) - base(p2))
                       kmpvltdif = int (vltdif)
                    endif
                 else if (p1 .gt. 0 .and. p2 .eq. 0) then
                    kmpvltdif = kompr (bus(p1), oldbus(op2), junk)
                    if (kmpvltdif .eq. 0) then
                       vltdif = 100.0 * (base(p1) - oldbase(op2))
                       kmpvltdif = int (vltdif)
                    endif
                 else if (p1 .eq. 0 .and. p2 .gt. 0) then
                    kmpvltdif = kompr (oldbus(op1), bus(p2), junk)
                    if (kmpvltdif .eq. 0) then
                       vltdif = 100.0 * (oldbase(op1) - base(p2))
                       kmpvltdif = int (vltdif)
                    endif
                 else
                    kmpvltdif = kompr (oldbus(op1), oldbus(op2), junk)
                    if (kmpvltdif .eq. 0) then
                       vltdif = 100.0 * (oldbase(op1) - oldbase(op2))
                       kmpvltdif = int (vltdif)
                    endif
                 endif
              else
                 op1 = kdiff(2,i)
                 op2 = kdiff(2,j)
                 if (p1 .gt. 0 .and. p2 .gt. 0) then
                    kmpvltdif = kompr (bus(kx(p1)), bus(kx(p2)), junk)
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (bus(ky(p1)), bus(ky(p2)), 
     &                                    junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (brid(p1), brid(p2), junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = brsect(p1) - brsect(p2)
                    endif
                 else if (p1 .gt. 0 .and. p2 .eq. 0) then
                    kmpvltdif = kompr (bus(kx(p1)), oldbus(okx(op2)), 
     &                                 junk)
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (bus(ky(p1)), oldbus(oky(op2)),
     &                                    junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (brid(p1), obrid(op2), junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = brsect(p1) - obrsect(op2)
                    endif
                 else if (p1 .eq. 0 .and. p2 .gt. 0) then
                    kmpvltdif = kompr (oldbus(okx(op1)), bus(kx(p2)), 
     &                                 junk)
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (oldbus(oky(op1)), bus(ky(p2)),
     &                                    junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (obrid(op1), brid(p2), junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = obrsect(op1) - brsect(p2)
                    endif
                 else
                    kmpvltdif = kompr (oldbus(okx(op1)), 
     &                                 oldbus(okx(op2)), junk)
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (oldbus(oky(op1)), 
     &                                    oldbus(oky(op2)),
     &                                    junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = kompr (obrid(op1), obrid(op2), junk)
                    endif
                    if (kmpvltdif .eq. 0) then
                       kmpvltdif = obrsect(op1) - obrsect(op2)
                    endif
                 endif
              endif
           endif
        endif
        return
        end        
