C    @(#)swpvltdif.f	20.3 2/13/96
C****************************************************************
C
C   File: swpvltdif.f
C
C   Purpose: Routine to swap indices for kdiff(p) and kdiff(q)
c
c            "key" denotes the interpretation of kdiff(*,*)
c              1  = interpret as bus indices.
c              2  = interpret as branch indices.
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: vltdifrpt.f, lfodifrpt.f
C
C****************************************************************
	subroutine swpvltdif (p, q)
        integer p, q

	include 'ipfinc/parametr.inc'

        common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS), 
     &                   fdiff(6,MAXBUS), cdiff(2,MAXBUS)
        character cdiff * 1

        i = ixref(p)
        ixref(p) = ixref(q)
        ixref(q) = i

        return
        end        
