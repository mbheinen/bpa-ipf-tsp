C    @(#)swapsrt2.f	20.3 2/13/96
C****************************************************************
C
C       File: swapsrt2.f
C       Purpose: Routine to swap ownership entities in array sort
C                pertaining to array lowntie().
C
C       Author: Walt Powell  Date: 6 Jul 1995
C                            Modified: 
C       Called by:
C
C****************************************************************
	subroutine swapsrt2 (m, n)
        integer m, n


        common /scratch/ sort(1000)
        integer sort

        i = sort(m)
        sort(m) = sort(n)
        sort(n) = i

        return
        end
