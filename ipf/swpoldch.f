C    @(#)swpoldch.f	20.3 2/13/96
C****************************************************************
C
C   	File: swpoldch.f
C
C   	Purpose: Swap routine for sorting oldchg records to faciliate 
C                change reduction. 
C                The change records are sorted by data element.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: qiksrt.f through savechgs.f
C
C****************************************************************
C
        subroutine swpoldch (c1, c2) 
        integer c1, c2

        include 'ipfinc/parametr.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/oldchg.inc'

        common /scratch/ array(MAXCHG)
        integer array

        ic = array(c1)
        array(c1) = array(c2)
        array(c2) = ic
        
        return
        end
