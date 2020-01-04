C    @(#)swpchgls.f	20.3 2/13/96
C****************************************************************
C
C   	File: swpchgls.f
C
C   	Purpose: Swap routine for sorting chgcrd records to restore
C                original order/
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: qiksrt.f via chglis.f
C
C****************************************************************
C
        subroutine swpchgls (c1, c2) 
        integer c1, c2

        include 'ipfinc/parametr.inc'
        include 'ipfinc/changr.inc'

        common /scratch/ array(MAXCHG)
        integer array

        ic = array(c1)
        array(c1) = array(c2)
        array(c2) = ic
        
        return
        end
