C    @(#)fnd_ptic.f	20.2 11/11/97
C****************************************************************
C
C     File: fnd_ptic.f
C
C     Purpose: Routine to find PTI hash index given its area number
C
c     Return code:  n > 0 : PTI area hash number
c                   n = 0 : Error -  number not found
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: saveptid.f
C
C****************************************************************
        integer function fnd_ptic (areanum)
        integer areanum

        include 'ipfinc/parametr.inc'

        include 'ipfinc/pti_data.inc'

        integer        h
        logical found

        h = mod (areanum, MAXCAR-1) + 1
        fnd_ptic = htable_a(h)
        return
        end
