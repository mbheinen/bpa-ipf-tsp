C    @(#)linflorpt.f	20.1 2/13/96
C****************************************************************
C
C   File: linflorpt.f
C
C   Purpose: Routine to obtain a filtered loss sensitivity report.
C
C   Author: Walt Powell  Date: 15 July 1994
C   Called by: get_orpt22.f
C
C****************************************************************
C
        subroutine linflorpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/alpha.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/ordsta.inc'

        return
        end
