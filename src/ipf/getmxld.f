C    @(#)getmxld.f	20.3 2/13/96
C****************************************************************
C
C   File: gtmaxld.f
C
C   Purpose: Routine to loop through all branches between terminal
c            buses kx() and ky() and identify the most critically
c            loaded branch element.
C
C   Input parameters:
c         ptr     - branch index
c
c   Output parameters:
c         ptrx    - branch index of critical element
c         lastptr - branch index of last branch element belonging 
c                   to the set kx() and ky()
c         loading - line flow loading expressed in percent of rating
c         rating  - if negative : line current in amps
c                   if positive : transformer MVA
C         tag     - 'N', 'T', 'E', 'B' denoting rating which applies.
c
c   Author: Walt Powell  Date: 14 December 1992
C   Called by: lfodifrpt.f
C
C****************************************************************
C
        subroutine getmxld (ptr, ptrx, lastptr, loading,
     &                      rating, rattag)
        character rattag * 1
        integer loading, rating, ptr, ptrx

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
c	Global variables used:
c		None
        include 'ipfinc/bus.inc'
c	Global variables used:
c		None
        include 'ipfinc/area.inc'
c	Global variables used:
c		None
        include 'ipfinc/owncom.inc'
c	Global variables used:
c		None
        include 'ipfinc/alpha.inc'
c	Global variables used:
c		None
        include 'ipfinc/sortuvov.inc'
c	Global variables used:
c		None
        include 'ipfinc/branch.inc'
c	Global variables used:
c		brtype, brnch_nxt, ky, brid
        include 'ipfinc/prt.inc'
c	Global variables used:
c		None

        integer p
        real lnload

        k2 = ky(ptr)
        ptrx = ptr
        lastptr = ptr
        p = ptr
        loading = 0
        rattag = ' '
        do while (p .gt. 0 .and. (ky(p) .eq. k2))
           ltyp = brtype(p)
           if (ltyp .eq. 1) then
              call gtlnload (p, caseld, lnload, ratthr, 
     1                       ratbtl, ratllf, ratnom)
              lastptr = p
              p = brnch_nxt(p)
              do while (p .gt. 0 .and. 
     &                 (ky(p) .eq. k2 .and. brid(p) .eq. brid(ptr)))
                 ltyp = brtype(p)
                 if (ltyp .eq. 2 .or. ltyp .eq. 3 .or. 
     &               ltyp .eq. 5 .or. ltyp .eq. 5 .or. 
     &               ltyp .eq. 7 .or. ltyp .eq. 8) then
                    call gtlnload (p, caseld, lnload, ratthr, 
     &                             ratbtl, ratllf, ratnom)
                    if (loading .lt. int(lnload)) then
                       if (ratnom .eq. 0.0) ratnom = 9999.0
                       if (ratbtl + ratthr + ratllf .eq. 0.0) then
                          loading = int(lnload)
                          rating = int(ratnom)
                          rattag = 'N'
                       else
                          if (ratthr .eq. 0.0) ratthr = 9999.0
                          if (ratbtl .eq. 0.0) ratbtl = 9999.0
                          if (ratllf .eq. 0.0) ratllf = 9999.0
                          ratmin = amin1 (ratthr, ratbtl, ratllf)
                          if (ratthr .eq. ratmin) then
                             loading = int(lnload)
                             rating = int(ratthr)
                             rattag = 'T'
                          else if (ratbtl .eq. ratmin) then
                             loading = int(lnload)
                             rating = int(ratbtl)
                             rattag = 'B'
                          else
                             loading = int(lnload)
                             rating = int(ratllf)
                             rattag = 'E'
                          endif
                       endif
c
c                      Flag "amps" as negative rating
c
                       if (ltyp .eq. 2 .or. ltyp .eq. 3 .or.
     &                     ltyp .eq. 7 .or. ltyp .eq. 8) 
     &                    rating = -rating 
                       ptrx = p
                    endif
                 endif
                 lastptr = p
                 p = brnch_nxt(p)
              enddo
           else if (ltyp .eq. 2 .or. ltyp .eq. 3 .or.
     &              ltyp .eq. 5 .or. ltyp .eq. 6 .or.
     &              ltyp .eq. 7 .or. ltyp .eq. 8) then
              call gtlnload (p, caseld, lnload, ratthr, 
     1                       ratbtl, ratllf, ratnom)
              if (loading .lt. int(lnload)) then
                 if (ratnom .eq. 0.0) ratnom = 9999.0
                 if (ratbtl + ratthr + ratllf .eq. 0.0) then
                    loading = int(lnload)
                    rating = int(ratnom)
                    rattag = 'N'
                 else
                    if (ratthr .eq. 0.0) ratthr = 9999.0
                    if (ratbtl .eq. 0.0) ratbtl = 9999.0
                    if (ratllf .eq. 0.0) ratllf = 9999.0
                    ratmin = amin1 (ratthr, ratbtl, ratllf)
                    if (ratthr .eq. ratmin) then
                       loading = int(lnload)
                       rating = int(ratthr)
                       rattag = 'T'
                    else if (ratbtl .eq. ratmin) then
                       loading = int(lnload)
                       rating = int(ratbtl)
                       rattag = 'B'
                    else
                       loading = int(lnload)
                       rating = int(ratllf)
                       rattag = 'E'
                    endif
                 endif
                 ptrx = p
c
c                Flag "amps" as negative rating
c
                 if (ltyp .eq. 2 .or. ltyp .eq. 3 .or.
     &               ltyp .eq. 7 .or. ltyp .eq. 8) rating = -rating 
              endif
              lastptr = p
              p = brnch_nxt(p)
           else
              lastptr = p
              p = brnch_nxt(p)
           endif
        enddo
        return
        end
