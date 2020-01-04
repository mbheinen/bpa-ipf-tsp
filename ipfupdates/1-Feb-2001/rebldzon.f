C    %W% %G%
C****************************************************************
C
C     File: rebldzon.f
C
C     Purpose: Routine to rebuild acznam() using zone hashing
C
c     Return code:  n = 0 : Success
c                   N > 0 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: clnuppti.f
C
C****************************************************************
        integer function rebldzon (error)
        integer error

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/zonehash.inc'

        integer bldzone

        error = 0
        rebldzon = 0
c
c       Reinitialize zone hash tables
c
        do nb = 1, MAXCZN
           nextptr_z(nb) = 0
        enddo
        do nb = 1, HASHSIZE_Z
           htable_z(nb) = 0
        enddo
        nztot = 0

        do nb = 1, ntot
          if (bus(nb) .ne. srtlst) then
            iz = bldzone(zone(nb), jarzn(nb))
          endif
        enddo

        return
        end
