C    @(#)c_redchg.f	20.3 2/13/96
C****************************************************************
C
C   	File: c_redchg.f
C
C   	Purpose: 1. Reduce ".#" interactive command records
C
C   	Author: Walt Powell            Date: 24 December 1992
C   	Called by: redchgs.f
C
C****************************************************************
C
      	subroutine c_redchg (nx, nxdx)
        integer nx, nxdx(*)
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/changr.inc'
      	include 'ipfinc/oldchg.inc'
      	include 'ipfinc/prt.inc'

        common /scratch/ array(MAXCHG)
        integer array

        ix = 1
        do while (ix .lt. nx)
           i = array(nxdx(ix))
           if (oldchg(i)(126:126) .ne. 'E') then           
              jx = ix + 1
              do while (jx .le. nx)
                 j = array(nxdx(jx))           
                 if (oldchg(j)(126:126) .ne. 'E') then           
                    read (oldchg(i)(127:128), '(bz, i2)') nfile1
                    read (oldchg(j)(127:128), '(bz, i2)') nfile2
                    komp = kompr (oldchg(i)(1:80), oldchg(j)(1:80),
     &                            komp)
                    if (komp .eq. 0) komp = nfile1 - nfile2
                    if (komp .eq. 0) then
c
c                      Delete oldchg(j)
c
                       oldchg(j)(126:126) = 'E'
                    endif
                 endif
                 jx = jx + 1
              enddo
           endif
           ix = ix + 1
        enddo

        return
        end
