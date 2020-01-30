C    @(#)link56.inc	4.3 9/21/93
C    Begin LINK56.INC
c
c    Revs:
c    May/06/92 - DEM: Added AUXFMT flag for tabular (aux) file format
c    Mar/10/92 - DEM: Redimensioned BASEKV by parameter instead of
c                     specific value
c        -
      common/link56/ ibxyz,basekv(maxkv),isg,nmx,ntoto,ntotd,izdotz,    !dem
     1               zscale,icont,auxfmt
      character auxfmt*3
      integer ibxyz, isg, nmx, ntoto, ntotd, izdotz, icont
      real basekv, zscale 
C     end LINK56.INC
