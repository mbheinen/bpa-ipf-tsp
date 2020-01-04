C    @(#)pointr.inc	4.3 9/21/93
C     -                POINTR.INC
C
C   Revs:
C     Mar/10/92  -  DEM
C       Redimensioned BASEKV, ROVX & IMLVI by parameter instead of
C         specific value
C     -
      integer MAXINDMTR
      parameter (MAXINDMTR = 1000)
      common /pointr/ basekv(MAXKV), iabort, ibxyz, ifqsw, istop3,      !dem
     1       jdbus(100), jdc, jdctot, jdelet, ldar, ldz, ln, lrep, lrd, !dem
     2       lrr, ls, lsc, mac, rovx(MAXKV), imlv, imlvi(MAXINDMTR)     !dem
C     -                end of POINTR.INC
      integer iabort, ibxyz, ifqsw, istop3, 
     1       jdbus, jdc, jdctot, jdelet, ldar, ldz, ln, lrep, lrd,
     2       lrr, ls, lsc, mac, imlv, imlvi
      real basekv, rovx
