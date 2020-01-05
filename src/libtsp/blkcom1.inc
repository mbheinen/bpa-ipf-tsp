c    @(#)blkcom1.inc	4.3 9/21/93
c     blkcom1.inc
c     Revisions:
c     Jun/10/92 - DEM:
c    
c     Assigned name BLK1A to previous blank common
c     Renamed common BLKC to BLK1B
c
      common /blk1a/ link, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11,
     &               l12, l13, l15, l22, l23, nus11, ireuse, errflag
      common /blk1b/ swcas, scase, ver                                  !dem
      character*10 swcas, scase
      character*13 ver
      integer link, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12,
     &              l13, l15, l22, l23, nus11, ireuse, errflag

c     end of blkcom1.inc
