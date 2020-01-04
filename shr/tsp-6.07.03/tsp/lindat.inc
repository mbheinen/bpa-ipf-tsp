C    @(#)lindat.inc	4.3 9/21/93
      common/lndatn/lndatn(9,8,500)
      common/lndatc/lndatc(500)
      character * 8 lndatc
      dimension bndatn(9,8,500)
      equivalence (lndatn,bndatn)
      integer lndatn
      real bndatn
