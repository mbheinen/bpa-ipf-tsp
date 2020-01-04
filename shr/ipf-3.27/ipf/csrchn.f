C    @(#)csrchn.f	20.3 2/13/96
      character*1 function csrchn (ctest,clist,nchar,nitem)
C
C     Searches through the CLIST character list for a match with CTEST.
C     NCHAR is the number of characters in CTEST and each item of
C           CLIST.
C     NITEM is the number of items of CLIST.
C     Returns  'Y'  if any matches
C              'N'  if no matches
C
      character*(*) ctest,clist(nitem)
C
      csrchn = 'N'
      do 200 la = 1,nitem
        if (ctest .eq. clist(la)) goto 234
 200  continue
C                Flunked
      return
C                 Passed
 234  csrchn = 'Y'
      return
      end
