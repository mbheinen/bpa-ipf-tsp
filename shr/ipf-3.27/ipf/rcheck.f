C    @(#)rcheck.f	20.3 2/13/96
      subroutine rcheck (imin,imax,inum)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/elim2.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/red2.inc'
C
      ix0 = 200
      inum = 0
      do 470 ncn = imin,imax
      i = indexx(ncn+ix0)
      if (i.eq.0) go to 470
      inum = inum + 1
      inext = i
      icheck = 0
  280 if (ich1(inext).eq.0) go to 290
      inum = inum + 1
      icheck = icheck + 1
      inext = ich1(inext)
      go to 280

  290 if (ich2(inext).eq.0) go to 300
      icheck = icheck - 1
      inext = ich2(inext)
      go to 290

  300 if (icheck.ne.0) write (dbug,310) i,icheck
  310 format (' ERROR - NODE ',i4,' ICHECK ',i3)
  470 continue
      return
      end
