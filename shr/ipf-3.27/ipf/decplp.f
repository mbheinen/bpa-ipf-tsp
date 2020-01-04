C    @(#)decplp.f	20.3 2/13/96
      subroutine decplp (status, z)
C ***                                                                  *
C *** This routine uses a general purpose LP to optimize LTC phase     *
C *** shifters. P_alpha relates to ideal branch power flow in          *
C *** accordance with TXTIE; alpha relates to LTC phase shift.         *
C ***                                                                  *
C *** Input:                                                           *
C ***                                                                  *
C *** Output:                                                          *
C ***                                                                  *
C *** STATUS -  1 : 'Optimum'                                          *
C ***           2 : 'Infeasible'                                       *
C ***           3 : 'Unbounded'                                        *
C ***           4 : 'The maximum size of the inverse has been exceeded *
C ***           5 : 'The maximum number of iterations has been reached *
C ***           6 : 'Either the AA vector is full or the number of     *
C ***                constraints is exceed the maximum'                *
C ***           7 : 'Still inaccurate after reinverting'               *
C *** Z - Value of objective function.                                 *
C ***                                                                  *
C *** The LP used is copied from the book by A. Land and S. Powell:    *
C *** "Fortran Codes for Mathematical Programming", 1978, John Wiley   *
C *** and Sons, N.Y.                                                   *
C ***                                                                  *
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/area.inc'
c	Global variables used:
c		tie(r*8)
      include 'ipfinc/aref.inc'      
c	Global variables used:
c		aa(r*8), jcol, irow
      include 'ipfinc/blank.inc'
c	Global variables used:
c		ntota
      include 'ipfinc/bus.inc'
c	Global variables used:
c		e(r*8)
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		idswa, ntotx
      include 'ipfinc/ikk.inc'
c	Global variables used:
c		ikk
      include 'ipfinc/intbus.inc'
c	Global variables used:
c		None
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/ntotzt.inc'
c	Global variables used:
c		ntotzt, alp
      include 'ipfinc/optim1.inc'
c	Global variables used:
c		txtie(r*8)
      include 'ipfinc/prt.inc'
c	Global variables used:
c		None
      include 'ipfinc/smallp.inc'
c	Global variables used:
c		ir, tir, neginv, ngerow, istate, 
c		mnow, isdone, inrev, newx, newy,
c		tol, morepr, isbnd, irmax, itrmax,
c		r, yaminc, obj, s, b, c, bound, small,
c		big, x
      include 'ipfinc/slnopt.inc'
c	Global variables used:
c		None
      include 'ipfinc/tran.inc'
c	Global variables used:
c		tran(r*4)
 
      integer row, col, status, debug
      real cp
      double precision cs, sn, delang, gkm, gmk, bkm, bmk, anew
c
c     cp was used before a value was assigned.
c     initialize to zero.
c
      cp = 0.0
C ***                                                                  *
C *** The following tolerances are defined for double-precision words  *
C *** (64-bits).  See page 186 for 32-bit equivalent tolerances.       *
C ***                                                                  *
      big = 1.0e11
      small = 1.0e - 9
      tol(1) = 1.0e - 6
      tol(2) = 1.0e - 5
      tol(3) = 1.0e - 6
      tol(4) = 1.0e - 5
      tol(5) = 1.0e - 5
      tol(6) = 1.0e - 5
      tol(7) = 1.0e - 5
      tol(8) = tol(5) * 10.0
 
      isdone = 0
      debug = idswa
      kerrsw = 0
 
      nlp = ntotx - ntotzt
      if (nlp .eq. 0) go to 280
      m = 5 * nlp
      n = 8 * nlp
      isbnd = n
 
      if (m .gt. MAXM .or. n .gt. MAXN .or. isbnd .gt. MAXN) then
         write (dbug,100) MAXM, MAXN
  100    format (' EXCEEDED CAPACITY OF LP -',
     1           i5, ' CONSTRAINTS AND ',  i5, ' VARIABLES')
         isdone = 1
         go to 270
      endif
 
      morepr = 1
      itrmax = 0
      irmax = 0

C     Initialize arrays

      bndj = -1.0
      if (isbnd .eq. -1) bndj = 1.0
 
      if (itrmax .eq. 0) then
         ijk = isbnd
         if (ijk .eq. -1) ijk = n
         itrmax = 3 * (m + n + ijk)
      endif
 
      if (irmax .eq. 0) then
         irmax = itrmax
      endif
 
      do 110 i = 1, n
         c(i) = 0.0
         bound(i) = bndj
  110 continue
 
      do 120 i = 1, m
         b(i) = big
         s(i) = 1.0
  120 continue
C ***                                                                  *
C *** Implement LTC controls "softly", i.e., impose penalties and      *
C *** optimize. The penalty factors are negative because this          *
C *** algorithm maximizes the objective function.                      *
C ***                                                                  *
      ca = -0.01
      if (iopton(21) .eq. 0) then
        cp = -1000.0        ! BPA option - soft penalties
      else
        cp = 0.0            ! WSCC option - no soft penalties
      endif 
C *** Load A and B elements. SI(*) flags equality-inequality           *
C *** status. Non-zero A elements must be loaded in ascending          *
C *** order:  A(1,1),A(1,3),...,A(2,1),...  At least one               *
C *** element in each row must be specified, even if that              *
C *** element is zero!                                                 *
C ***                                                                  *
C *** 1 means <=                                                       *
C *** 2 means =>                                                       *
C *** 0 means =                                                        *
 
      next = 0
      look = 1
      row = 0
      col = 0
C ***                                                                  *
C *** Load constraints.                                                *
C ***                                                                  *
      do 150 i = 1, nlp
         row = i
         irow(row) = look
C ***                                                                  *
C ***    Fetch LTC values.                                             *
C ***                                                                  *
         kta = ntotzt + i - 1
         ntx = ikk(5,kta)
         ltc = txtie(7,ntx)
         if (ltc .le. 0) call erexit
C ***                                                                  *
C ***    Impose hard limits on phase angles.                           *
C ***                                                                  *
         col = 2 * nlp + 2 * i - 1
         bound(col) = dmax1 (0.0d0, dble(tran(7,ltc)))
         bound(col+1) = -dmin1 (0.0d0, dble(tran(8,ltc)))
 
         do 130 j = 1, nlp
            col = 2 * j - 1
            jcol(look) = col
            aa(look) = alp(i,j)
            look = look + 1
 
            col = col + 1
            jcol(look) = col
            aa(look) = -alp(i,j)
            look = look + 1
 
  130    continue
 
         col = 2 * nlp + 2 * i - 1
         jcol(look) = col
         aa(look) = -1.0
         look = look + 1
 
         col = col + 1
         jcol(look) = col
         aa(look) = 1.0
         look = look + 1
 
         b(row) = blp(i)
         s(row) = 0.0
 
         if (debug .ne. 0) then
            write (dbug,140) row, i, b(row), s(row)
  140       format (' DECPLP - P_alpha   ROW: I, B, S ', 2i5, 2e10.3)
         endif
 
  150 continue
C ***                                                                  *
C *** Impose P_limits with linear penalties                            *
C ***                                                                  *
      do 180 i = 1, nlp
 
         row = nlp + 2 * i - 1
         irow(row) = look
 
         col = 2 * i - 1
         jcol(look) = col
         aa(look) = 1.0
         look = look + 1
 
         col = col + 1
         jcol(look) = col
         aa(look) = -1.0
         look = look + 1
 
         col = 4 * nlp + 2 * i - 1
         c(col) = cp
         jcol(look) = col
         aa(look) = -1.0
         look = look + 1
 
         kta = ntotzt + i - 1
         ntx = ikk(5,kta)
         ltc = txtie(7,ntx)
 
         k1 = txtie(1,ntx)
         k2 = txtie(2,ntx)
 
         b(row) = tran(4,ltc)
         s(row) = 1.0
 
         if (debug .ne. 0) then
            write (dbug,160) row, i, b(row), s(row)
  160       format (' DECPLP - Del_p_max ROW: I, B, S ', 2i5, 2e10.3)
         endif
 
         row = row + 1
         irow(row) = look
 
         col = 2 * i - 1
         jcol(look) = col
         aa(look) = 1.0
         look = look + 1
 
         col = col + 1
         jcol(look) = col
         aa(look) = -1.0
         look = look + 1
 
         col = 4 * nlp + 2 * i
         c(col) = cp
         jcol(look) = col
         aa(look) = 1.0
         look = look + 1
 
         b(row) = tran(5,ltc)
         s(row) = 2.0
 
         if (debug .ne. 0) then
            write (dbug,170) row, i, b(row), s(row)
  170       format (' DECPLP - Del_p_min ROW: I, B, S ',2i5,2e10.3)
         endif
 
  180 continue
C
C     Depending upon the option PHASE_SHIFTER_BIAS, bias the phase shift
C     either 0 degrees (BPA) or the initial degrees (WSCC) using linear
C     linear penalties.
         
      do 210 i = 1, nlp
 
         kta = ntotzt + i - 1
         ntx = ikk(5,kta)
         ltc = txtie(7,ntx)
 
         row = 3 * nlp + 2 * i - 1
         irow(row) = look
 
         col = 2 * nlp + 2 * i - 1
         jcol(look) = col
         aa(look) = 1.0
         look = look + 1
 
         col = col + 1
         jcol(look) = col
         aa(look) = -1.0
         look = look + 1
 
         col = 6 * nlp + 2 * i - 1
         c(col) = ca
         jcol(look) = col
         aa(look) = -1.0
         look = look + 1
 
         b(row) = txtie(4,ntx)
         s(row) = 1.0
 
         if (debug .ne. 0) then
            write (dbug,190) row, i, b(row), s(row)
  190       format (' NRDCLP - A_max_exc ROW: I, B, S ',2i5,2e10.3)
         endif
 
         row = row + 1
         irow(row) = look
 
         col = 2 * nlp + 2 * i - 1
         jcol(look) = col
         aa(look) = 1.0
         look = look + 1
 
         col = col + 1
         jcol(look) = col
         aa(look) = -1.0
         look = look + 1
 
         col = 6 * nlp + 2 * i
         c(col) = ca
         jcol(look) = col
         aa(look) = 1.0
         look = look + 1
 
         b(row) = txtie(4,ntx)
         s(row) = 2.0
 
         if (debug .ne. 0) then
            write (dbug,200) row, i, b(row), s(row)
  200       format (' DECPLP - A_min_exc ROW: I, B, S ', 2i5, 2e10.3)
         endif
  210 continue
 
      mhold = m
      nhold = n
 
      irow(row+1) = look
      irow(row+2) = look + 1
 
      mnow = m
 
      do 230 j = 1, m
         si = s(j)
         if (si .ne. 1.0 .and. si .ne. 2.0 .and. si .ne. 0.0) then
            write (dbug,220) j, s(j)
  220       format ('0 ILLEGAL EQUALITY-INEQUALITY CODE FOR ROW ', i3,
     1         ' (', f3.0,').  ',
     2       'LEGITIMATE VALUES ARE "0" (=), "1" (<=), OR "2" 1(=>).')
         endif
         if (si .eq. 2.0) si = -1.0
         s(j) = si
  230 continue
 
      isdone = 0
      inrev = 0
      ir = 0
      itr = 0
      neginv = 0
      negrow = 0
      newx = 0
      newy = 0
      r = 0.0
      size = 0
      isbig = 1
      yaminc = 0.0
C                                           Execute LP
      istate = 0
      call lp
 
      z = obj
      status = istate
      if (istate .ne. 1) then
         kerrsw = 1
         go to 260
      else
         error = 0.0
 
         do 250 i = 1, nlp
 
            kta = ntotzt + i - 1
            ntx = ikk(5,kta)
            ltc = txtie(7,ntx)
            kt = kta - ntota
 
            col = 2 * i - 1
            pkm = x(col) - x(col+1)
            e(kt) = pkm
 
            col = 2 * nlp + 2 * i - 1
            delang = x(col) - x(col+1) - txtie(4,ntx)
C
C           The following phase shift angle is the ultimate value
C           desired from this LP solution. The value obtained assures
C           that Pkm - recalculated in DWNBAK - will be optimal.
C
            anew = x(col) - x(col+1)
C
C           Update admittances to correspond with LTC phase shifter
C           adjustment. Note that subroutine LTCADJ cannot be used since
C           that routine uses the temporarily non-existant admittances
C           stored in the Y-matrix in ECS.
C
            cs = dcos(delang)
            sn = dsin(delang)
            gkm = txtie(5,ntx)
            bkm = txtie(6,ntx)
            gmk = gkm * cs - bkm * sn
            bmk = bkm * cs + gkm * sn
            lt = jckikk(ltc,7)
            if (lt.gt.0) then
               if (tie(1,lt) .eq. txtie(1,ntx) .and.
     1             tie(7,lt) .eq. txtie(2,ntx)) then
                  tie(5,lt) = gmk
                  tie(6,lt) = bmk
               endif
            endif
            txtie(4,ntx) = anew
            txtie(5,ntx) = gmk
            txtie(6,ntx) = bmk
C
C           Update transpose TXTIE branch also.  We have to fish around
C           the index. Alternatively to a direct search in TXTIE, we can
C           exploit the linkage through the LTC which was established in
C           OPSLN1.
C
            lx2 = jckikk(ltc,12)
            gkm = txtie(5,lx2)
            bkm = txtie(6,lx2)
            gmk = gkm * cs + bkm * sn
            bmk = bkm * cs - gkm * sn
            txtie(4,lx2) = -anew
            txtie(5,lx2) = gmk
            txtie(6,lx2) = bmk
            if (lt.gt.0) then
               if (tie(1,lt) .eq. txtie(2,ntx) .and.
     1             tie(7,lt) .eq. txtie(1,ntx)) then
                  tie(5,lt) = gmk
                  tie(6,lt) = bmk
               endif
            endif
 
            if (debug .ne. 0) then
               write (dbug,240) ltc, e(kt), txtie(4,ntx)
  240          format (' Phase shifter ', i3, ' Pkm ',e12.5,
     1            ' phase shift ',e12.5)
            endif
  250    continue
      endif
 
  260 continue     !  *** End of error iteration loop
 
C ***                                                                  *
C *** Print out summary of optimation.                                 *
C ***                                                                  *
 
  270 if (debug .ne. 0) call iexit (istate)
  280 return
      end
