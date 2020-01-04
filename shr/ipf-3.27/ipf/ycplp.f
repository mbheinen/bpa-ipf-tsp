C    @(#)ycplp.f	20.3 2/13/96
      subroutine ycplp (pij,pijadj,imag,vmag,vmin,vmax,dx,db,pijvio,
     1                  iadj,ivio,z,status)
C
C     This routine uses a general purpose LP to estimate the integrated
C     corrections DX(*) which will solve (or optimize) PIJ(*).
C
C     Input:
C       PIJ(*) - The original line flows.
C       IMAG(*) - The original current flows.
C       VMAG(*) - The nodal voltages.
C       VMAX(*) - The nodal voltage maximum limit.
C       VMIN(*) - The nodal voltage minimum limit.
C
C     Output:
C       PIJVIO(*) - The remaining line flow residual error after correct
C                   DX(*), DB(*);
C       PIJADJ(*) - The line flows corrections modified with DX(*);
C       IVIO(*) - The remaining line current residual error after correc
C                 DX(*), DB(*);
C       IADJ(*) - The line current corrections modified with DX(*);
C       DX(*)  -  The line reactive adjustments.
C       DB(*)  -  The line susceptive adjustments.
C       STATUS -  1 : 'Optimum'
C                 2 : 'Infeasible'
C                 3 : 'Unbounded'
C                 4 : 'The maximum size of the inverse has been exceeded
C                 5 : 'The maximum number of iterations has been reached
C                 6 : 'Either the AA vector is full or the number of
C                      constraints is exceed the maximum'
C                 7 : 'Still inaccurate after reinverting'
C       Z - Value of objective function.
C
C     The LP used is copied from the book by A. Land and S. Powell:
C     "Fortran Codes for Mathematical Programming", 1978, John Wiley
C     and Sons, N.Y.
C
 
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/aref.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/smallp.inc'
      include 'ipfinc/ycomp.inc'
      include 'ipfinc/ycpsen.inc'
 
 
      real pij(*),  pijadj(*), dx(*), db(*), pijvio(*), imag(*),
     1     vmag(*), vmin(*),   vmax(*), iadj(*), ivio(*), irate
 
      integer row, col, status, debug
C
C     The following tolerances are defined for double-precision words
C     (64-bits).  See page 186 for 32-bit equivalent tolerances.
C
      big = 1.0e11
      small = 1.0e-9
      tol(1) = 1.0e-6
      tol(2) = 1.0e-5
      tol(3) = 1.0e-6
      tol(4) = 1.0e-5
      tol(5) = 1.0e-5
      tol(6) = 1.0e-5
      tol(7) = 1.0e-5
      tol(8) = tol(5) * 10.0
 
      isdone = 0
      debug = 1
C
C     Define problem size. "M" is initially 9 per device. If no current
C     ratings are present, "M" is reduced to 7.
C
      m = 9 * nycomp
      n = 16 * nycomp
 
      isbnd = 4 * nycomp
 
      morepr = 1
      itrmax = 0
      irmax = 0
C
C     Initialize arrays
C
      bndj = -1.0
      if (isbnd .eq. -1) bndj = 1.0
      do 100 j = 1, n
         c(j) = 0.0
         bound(j) = bndj
  100 continue
      do 110 i = 1, m
      b(i) = big
  110 s(i) = 1.0
C
C     Load C elements: Cp = -0.1, Cv = -1.0, Ci = -10.0. Negative
C     coefficients are used since this is a maximization problem.
C
      do 120 jt = 1,nycomp
      j = 16 * jt - 15
 
      c(j+2) = -0.1
      c(j+3) = -0.1
      c(j+6) = -1.0
      c(j+7) = -1.0
      c(j+10) = -10.0
      c(j+12) = -10.0
C
C     Add bounds of Xij; do not permit adjustment in excess of
C     0.005 p.u. or 2.5 Pij (p.u.) per iteration.
C
      xij = ycomp(32,jt)
      val1 = amax1 (ycomp(35,jt)-xij,0.0)
      if (ycomp(42,jt) .ne. 0.0) then
         val2 = 2.5 / abs(ycomp(42,jt))
      else
         val2 = big
      endif
      bound(j+12) = amin1 (val1,val2,0.005)
      val1 = amax1 (xij-ycomp(34,jt),0.0)
      bound(j+13) = amin1 (val1,val2,0.005)
C
C     Add bounds to Bis; do not permit adjustment in excess of
C     0.010 p.u. per iteration.
C
      bis = ycomp(36,jt)
      val = amax1 (ycomp(39,jt)-bis,0.0)
      bound(j+14) = amin1 (val,0.010)
      val = amax1 (bis-ycomp(38,jt),0.0)
      bound(j+15) = amin1 (val,0.010)
 
  120 continue
 
      look = 1
      row = 0
 
      do 140 jt = 1,nycomp
 
      col = 16 * jt - 15
C
C     Load A and B elements. SI(*) flags equality-inequality signs.
C     Non-zero A elements must be loaded in ascending order:
C     A(1,1),A(1,3),...,A(2,1),...  At least one element in each row
C     must be specified, even if that element is zero!
C
C
C     1 means <=
C     2 means =>
C     0 means =
C
C     P(j) upper limit constraint
C
      row = row + 1
      irow(row) = look
 
      jcol(look) = col
      aa(look) = 1.0
      look = look + 1
 
      jcol(look) = col + 1
      aa(look) = -1.0
      look = look + 1
 
      jcol(look) = col + 2
      aa(look) = -1.0
      look = look + 1
 
      b(row) = ycomp(6,jt) - pij(jt)
      s(row) = 1.0
 
      if (debug .ne. 0) then
         write (dbug,111) row, jt, b(row), s(row)
  111    format (' YCPLP - P_max ROW: JT, B, S ',2i5,2e10.3)
      endif
C
C     P(j) lower limit constraint
C
      row = row + 1
      irow(row) = look
 
      jcol(look) = col
      aa(look) = 1.0
      look = look + 1
 
      jcol(look) = col + 1
      aa(look) = -1.0
      look = look + 1
 
      jcol(look) = col + 3
      aa(look) = 1.0
      look = look + 1
 
      b(row) = ycomp(5,jt) - pij(jt)
      s(row) = 2.0
 
      if (debug .ne. 0) then
         write (dbug,112) row, jt, b(row), s(row)
  112    format (' YCPLP - P_min ROW: JT, B, S ',2i5,2e10.3)
      endif
C
C     dP/dX constraint
C
      row = row + 1
      irow(row) = look
 
      do 130 k = 1,nycomp
      kol = 16 * k - 15
 
      if (k .eq. jt) then
 
         jcol(look) = kol
         aa(look) = -1.0
         look = look + 1
 
         jcol(look) = kol + 1
         aa(look) = 1.0
         look = look + 1
 
      endif
 
      jcol(look) = kol + 12
      aa(look) = ycpsen(jt,k)
      look = look + 1
 
      jcol(look) = kol + 13
      aa(look) = -ycpsen(jt,k)
      look = look + 1
 
  130 continue
 
      b(row) = 0.0
      s(row) = 0.0
 
      if (debug .ne. 0) then
         write (dbug,131) row, jt, b(row), s(row)
  131    format (' YCPLP - dP/dX ROW: JT, B, S ',2i5,2e10.3)
      endif
C
C     dV/dB constraint
C
      if (ycomp(43,jt) .eq. 0.0 .and. ycomp(48,jt) .eq. 0.0) then
 
      else
 
         row = row + 1
         irow(row) = look
 
         jcol(look) = col + 4
         aa(look) = 1.0
         look = look + 1
 
         jcol(look) = col + 5
         aa(look) = -1.0
         look = look + 1
 
         jcol(look) = col + 12
         aa(look) = -ycomp(43,jt)
         look = look + 1
 
         jcol(look) = col + 13
         aa(look) = ycomp(43,jt)
         look = look + 1
 
         jcol(look) = col + 14
         aa(look) = -ycomp(48,jt)
         look = look + 1
 
         jcol(look) = col + 15
         aa(look) = ycomp(48,jt)
         look = look + 1
 
         b(row) = 0.0
         s(row) = 0.0
 
         if (debug .ne. 0) then
            write (dbug,132) row, jt, b(row), s(row)
  132       format (' YCPLP - dV/dB ROW: JT, B, S ',2i5,2e10.3)
         endif
 
      endif
C
C     dP - V * dI - I * dV = 0 constraint
C
      row = row + 1
      irow(row) = look
 
      jcol(look) = col
      aa(look) = 1.0
      look = look + 1
 
      jcol(look) = col + 1
      aa(look) = -1.0
      look = look + 1
 
      jcol(look) = col + 4
      aa(look) = -imag(jt)
      look = look + 1
 
      jcol(look) = col + 5
      aa(look) = imag(jt)
      look = look + 1
 
      jcol(look) = col + 8
      aa(look) = -vmag(jt)
      look = look + 1
 
      jcol(look) = col + 9
      aa(look) = vmag(jt)
      look = look + 1
 
      b(row) = 0.0
      s(row) = 0.0
 
      if (debug .ne. 0) then
         write (dbug,133) row, jt, b(row), s(row)
  133    format (' YCPLP - d(VI) ROW: JT, B, S ',2i5,2e10.3)
      endif
 
      irate = ycomp(40,jt)
      if (irate .gt. 0.0) then
C
C        Imag < Imax (=Irate)
C
         row = row + 1
         irow(row) = look
 
         jcol(look) = col + 8
         aa(look) = 1.0
         look = look + 1
 
         jcol(look) = col + 9
         aa(look) = -1.0
         look = look + 1
 
         jcol(look) = col + 10
         aa(look) = -1.0
         look = look + 1
 
         b(row) = irate - imag(jt)
         s(row) = 1.0
 
         if (debug .ne. 0) then
            write (dbug,134) row, jt, b(row), s(row)
  134       format (' YCPLP - I_max ROW: JT, B, S ',2i5,2e10.3)
         endif
C
C        Imin (=-Irate) < Imag
C
         row = row + 1
         irow(row) = look
 
         jcol(look) = col + 8
         aa(look) = 1.0
         look = look + 1
 
         jcol(look) = col + 9
         aa(look) = -1.0
         look = look + 1
 
         jcol(look) = col + 11
         aa(look) = 1.0
         look = look + 1
 
         b(row) = -irate - imag(jt)
         s(row) = 2.0
 
         if (debug .ne. 0) then
            write (dbug,135) row, jt, b(row), s(row)
  135       format (' YCPLP - I_min ROW: JT, B, S ',2i5,2e10.3)
         endif
      endif
C
C     Overvoltage excursion
C
      row = row + 1
      irow(row) = look
 
      jcol(look) = col + 4
      aa(look) = 1.0
      look = look + 1
 
      jcol(look) = col + 5
      aa(look) = -1.0
      look = look + 1
 
      jcol(look) = col + 6
      aa(look) = -1.0
      look = look + 1
 
      b(row) = vmax(jt) - vmag(jt)
      s(row) = 1.0
 
      if (debug .ne. 0) then
         write (dbug,136) row, jt, b(row), s(row)
  136    format (' YCPLP - V_max ROW: JT, B, S ',2i5,2e10.3)
      endif
C
C     Undervoltage excursion
C
      row = row + 1
      irow(row) = look
 
      jcol(look) = col + 4
      aa(look) = 1.0
      look = look + 1
 
      jcol(look) = col + 5
      aa(look) = -1.0
      look = look + 1
 
      jcol(look) = col + 7
      aa(look) = 1.0
      look = look + 1
 
      b(row) = vmin(jt) - vmag(jt)
      s(row) = 2.0
 
      if (debug .ne. 0) then
         write (dbug,137) row, jt, b(row), s(row)
  137    format (' YCPLP - V_min ROW: JT, B, S ',2i5,2e10.3)
      endif
 
  140 continue
 
      irow(row+1) = look
      irow(row+2) = look + 1
 
      m = row
      mnow = row
 
      if (itrmax .eq. 0) then
         ijk = isbnd
         if (ijk .eq. -1) ijk = n
         itrmax = 3 * (m + n + ijk)
      endif
 
      if (irmax .eq. 0) then
         irmax = itrmax
      endif
 
      write (dbug,150) itrmax, irmax
  150 format('0ITRMAX = ',i5,' IRMAX = ',i5)
 
      write (dbug,160) m, n, isbnd
  160 format('0 M (NO. OF CONSTRAINTS) = ',i5,
     1       ', N (NO. OF VARIBLES--REAL, NOT SLACK) = ',i5,','
     2       /1x,'NUMBER OF UPPER BOUNDED VARIBLES= ',i5,'.')
 
      if (m .gt. MAXM .or. n .gt. MAXN .or. isbnd .gt. MAXN) then
         write (dbug,170) MAXM, MAXN
  170    format('0YOU ARE TRYING TO SOLVE A PROBLEM WHICH IS TOO ',
     1          'BIG FOR THE PROGRAM.'/,' THE LARGEST PROBLEM HAS',
     2          i5,' CONSTRAINTS AND',i5,' VARIABLES.')
         isdone = 1
         go to 900
      endif
 
      if (m .lt. 0 .or. n .lt. 0 .or. isbnd .lt. -1) then
         write (dbug,180)
  180    format ('0 IT IS NOT POSSIBLE TO HAVE A NEGATIVE NUMBER ',
     1           'OF ROWS OR COLUMNS.')
         isdone = 1
         go to 900
      endif
 
      do 200 j = 1, m
      si = s(j)
      if (si .ne. 1.0 .and. si .ne. 2.0 .and. si .ne. 0.0) then
         write (dbug,190) j, s(j)
  190    format ('0 THE EQUALITY-INEQUALITY CODE FOR ROW ',i3,
     1           ' IS INCORRECT (',f3.0,').  IT MUST BE "1" ONLY ',
     2           'BE 1 FOR A LESS-THAN-OR-EQUAL CONSTRAINT, "0" ',/,
     3           ' FOR AN EQUALITY, OR "2" FOR GREATER-THAN-OR EQUAL.')
      endif
      if (si .eq. 2.0) si = -1.0
      s(j) = si
  200 continue
 
      mnow = m
 
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
C
C     Execute LP
C
      istate = 0
      call lp
 
      z = obj
      status = istate
      if (istate .eq. 1) then
         do 210 jt = 1,nycomp
         col = 16 * jt - 15
         pijadj(jt) = x(col) - x(col+1)
         pijvio(jt) = x(col+2) - x(col+3)
         iadj(jt) = x(col+8) - x(col+9)
         ivio(jt) = x(col+10) - x(col+11)
         dx(jt) = x(col+12) - x(col+13)
         db(jt) = x(col+14) - x(col+15)
  210    continue
      else
         do 220 jt = 1,nycomp
         col = 16 * jt - 15
         pijadj(jt) = 0.0
         pijvio(jt) = dim (pij(jt),ycomp(6,jt)) -
     1                dim (ycomp(5,jt),pij(jt))
         pijadj(jt) = 0.0
         iadj(jt) = 0.0
         if (ycomp(40,jt) .eq. 0.0) then
            ivio(jt) = 0.0
         else
            ivio(jt) = dim (imag(jt),ycomp(40,jt)) -
     1                 dim (-ycomp(40,jt),imag(jt))
         endif
         dx(jt) = 0.0
         db(jt) = 0.0
  220    continue
      endif
C
C     Print out summary of optimation.
C
  900 if (debug .ne. 0) call iexit (istate)
 
      end
