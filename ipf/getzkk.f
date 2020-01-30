C    @(#)getzkk.f	20.5 5/27/98
        subroutine getzkk (nodek, nodem, zkk, error)

C       Input parameters:
c
c         nodek - the driving point bus if nodem == 0;
c                 the from bus if nodem <> 0
c         nomem = 0 if the driving point impedance is sought
C               <> 0 if the transfer impedance between nodek and nodem
c                    is sought.
C
C       Returned values:
c          ZKK - the driving point/transfer impedance
c          ERROR - the return status: 0 - normal, 1 - error.

        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/alpha.inc'
        include 'ipfinc/alpha2.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/ikk.inc'
        include 'ipfinc/prt.inc'

        parameter   ( MAXMTX = 8.0 * MAXYE )   ! factoried jacobian e
        common /amtrx/ row(0:500), next(500), yrow(500), iyrow(MAXBUS),
     1                 v(MAXBUS),iy(MAXMTX/4), y(MAXMTX/4)
        integer error, row, next, iyrow, iy
        complex yrow, y, v, zkk, diag, z(2,2)
        logical finished
C
C       Begin factorization
C
        error = 0
        pass = 1
        node = nodek
        if (nodem .eq. 0) then
          min_node = nodek
        else
          min_node = min0 (nodek, nodem)
        endif
        zkk = cmplx (0.0, 0.0)
        finished = .false.

        ikec = 1
        do kt = 1, ntot
          iyrow(kt) = ikec
          if (kt .eq. nodek) then
            v(kt) = cmplx (1.0, 0.0)
          else if (kt .eq. nodem) then
            v(kt) = cmplx (-1.0, 0.0)
          else
            v(kt) = 0.0
          endif
c
c         Get row from Y-matrix
c
          lp = 0
          lsw = 0
          do l = km(kt), km(kt) - 1 + kmlen(kt)
            lp = lp + 1
            mt = ikmu(l)                         !uur
            if (lsw .eq. 0 .and. kt .lt. mt) then
              row(lp) = kt
              next(lp) = lp + 1
              yrow(lp) = cmplx(gkku(kt), bkku(kt))
              lsw = 1
              lp = lp + 1
            endif
            row(lp) = mt
            next(lp) = lp + 1
            yrow(lp) = cmplx (gkmu(l), bkmu(l)) !uur
          enddo
          if (lsw .eq. 0) then
            lp = lp + 1
            row(lp) = kt
            next(lp) = lp + 1
            yrow(lp) = cmplx(gkku(kt), bkku(kt))
            if (yrow(lp) .eq. cmplx (0.0, 0.0)) then
              yrow(lp) = cmplx (1.0e-6, 0.0)
            endif
          endif
          next(lp) = 0

C         Eliminate lower-diagonal elements.

          now = 1
          do while (now .gt. 0 .and. (row(now) .lt. kt))
            mt = row(now)
            v(kt) = v(kt) - yrow(now) * v(mt)
            kfirst = iyrow(mt)
            klast = iyrow(mt+1) - 1
            ilast = now
            i = next(now)
            do j = kfirst,klast
              icol = iy(j)
              do while (i .gt. 0 .and. (row(i) .lt. icol))
                ilast = i
                i = next(i)
              enddo
              if (i .eq. 0 .or. (row(i) .gt. icol)) then
                lp = lp + 1
                next(ilast) = lp
                next(lp) = i
                row(lp) = icol
                yrow(lp) = cmplx (0.0, 0.0)
                i = lp
              endif
              yrow(i) = yrow(i) - yrow(now) * y(j)
            enddo
            now = next(now)
          enddo

C         Normalize upper-diagonal elements.

          if (now .gt. 0 .and. (row(now) .eq. kt)) then
            diag = 1.0 / yrow(now)
            v(kt) = v(kt) * diag
            now = next(now)
            do while (now .gt. 0)                 !uur
              iy(ikec) = row(now)
              y(ikec) = yrow(now) * diag
              ikec = ikec + 1
              now = next(now)                    !uua
            end do                                !uur
          else
            write (errbuf(1), 160) kt
  160       format (' Ill-conditioned Y-matrix at row ', i4)
            call prterx ('W', 1)
            error = 1
            return
          endif

        enddo
        iyrow(ntot+1) = ikec
C
C       Perform backwards solution
C
        do kt = ntot-1, min_node, -1
          do i = iyrow(kt), iyrow(kt+1)-1
            mt = iy(i)
            v(kt) = v(kt) - y(i)*v(mt)
          enddo
        enddo
        if (nodem .eq. 0) then
          zkk = v(nodek)
        else 
          zkk = v(nodek) - v(nodem)
        endif

        return
        end
