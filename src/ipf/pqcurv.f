C    @(#)pqcurv.f	20.5 1/4/99
c****************************************************************
c
c   File: pqcurv.for
c   Purpose:
c            To store the proper qmax and qmin values for
c            a bus with an active p-q curve. The following buses are
c            candidates:
c            1. Area and system slack buses.
c            2. Generator-dropped candidates.
c            3. Buses with Pgen changes.
c            However, this feature has not been implemented yet.
c
c            If the bus pgen is within the P/Q curve data, do a table
c            look-up for the QMAX and QMIN values that go with
c            the scheduled PGEN
c
c   Author: Merilyn George - WSCC   Date: 3 Oct 1992
c
c   Modified: Albert Schmidt- BPA    Date: 15 Oct 1992
c             Merilyn George - WSCC  Date: 13 Apr 1994
c                      Remove "Fatal" errors, add PU processing
c
c   Called by: btable.for
c
c****************************************************************
c
      subroutine pqcurv (ieq, jtbx, kerr)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/prt.inc'
 
      integer ptr, bustype
      logical found_qmax, found_qmin, pass_2, finished, compute_qlim
      real pqmaxvalues(16,2), pqminvalues(16,2)
      double precision qadj

c     Process MW/MVar data

      kerr = 0

      nb = pqbusptr(ieq)
      kt = inp2opt(nb)

      pgen = (pnetu(kt) - ploadu(kt)) * bmva

      compute_qlim = (pqactive(ieq) .and. 
     &                abs (pg_sched(ieq) - pgen) .le. 1.0e-3)
      if ( compute_qlim ) then

        units = max0 (1, pqnumunit(ieq))
        pgenpu = pgen / units
        found_qmax = .false.
        found_qmin = .false.
        qmax = 0.0
        qmin = 0.0
        pass_2 = .true.
 
        if (pgenpu .gt. pqpgen(0,ieq) .and. pqpgen(0,ieq) .gt. 0.0) then
          write (errbuf(1), 10000) bus(nb), base(nb), pgen,
     &      pqpgen(0,ieq)*units
10000	  format (' Bus ', a8, f6.1, 
     &        ' has Pgen (', f8.1, ') > Pmax (', f8.1, 
     &        ' on QP record')
          call prterx('W', 1)
        endif

c       First pass - check constant MVA model regime

        if (pqpgen(-1,ieq) .gt. 0.0) then
          if (pgenpu .gt. pqpgen(-1,ieq)) then
            write (errbuf(1), 10001) bus(nb), base(nb), pgen,
     &        pqpgen(-1,ieq)*units
10001	    format (' Bus ', a8, f6.1, 
     &        ' has Pgen (', f8.1, ') > MMVA (', f8.1, 
     &        ' on QP record')
            call prterx('W', 1)
            kerr = 1
            pass_2 = .false.
          else
            pgenpmax = pqpgen(-1,ieq) * pqqmax(-1,ieq)
            if (pgenpu .ge. pgenpmax) then
              angle = acos (pgenpu / pqpgen(-1,ieq))
              qmax = pqqmax(1,ieq) * sin(angle)
              found_qmax = .true.
            endif
            pgenpmin = pqpgen(-1,ieq) * pqqmin(-1,ieq)
            if (pgenpu .ge. pgenpmin) then
              angle = acos (pgenpu / pqpgen(-1,ieq))
              qmin = -pqpgen(-1,ieq) * sin(angle)
              found_qmin = .true.
            endif
            pass_2 = .not. (found_qmax .and. found_qmin) 
          endif

        endif

        if (pass_2) then
 
c         Second pass - check piece-wise line model regime

          if (.not. found_qmax) then
            maxi = 0
            i = 1
            finished = (pqqmax(i, ieq) .eq. 0.0)
            do while (.not. finished)
              maxi = i
              pqmaxvalues(maxi,1) = pqpgen(i, ieq)
              pqmaxvalues(maxi,2) = pqqmax(i, ieq)
              if (i .lt. 15 .and. pqqmax(i+1,ieq) .gt. 0.0) then
                i = i + 1
              else
                finished = .true.
              endif
            enddo
c
c           Append the starting value of the constant MVA model to 
c           the piecewise linear points
c
            if (pqpgen(-1,ieq) .gt. 0.0) then
              pgenpmax = pqpgen(-1,ieq) * pqqmax(-1,ieq)
              i = 1
              finished = (pqmaxvalues(i,1) .eq. pgenpmax)
              do while (.not. finished)
                if (i .lt. maxi .and. pqmaxvalues(i,1) .lt. pgenpmax)
     &            then
                  i = i + 1
                else
                  maxi = i
                  finished = .true.
                endif
              enddo
              maxi = i + 1
              pqmaxvalues(maxi,1) = pgenpmax
              angle = acos (pgenpmax / pqpgen(-1,ieq))
              pqmaxvalues(maxi,2) = pqpgen(-1,ieq) * sin(angle)
            endif
            i = 1
            finished = (pqmaxvalues(i,1) .gt. pgenpu)
            do while (.not. finished)
              if (i .lt. maxi .and. pqmaxvalues(i+1,1) .le. pgenpu) then
                i = i + 1
              else
                finished = .true.
              endif
            enddo
            if (i .eq. maxi) then
              write (errbuf(1), 10010) bus(nb), base(nb), pgen
10010	      format (' Bus ', a8, f6.1, 
     &          ' could not interpolate Qmax for Pgen (', f8.1, ')')
              write (errbuf(1), 10011) pqmaxvalues(i,1), 
     &          pqmaxvalues(i,2), 9999.9, 9999.9
10011	      format (' between points (', f8.1, ',', f8.1, ') and (', 
     &          f8.1, ',', f8.1, ')')
              call prterx('W', 2)
              kerr = 1
            else if (pgenpu .ge. pqmaxvalues(i,1) .and. 
     &               pgenpu .le. pqmaxvalues(i+1,1)) then
              qmax = pqmaxvalues(i,2) 
     &             + (pgenpu - pqmaxvalues(i,1))
     &             * (pqmaxvalues(i+1,2) - pqmaxvalues(i,2))
     &             / (pqmaxvalues(i+1,1) - pqmaxvalues(i,1))
            else
              write (errbuf(1), 10010) bus(nb), base(nb), pgen
              write (errbuf(1), 10011) pqmaxvalues(i,1), 
     &          pqmaxvalues(i,2), pqmaxvalues(i+1,1), pqmaxvalues(i+1,2)
              call prterx('W', 2)
              kerr = 1
            endif
          endif

          if (.not. found_qmin) then
            mini = 0
            i = 1
            finished = (pqqmax(i, ieq) .eq. 0.0)
            do while (.not. finished)
              mini = i
              pqminvalues(mini,1) = pqpgen(i, ieq)
              pqminvalues(mini,2) = pqqmin(i, ieq)
              if (i .lt. 15 .and. pqqmin(i+1,ieq) .lt. 0.0) then
                i = i + 1
              else
                finished = .true.
              endif
            enddo
c
c           Append the starting value of the constant MVA model to 
c           the piecewise linear points
c
            if (pqpgen(-1,ieq) .gt. 0.0) then
              pgenpmin = pqpgen(-1,ieq) * pqqmin(-1,ieq)
              i = 1
              finished = (pqminvalues(i,1) .ge. pgenpmin)
              do while (.not. finished)
                if (i .lt. mini .and. pqminvalues(i,1) .lt. pgenpmin)
     &            then
                  i = i + 1
                else
                  mini = i
                  finished = .true.
                endif
              enddo
              mini = i + 1
              pqminvalues(mini,1) = pgenpmin
              angle = acos (pgenpmin / pqpgen(-1,ieq))
              pqminvalues(mini,2) = -pqpgen(-1,ieq) * sin(angle)
            endif
            i = 1
            finished = (pqminvalues(i,1) .gt. pgenpu)
            do while (.not. finished)
              if (i .lt. mini .and. pqminvalues(i+1,1) .le. pgenpu) then
                i = i + 1
              else
                finished = .true.
              endif
            enddo
            if (i .eq. mini) then
              write (errbuf(1), 10020) bus(nb), base(nb), pgen
10020	      format (' Bus ', a8, f6.1, 
     &          ' could not interpolate Qmin for Pgen (', f8.1, ')')
              write (errbuf(1), 10021) pqminvalues(i-1,1), 
     &          pqminvalues(i-1,2), 9999.9, -9999.9
10021	      format (' between points (', f8.1, ',', f8.1, ') and (', 
     &          f8.1, ',', f8.1, ')')
              call prterx('W', 2)
              kerr = 1
            else if (pgenpu .ge. pqminvalues(i,1) .and. 
     &          pgenpu .le. pqminvalues(i+1,1)) then
              qmin = pqminvalues(i,2) 
     &             + (pgenpu - pqminvalues(i,1))
     &             * (pqminvalues(i+1,2) - pqminvalues(i,2))
     &             / (pqminvalues(i+1,1) - pqminvalues(i,1))
            else
              write (errbuf(1), 10020) bus(nb), base(nb), pgen
              write (errbuf(1), 10021) pqminvalues(i,1), 
     &          pqminvalues(i,2), pqminvalues(i+1,1), pqminvalues(i+1,2)
              call prterx('W', 2)
              kerr = 1
            endif
          endif
        endif

        qmax = qmax * units
        qmin = qmin * units

        qmax_old = busdta(9,nb)
        qmin_old = busdta(10,nb)
        ptr = kbsdta(15,nb)
        do while (ptr .gt. 0)
          qmax_old = qmax_old + bctbl(11,ptr)
          qmin_old = qmin_old + bctbl(12,ptr)
          ptr = bctbl_nxt(ptr)
        enddo
        busdta(9,nb) = qmax - qmax_old
	busdta(10,nb) = qmin - qmin_old
        if (jtbx .gt. 0) then
          bustype = kbsdta(1,nb) 
          if (bustype .eq. 7) then
            qadj = tbx(6,jtbx) * vstart(nb)**2     
            tbx(3,jtbx) = qmax/bmva - qloadu(kt) - dmin1(0.0d0, qadj)    
            tbx(4,jtbx) = qmin/bmva - qloadu(kt) - dmax1(0.0d0, qadj)  
          else if (bustype .eq. 8) then
            tbx(3,jtbx) = qmax/bmva - qloadu(kt)
            tbx(4,jtbx) = qmin/bmva - qloadu(kt)
          else
          endif
        endif
        pg_sched(ieq) = pgen

      endif     ! End of active curve processing
      return
      end
