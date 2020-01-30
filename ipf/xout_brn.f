C    @(#)xout_brn.f	20.3 2/13/96
C****************************************************************
C
C      File: xout_brn.f
C      Purpose: Routine to obtain BCD image of INPUT/OUTPUT/SYSTEM
C               data
C
C               Note: This is limited edition of the original out_brn
c               module. The limitation is that the alternate data set 
c               excludes some data accessing routines.
C
C      Author: Walt Powell  Date: 20 February 1992
C                           Modified: 20 February 1992
C      Called by:
C
C****************************************************************
C
        subroutine xout_brn (ptr, id, sect, datarec)
        integer ptr

        character datarec * (*), id * 1
        integer sect
c
c       This subroutine returns WSCC-formated output data records.
c       Output parameter:
c
c       datarec - a character string for storing data
c
        include 'ipfinc/parametr.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/alt_case.inc'
        include 'ipfinc/com007.inc'
  
 	character idx * 1, ratec * 10, ratecx * 10, ovldtag * 1,
     &            code * 7, loading_c * 7, crit_ln_tag * 1, 
     &            crit_tx_tag * 1
        integer p, pold, qptr, whichend, whichend1, whichend2, 
     &          isection(4,10), count_line, count_tx, crit_ln_end,
     &          crit_tx_end,  crit_ln_ptr, crit_tx_ptr     
        real section(3,10)

        k1 = okx(ptr)
        k2 = oky(ptr)

        pintot = 0.0
        qintot = 0.0
        pouttot = 0.0
        qouttot = 0.0
        plosstot = 0.0
        qlosstot = 0.0
        ratingtot = 0.0
        tap1 = 0.0
        tap2 = 0.0
        numckts = 0
        pctcomp = 0.0
        num_par_rec = 0
        crit_ln_tag = ' '
        crit_tx_tag = ' '
        crit_ln_amps = 0.0
        crit_ln_rate = 0.0
        crit_ln_load = 0.0
        crit_tx_mva = 0.0
        crit_ln_end = 0
        crit_tx_end = 0
        crit_tx_rate = 0
        crit_tx_load = 0
        tot_ln_amps = 0.0
        tot_ln_rate = 0.0
        tot_tx_mva = 0.0
        tot_tx_rate = 0.0
        crit_ln_ptr = 0
        crit_tx_ptr = 0
 
        p = ptr
        do while (p .gt. 0 .and. (k2 .eq. oky(p)))
           ltyp = obrtype(p)
           idx = obrid(p)
           if (ltyp .eq. 1) then
              num_par_rec = num_par_rec + 1
              qptr = obrnch_ptr(p)
              nbr = iabs (qptr)
              if (obrnch(16,nbr) .eq. 0.0) then
                 numckts = numckts + 1
              else
                 numckts = numckts + obrnch(16,nbr)
              endif
              rating = 0.0
              call xgtlfq (p, pin, qin, ploss, qloss, ovld, ratec,
     &                 actual_amps, whichend1, actual_mva, whichend2)
              numsec = 1
              isection(1,numsec) = 1
              isection(2,numsec) = 0
              isection(3,numsec) = whichend1
              isection(4,numsec) = whichend2
              section(1,numsec) = actual_amps
              section(2,numsec) = actual_mva
              section(3,numsec) = 0.0
              if (id .eq. '*' .or.
     &           (id .eq. idx .and. sect .eq. 0)) then
                 pintot = pintot + pin
                 qintot = qintot + qin
                 pouttot = pouttot + pin - ploss
                 qouttot = qouttot + qin - qloss
                 plosstot = plosstot + ploss
                 qlosstot = qlosstot + qloss
              endif
              pold = p
              j = obrnch_nxt(p)
              xtot = 0.0
              ctot = 0.0
              count_line = 0
              count_tx = 0
              crit_ln_ptr = 0
              crit_tx_ptr = 0
              do while (j .gt. 0 .and.
     &                 (oky(j) .eq. k2 .and. obrid(j) .eq. idx))
                 call xgtlfq (j, pinx, qinx, plossx, qlossx, ovldx, 
     &                  ratecx, actual_amps, whichend1, actual_mva, 
     &                  whichend2)
                 numsec = numsec + 1
                 isection(1,numsec) = obrtype(j)
                 isection(2,numsec) = obrsect(j)
                 isection(3,numsec) = whichend1
                 isection(4,numsec) = whichend2
                 section(1,numsec) = actual_amps
                 section(2,numsec) = actual_mva
                 read (ratecx, 109) section(3,numsec)
  109            format (2x, f4.0)
                 if (id .eq. '*' .or.
     &              (id .eq. idx .and. sect .eq. obrsect(j))) then
c
c                   Find critical loadings
c
                    if (obrtype(j) .eq. 3 .or. obrtype(j) .eq. 8) then
                       if (crit_ln_amps .eq. 0.0 .or. 
     &                    (crit_ln_amps .ne. 0.0 .and. 
     &                     crit_ln_amps .lt. actual_amps)) then
                          crit_ln_amps = actual_amps
                          crit_ln_load = ovldx
                          read (ratecx, 109) crit_ln_rate
                          crit_ln_tag = ratecx(1:1)
                          crit_ln_end = whichend1
                          crit_ln_ptr = j        
                       endif
                    else if (obrtype(j) .eq. 5 .or. obrtype(j) .eq. 6) 
     &                 then
                       if (crit_tx_mva .eq. 0.0 .or. 
     &                    (crit_tx_mva .ne. 0.0 .and. 
     &                     crit_tx_mva .lt. actual_mva)) then
                          crit_tx_mva = actual_mva
                          crit_tx_load = ovldx
                          read (ratecx, 109) crit_tx_rate
                          crit_tx_tag = ratecx(1:1)
                          crit_tx_end = whichend2
                          crit_tx_ptr = j           
                       endif
                    endif
                 endif
c
c                Add section flows only if specific to request
c
                 if (id .eq. idx .and. sect .eq. obrsect(j)) then
                    pintot = pintot + pinx
                    qintot = qintot + qinx
                    pouttot = pouttot + pinx - plossx
                    qouttot = qouttot + qinx - qlossx
                    plosstot = plosstot + plossx
                    qlosstot = qlosstot + qlossx
                    if (obrtype(j) .eq. 5 .or. obrtype(j) .eq. 6) then
                       tot_tx_mva = tot_tx_mva + actual_mva
                       read (ratecx, 109) rate
                       tot_tx_load = tot_tx_load + rate
                    else
                       tot_ln_amps = tot_ln_amps + actual_amps
                       read (ratecx, 109) rate
                       tot_ln_load = tot_ln_load + rate
                    endif
                 endif
                 if (obrtype(j) .eq. 5) then
                    count_tx = count_tx + 1
                    qptr = obrnch_ptr(j)
                    nbr = iabs (qptr)
                    if (qptr .gt. 0) then
                       tap1 = obrnch(9,nbr)
                       tap2 = obrnch(10,nbr)
                    else
                       tap1 = obrnch(10,nbr)
                       tap2 = obrnch(9,nbr)
                    endif
                 else if (obrtype(j) .eq. 6) then
                    count_tx = count_tx + 1
                    qptr = obrnch_ptr(j)
                    nbr = iabs (qptr)
                    if (qptr .gt. 0) then
                       tap1 = obrnch(9,nbr)
                       tap2 = obrnch(10,nbr)
                    else
                       tap1 = -obrnch(9,nbr)
                       tap2 = obrnch(10,nbr)
                    endif
                 else
                    count_line = count_line + 1
                    qptr = obrnch_ptr(j)
                    nbr = iabs (qptr)
                    if (obrnch(6,nbr) .gt. 0.0) then
                       xtot = xtot + obrnch(6,nbr)
                    else
                       ctot = ctot + obrnch(6,nbr)
                    endif
                 endif
                 pold = j
                 j = obrnch_nxt(j)
              enddo
c
c             Determine tot_ln_amps, tot_tx_mva from most loaded
c             section
c
              if (id .eq. idx .and. sect .eq. obrsect(j)) then
              else
                 xmax = -1.0e+10
                 imax = 1
                 do i = 2, numsec
                    if (isection(1,i) .ne. 5 .and. 
     &                  isection(1,i) .ne. 6) then
                       if (section(1,i) .gt. xmax) then
                          imax = i
                          xmax = section(1,i)
                       endif
                    endif
                 enddo

                 xmax = -1.0e+10
                 imax = 1
                 do i = 2, numsec
                    if (isection(1,i) .eq. 5 .or. 
     &                  isection(1,i) .eq. 6) then
                       if (section(2,i) .gt. xmax) then
                          imax = i
                          xmax = section(2,i)
                       endif
                    endif
                 enddo
                 if (id .eq. '*' .or.                            
     &              (id .eq. idx .and. sect .eq. 0)) then        
                    if (crit_tx_ptr .gt. 0) then                 
                       tot_tx_mva = tot_tx_mva + crit_tx_load    
                       tot_tx_rate = tot_tx_rate + crit_tx_rate  
                    endif                                        
                    if (crit_ln_ptr .gt. 0) then                 
                       tot_ln_amps = tot_ln_amps + crit_ln_amps  
                       tot_ln_rate = tot_ln_rate + crit_ln_rate  
                    endif                                        
                 endif
              endif

              if (xtot .gt. 0.0 .and. ctot. lt. 0.0) then
                 pctcomp = -100.0 * ctot / xtot
              else
                 pctcomp = 0.0
              endif
              p = pold
           else if (ltyp .eq. 4) then
           else
              num_par_rec = num_par_rec + 1
              call xgtlfq (p, pin, qin, ploss, qloss, ovld, ratec,
     &                 actual_amps, whichend1, actual_mva, whichend2)
              qptr = obrnch_ptr(p)
              nbr = iabs (qptr)
              if (id .eq. '*' .or.
     &           (id .eq. idx .and. sect .eq. 0)) then
                 pintot = pintot + pin
                 qintot = qintot + qin
                 pouttot = pouttot + pin - ploss
                 qouttot = qouttot + qin - qloss
                 plosstot = plosstot + ploss
                 qlosstot = qlosstot + qloss
                 if (obrtype(p) .eq. 5 .or. obrtype(p) .eq. 6) then
                    tot_tx_mva = tot_tx_mva + actual_mva
                    read (ratec, 109) rate
                    tot_tx_load = tot_tx_load + rate
                 else
                    tot_ln_amps = tot_ln_amps + actual_amps
                    read (ratec, 109) rate
                    tot_ln_load = tot_ln_load + rate
                 endif
c
c                Find critical loadings
c
                 if (obrtype(p) .eq. 2 .or. obrtype(p) .eq. 3 .or.
     &               obrtype(p) .eq. 7 .or. obrtype(p) .eq. 8) then
                    if (crit_ln_amps .eq. 0.0 .or. 
     &                 (crit_ln_amps .ne. 0.0 .and. 
     &                  crit_ln_amps .lt. actual_amps)) then
                       crit_ln_amps = actual_amps
                       crit_ln_load = ovld
                       read (ratec, 109) crit_ln_rate
                       crit_ln_tag = ratec(1:1)
                       crit_ln_end = whichend1
                    endif
                 else if (obrtype(p) .eq. 5 .or. obrtype(p) .eq. 6) 
     &              then
                    if (crit_tx_mva .eq. 0.0 .or. 
     &                 (crit_tx_mva .ne. 0.0 .and. 
     &                  crit_tx_mva .lt. actual_mva)) then
                       crit_tx_mva = actual_mva
                       crit_tx_load = ovld
                       read (ratec, 109) crit_tx_rate
                       crit_tx_tag = ratec(1:1)
                       crit_tx_end = whichend2
                    endif
                 endif
                 if (ltyp .eq. 2 .or. ltyp .eq. 7 .or.
     &               obrnch(16,nbr) .eq. 0.0) then
                    numckts = numckts + 1
                 else
                    numckts = numckts + obrnch(16,nbr)
                 endif
              endif
              if (obrtype(p) .eq. 5) then
                 if (qptr .gt. 0) then
                    tap1 = obrnch(9,nbr)
                    tap2 = obrnch(10,nbr)
                 else
                    tap1 = obrnch(10,nbr)
                    tap2 = obrnch(9,nbr)
                 endif
              else if (obrtype(p) .eq. 6) then
                 if (qptr .gt. 0) then
                    tap1 = obrnch(9,nbr)
                    tap2 = obrnch(10,nbr)
                 else
                    tap1 = -obrnch(9,nbr)
                    tap2 = obrnch(10,nbr)
                 endif
              endif
           endif
           p = obrnch_nxt(p)
        enddo
        call obcdbrn (ptr, datarec)
        if (tap1 .eq. 0) then
           tap1 = pctcomp
        else
           datarec(1:1) = 'T' 
        endif
        if (num_par_rec .gt. 1) then
           ovldtag = '*'
        endif
        numckts = min (numckts, 9)
        if (tot_ln_rate .ne. 0.0) then
           tot_ln_load = 100.0 * abs (tot_ln_amps / tot_ln_rate)
        else
           tot_ln_load = 0.0
        endif
        if (tot_tx_rate .ne. 0.0) then
           tot_tx_load = 100.0 * abs (tot_tx_mva / tot_tx_rate)
        else
           tot_tx_load = 0.0
        endif
        write (datarec(33:), 100) numckts, pintot, qintot, pouttot, 
     &     qouttot, plosstot, qlosstot, crit_ln_amps, crit_ln_rate, 
     &     crit_ln_tag, crit_ln_end, crit_tx_mva, crit_tx_rate, 
     &     crit_tx_tag, crit_tx_end, tot_ln_load, tot_ln_amps, 
     &     tot_tx_load, tot_tx_mva, tap1, tap2
  100   format (i1, 7e15.7, f8.1, a1, i1, e15.7, f8.1, a1, i1, 4e15.7,
     &     2f8.2)
        return
        end

