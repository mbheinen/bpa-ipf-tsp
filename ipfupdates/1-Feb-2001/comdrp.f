C    @(#)comdrp.f	20.3 2/13/96
      subroutine comdrp (annote, iter, num1, deldrp, num2, delpku,
     1                   status, getpqv, numitr)

C              This subroutine computes the generation reallocation.
C       
C     Parameters: ANNOTE = Character string identifying calls.  
C                 ITER   = Current solution iteration number.   
C                 NUM1   = Count of busses which dropped generation 
C                          this iteration.  
C        (Input)  DELDRP = MW value of unallocated generation   
C                          which must be dropped.   
C        (Output) DELDRP = MW value of unallocated generation   
C                          remaining to be dropped. 
C                 NUM2   = Count of busses which picked up generation   
C                          this iteration.  
C                 DELPKU = MW value of generation picked up this
C                          iteration.
C                 STATUS = 0/1 = no errors/errors encountered.
C
C                 GETPQV = Subroutine name for computing P, Q, V.
C
C                 NUMITR = Cumulative iteration count.
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/comdrx.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/gendrp.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
c
c***kln Single to double precision.
c
      double precision pgen, pnew, pmax, pmin, pnewmw, dropmw
      double precision totpku, pmaxmw, pminmw, poldmw, totdrp
      double precision delpct, pgenmw, pickup

      integer status
      character btyp * 1, commnt * 37, annote * (*)
      external getpqv
 
      num1 = 0
      num2 = 0
  
      if (idswb .ne. 0) then
 
         call forbtm
         write (outbuf,10) annote, iter
   10    format (t10, a, i3, ' Status of machines with ',
     &           'generation dropped or picked up ')
         call shdlod(1)
 
         write (outbuf,20)
   20    format('0 Generator', t19, 'Type', t25, 'Zone',
     1      t31, '   ----------------- Generation -----------------',
     2      t95,'Comments')
         call shdlod(2)
 
         write (outbuf,30)
   30    format(t34,'Minimum    Maximum    Initial    Final    Change')
         call shdlod(3)
 
         write (outbuf,40)
   40    format(t34,' (MW)       (MW)       (MW)      (MW)       (MW)')
         call shdlod(4)
 
         outbuf = ' '
         call shdlod(5)
         call fortop
         write (outbuf, 50) drptot, 'Initially dropped generation'  
   50    format (t71, f10.1, t95, a)
         call prtout (1)
      endif 
      itrtot = itrtot + 1   
      numitr = itrtot   
      status = 0
C         Compute the total generation to be dropped. The varying nature
C         of system and area slack busses require recomputation of this 
C         quantity. 
C         TOTDRP = total generation dropped in this case.   
C         DELDRP = generation dropped (deficit) this iteration. 
      if (oldrop .eq. -9.0e10) then 
         oldrop = -drptot   
      endif 
      totdrp = oldrop   
      deldrp = 0.0  
      do 110 i = 1, numdrp  
         nb = gndpno(i) 
         kt = inp2opt(nb) 
         pmaxmw = gndpmx(i) 
         pminmw = gndpmn(i) 
         pmin = pminmw / bmva   
         pmax = pmaxmw / bmva   
         if (annote .eq. 'DC Iteration') then 
            if (gndpty(i) .ne. 0 .and. numitr .gt. 1) then  
C                       Update P- and Q-injections for area slack buses.
               call getpqv (kt,pk,dpk,qk,dqk,vk)
               pnetu(kt) = pk  
            endif   
            pgen = pnetu(kt) + ploadu(kt) 
         else   
            if (gndpty(i) .ne. 0) then  
C                        Update P- and Q-injections for area slack buses.   
               call getpqv (kt,pk,dpk,qk,dqk,vk)
               pnetu(kt) = pk  
            endif   
            pgen = pnetu(kt) + ploadu(kt) 
            pnew = pgen - ddim (pgen, pmax) + ddim (pmin, pgen)   
            pnetu(kt) = pnew - ploadu(kt)  
         endif  
         pnew = pgen - ddim (pgen, pmax) + ddim (pmin, pgen)  
         pnetu(kt) = pnew - ploadu(kt) 
         pnewmw = pnew * bmva   
         pgenmw = pgen * bmva   
         dropmw = pnewmw - pgenmw   
         totdrp = totdrp + dropmw   
         deldrp = deldrp + dropmw   
C       
C        Update PGEN quantities on BUSDTA for non-slack busses. 
C       
         if (gndpty(i) .eq. 0) then 
            busdta(8,nb) = busdta(8,nb) + dropmw
         endif  
         if (dropmw .lt. -0.5) then 
            num1 = num1 + 1 
            commnt = 'Generation dropped'   
         else   
            commnt = ' '
         endif  
         if (idswb .ne. 0) then 
            call typno (btyp, ntypu(kt)) 
            write (outbuf, 100) bus(nb), base(nb), btyp, zone(nb),  
     1         pminmw, pmaxmw, pgenmw, pnewmw, dropmw, commnt   
  100       format (t3, a8, f6.1, t21, a1, t27, a2, t31, 5f10.1,
     1         t95, a)  
            if (gndpty(i) .ne. 0) outbuf(122:) = '(Slack bus)'  
            call prtout (1) 
         endif  
        
  110 continue  
        
      if (itrtot .eq. 1) then   
         target = totdrp
      else  
         target = deldrp
      endif 
      delpku = 0.0  
C       
C     "TARGET" is the additional generation pickup desired for this 
C     subroutine call.  
C       
      do 170 itx = 1, 10
        
C       
C        First pass: determine the number and amount of eligible
C        generators for pickup (NUM2, TOTPMX).  
C       
         num2 = 0   
         spinpu = 0.0   
         totpmx = 0.0   
C       
C        NUM2   = total number of eligible pickup generators
C                 (P_i < P_i_max)   
C       
C        SPINPU = total spinning reserve (p.u.).
C        TOTPMX = total P_max on eligible pickup generators.
C       
         do 130 i = 1, numgen   
            nb = gennum(i)  
            kt = inp2opt(nb)  
            pmax = busdta(7,nb) / bmva  
C                     Update P- and Q-injections for area slack buses.  
               if (gentyp(i) .ne. 0) then   
C                     Update P- and Q-injections for area slack buses.  
                  call getpqv (kt,pk,dpk,qk,dqk,vk) 
                  pnetu(kt) = pk   
               endif
               pgen = pnetu(kt) + ploadu(kt)
            if (itx .eq. 1) then
               pold(i) = pgen   
C                    Compensate DELPKU for slack bus excursions since   
C                    previous iteration.
               if (gentyp(i) .ne. 0) then   
                  j = gentyp(i) 
                  if (itrtot .eq. 1) slkgen(j) = genpol(i) / bmva   
                  delpku = delpku + (pgen - slkgen(j)) * bmva   
                  slkgen(j) = pgen  
               endif
            endif   
            if (pgen + 0.005 .lt. pmax .and.
     1          pmax .gt. 0.0 .and. 
     2          gentyp(i) .eq. 0) then  
               spinpu = spinpu + ddim (pmax, pgen)   
               totpmx = totpmx + pmax   
               num2 = num2 + 1  
            endif   
  130    continue   
C       
C        Compute percentage pickup PCTPKU to balance DELDRP with
C        TARGET. Note: TARGET <= 0 means pickup is => 0.
C       
         if (totpmx .eq. 0) then
            delpct = 0.0
            go to 172   
         else   
            delpct = (-delpku - target) / bmva / totpmx 
         endif  
         pctpku = pctpku + delpct   
C       
C        Now apply this percentage pickup PCTPKU until TARGET is
C        reached.   
C       
         totpku = 0.0   
        
         do 160 i = 1, numgen   
            nb = gennum(i)  
            kt = inp2opt(nb)  
            pmax = busdta(7,nb) / bmva  
            pgen = pnetu(kt) + ploadu(kt) 
            pnew = dmin1 (genpol(i)/bmva + pctpku * pmax, pmax) 
C                   Update PGEN quantities on BUSDTA for non-slack busses.  
            if (gentyp(i) .eq. 0) then  
               busdta(8,nb) = busdta(8,nb) + (pnew - pgen) * bmva   
                  pnetu(kt) = pnew - ploadu(kt)
               pnetu(kt) = pnew - ploadu(kt)   
               totpku = totpku + pnew * bmva - genpol(i)
               delpku = delpku + (pnew - pgen) * bmva   
            endif   
        
  160    continue   
        
         if (gensum_flag .eq. 1) then
           if (itx .eq. 1) call space (1) 
           write (outbuf, 162) itx, num1, target, num2, delpku,   
     1        totpmx * bmva, pctpku * 100.0   
  162      format ('  Iteration ',i3,  ' Total dropped ', i3, ' (',   
     1        f10.1, ') Total pickup ', i3, ' (', f10.1, ') P_max (', 
     2        f10.1, ') Pickup (', f8.2, ') % ')  
           call prtout (1)
         endif
C       
C        Allocate PCTPKU until DELDRP < DRPTOL  
C       
         if (abs(delpku + target) .le. drptol) go to 176
        
  170 continue  
        
  172 write (errbuf(1), 174 ) 10, spinpu * bmva, target, delpku 
  174 format('Insufficent spinning reserve after ',i2,
     1       ' iterations (',f10.1,'), amount dropped (', f10.1,
     2       ') amount pickup (',f10.1,')') 
      call prterx ('W', 1)  
      status = 1
        
  176 continue  
C       
C     Summarize new allocated generation.   
C       
      totpku = 0.0  
      delpku = 0.0  
      num2 = 0  
        
      do 230 i  = 1, numgen 
         nb = gennum(i) 
         kt = inp2opt(nb) 
         pmaxmw = busdta(7,nb)  
         pminmw = 0.0   
            pgen = pnetu(kt) + ploadu(kt) 
        
         poldmw = genpol(i) 
         pgenmw = pgen * bmva   
         pickup = pgenmw - poldmw   
         totpku = totpku + pickup   
         j = gentyp(i)  
         if (j .gt. 0) then 
            delpku = delpku + pgenmw - slkgen(j) * bmva 
         else   
            delpku = delpku + pgenmw - pold(i) * bmva   
         endif  
         if (pgenmw - 0.5 .gt. poldmw) then 
            num2 = num2 + 1 
         endif  
        
         if (idswb .ne. 0) then 
            call typno (btyp, ntypu(kt)) 
            if (pmaxmw .gt. 0.0) then   
               pct = 100.0 * pickup / pmaxmw
            else
               pct = 0.0
            endif   
            write (outbuf, 220) bus(nb), base(nb), btyp, zone(nb),  
     1         pminmw, pmaxmw, poldmw, pgenmw, pickup, pct  
  220       format (t3, a8, f6.1, t21, a1, t27, a2, t31, 5f10.1,
     1         t95, 'Generation Pickup ',f6.2, ' %')
            if (gentyp(i) .ne. 0) outbuf(122:) = '(Slack bus)'  
            call prtout (1) 
         endif  
  230 continue  
        
      oldrop = totdrp + totpku  
        
      if (idswb .ne. 0) then
         write (outbuf, 240) totdrp 
  240    format ('0 Total dropped', t71, f10.1) 
         call prtout (1)
         write (outbuf, 242) totpku 
  242    format ('  Total pickup', t71, f10.1)  
         call prtout (1)
      endif 
        
      call space (1)
        
      do 260 i = 1, 5   
         outbuf = ' '   
         call shdlod(i) 
  260 continue  
        
      return
      end   
