C    @(#)prtprm.f	20.4 9/10/96
      subroutine prtprm (xvalue, yvalue, error)
      integer error
C                                                                   
C     postprocess /CHANGE_PARAMETER commands.     
C     If ERROR >< 0, skip present case because of errors (solution  
C     divergence, and read in next record.     
C                                              
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc' 
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/chgprm.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/coment.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/tbx.inc'
 
      character state * 5, ljstfy * 20, tempc1 * 20, tempc2 * 20,
     1          xpoint * 2, ypoint * 2
C
C     If failed solution, results are meaningless because they are
C     from a prior solved solution.
C
      if (error .gt. 0) go to 180
 
      kt = inp2opt(nb_prm)
C               Compute most recent values of PK, QK, etc.  
      call nrpqv (kt,pk,dpk,qk,dqk,vk)  
C                          Get "state"  
      state = ' '   
      if (ntbx_prm .gt. 0) then 
         ltyp = tbx(1,ntbx_prm)
         ityp = tbx(7,ntbx_prm)
         if (ltyp .eq. 1) then  
            if (ityp .eq. 3) then   
               state = 'V_min'  
            else if (ityp .eq. 4) then  
               state = 'V_max'  
            endif   
         else if (ltyp .eq. 2) then 
            if (ityp .eq. 3) then   
               state = 'Q_min'  
            else if (ityp .eq. 4) then  
               state = 'Q_max'  
            endif   
         else if (ltyp .eq. 3) then 
            if (ityp .eq. 2) then   
               state = 'Q_min'  
            else if (ityp .eq. 3) then  
               state = 'Q_max'  
            else if (ityp .eq. 4) then  
               state = 'V_min'  
            else if (ityp .eq. 5) then  
               state = 'V_max'  
            endif   
         else if (ltyp .eq. 5) then 
            if (ityp .eq. 2) then   
               state = 'Q_min'  
            else if (ityp .eq. 3) then  
               state = 'Q_max'  
            else if (ityp .eq. 4) then  
               state = 'Q_dis'  
            endif   
         endif  
      endif 
C                                        Output results 
      call space (1)
      write (outbuf,100 )   
  100 format (t10, 'Bus', t27, 'Zone', t35, 'Type', t43, 'State', t52,  
     1        '      Base value', t80, '   Perturbed value') 
      call prtout (1)   
      call space (1)
      ntyp = kbsdta(1,nb_prm)   
      write (outbuf,110 ) bus(nb_prm), base(nb_prm), zone(nb_prm), 
     &   bustyp(ntyp), state  
  110 format (t10, a8, f6.1, t27, 2x, a2, t35, 2x, a1, t43, 1x, a4) 
C  
C     Print out perturbed values (V, P, Q, %P, or %Q).   
C  
      if (typprm(1)(1:1) .eq. 'V') then 
         vnew = dsqrt (e(kt) ** 2 + f(kt) ** 2)  
         if (abs (vnew - valnew(1)) .gt. 0.001) then
            write (errbuf(1), 112) bus(nb_prm), base(nb_prm), 
     &        valnew(1), vnew   
  112       format ('CHANGE_PARAMETER discrepency: Bus ', a8, f6.1, 
     1         ' Projected voltage (', f6.3,') >< actual voltage (',
     2         f6.3, ')')   
            call prterx ('W', 1)
         endif  
         vold = valold(1)   
         write (outbuf(52:),120 ) vold, vnew
  120    format (f8.3, ' V (per unit)', 
     1      t29, f8.3, ' V (per unit)') 
         call prtout (1)
        
      else if (typprm(1)(1:1) .eq. 'Q') then
         qnew = (qnetu(kt) + qloadu(kt)) * bmva   
         if (abs (qnew - valnew(1)) .gt. 0.1) then  
            write (errbuf(1), 122) bus(nb_prm), base(nb_prm), 
     &        valnew(1), qnew   
  122       format ('CHANGE_PARAMETER discrepency: Bus ', a8, f6.1, 
     1         ' Projected Q_gen (', f7.1,') >< actual Q_gen (',
     2         f7.1, ')')   
            call prterx ('W', 1)
         endif  
         qold = valold(1)   
         write (outbuf(52:),130) qold, qnew 
  130    format (f8.1, ' Q_gen (MVAR)', 
     1      t28, f8.1, ' Q_gen (MVAR)')   
         call prtout (1)
        
      else if (typprm(1)(1:1) .eq. 'P') then
         pnew = (pnetu(kt) + ploadu(kt)) * bmva           
         if (abs (pnew - valnew(1)) .gt. 0.1) then  
            write (errbuf(1), 132) bus(nb_prm), base(nb_prm), 
     &        valnew(1), pnew   
  132       format ('CHANGE_PARAMETER discrepency: Bus ', a8, f6.1, 
     1         ' Projected P_gen (', f7.1,') >< actual P_gen (', 
     2         f7.1, ')')   
            call prterx ('W', 1)
         endif  
         pold = valold(1)   
         write (outbuf(52:),140 ) pold, pnew
  140    format (f8.1, ' P_gen (MW)',   
     1      t28, f8.1, ' P_gen (MW)')   
         call prtout (1)
        
      else if (typprm(1)(1:2) .eq. '%P') then   
         pold = valold(1)   
         pnew = valnew(1)   
         write (outbuf(52:),142) pold, pnew 
  142    format (f8.1, ' Load (MW)',
     1      t29, f8.1, ' Load (MW)')
         call prtout (1)
        
      else if (typprm(1)(1:2) .eq. '%Q') then   
         qold = valold(1)   
         qnew = valnew(1)   
         write (outbuf(52:),144) qold, qnew 
  144    format (f8.1, ' Load (MVAR)',  
     1      t29, f8.1, ' Load (MVAR)')  
         call prtout (1)
        
      endif 
C 
C     Compute and printout monitored values (V, P, or Q).     
C 
      if (typprm(2)(1:1) .eq. 'V') then 
         valnew(2) = dsqrt (e(kt) ** 2 + f(kt) ** 2) 
         vnew = valnew(2)   
         vold = valold(2)   
         write (outbuf,150 ) vold, vnew 
  150    format (t52, f8.3, ' V (per unit)',
     1           t80, f8.3, ' V (per unit)')
         call prtout (1)
        
      else if (typprm(2)(1:1) .eq. 'Q') then
         valnew(2) = (qk + qloadu(kt)) * bmva      
         qnew = valnew(2)   
         qold = valold(2)   
         call allocq (nb_prm, qk, qgen, qgnmax, qgnmin, qld, totcap,
     1                usecap, totrek, userek, unsked, qerr) 
         vlqnew(1,2) = qk   
         vlqnew(2,2) = qgen 
         vlqnew(3,2) = qld  
         vlqnew(4,2) = totcap   
         vlqnew(5,2) = usecap   
         vlqnew(6,2) = totrek   
         vlqnew(7,2) = userek   
         vlqnew(8,2) = unsked   
         vlqnew(9,2) = qerr 
        
         write (outbuf,160 ) vlqold(2,2), qgen  
  160    format (t52, f8.1, ' Q gen (MVAR)',
     1           t80, f8.1, ' Q gen (MVAR)')
         call prtout (1)
         write (outbuf,161 ) vlqold(3,2), qld   
  161    format (t52, f8.1, ' Q load (MVAR)',   
     1           t80, f8.1, ' Q load (MVAR)')   
         call prtout (1)
         if (totcap .gt. 0.0) then  
            write (outbuf,162 ) vlqold(4,2), totcap 
  162       format (t52, f8.1, ' Scheduled Caps (MVAR)',
     1              t80, f8.1, ' Scheduled Caps (MVAR)')
            call prtout (1) 
            write (outbuf,163 ) vlqold(5,2), usecap 
  163       format (t52, f8.1, ' Used Caps (MVAR)', 
     1              t80, f8.1, ' Used Caps (MVAR)') 
            call prtout (1) 
         endif  
         if (totrek .lt. 0.0) then  
            write (outbuf,164 ) vlqold(6,2), totrek 
  164       format (t52, f8.1, ' Scheduled Reactors (MVAR)',
     1              t80, f8.1, ' Scheduled Reactors (MVAR)')
            call prtout (1) 
            write (outbuf,165 ) vlqold(7,2), userek 
  165       format (t52, f8.1, ' Used Reactors (MVAR)', 
     1              t80, f8.1, ' Used Reactors (MVAR)') 
            call prtout (1) 
         endif  
         if (unsked .ne. 0.0) then  
            write (outbuf,166 ) vlqold(8,2), unsked 
  166       format (t52, f8.1, ' Unscheduled Reactive (MVAR)',  
     1              t80, f8.1, ' Unscheduled Reactive (MVAR)')  
            call prtout (1) 
         endif  
         kt = inp2opt(nb_prm) 
C       
C        Set options for bus sensitivity dQ/dV: 
C       
C        KSPARE(19) = LTC option
C        KSPARE(20) = Area Interchange option   
C        KSPARE(39) = BQ shunt is adjusable 
C        KSPARE(40) = BG Q-generation is adjustable 
C       
         kspare(19) = iopton(16)
         kspare(20) = iopton(17)
         kspare(39) = 0 
         kspare(40) = 0 
C       
C        Temporarily change bus type to PQ. 
C       
         ktemp = kvolt(kt)  
         kvolt(kt) = 0  
C       
C        Compute bus sensitivity dQ/dV. 
C       
         call senfac
         do 168 i = 1, ntot + ntota 
            dpt(1,i) = 0.0  
            dpt(2,i) = 0.0  
  168    continue   
         dpt(2,kt+ntota) = 1.0  
         call baksen (0)
C       
C        Restore bus type   
C       
         kvolt(kt) = ktemp  
         if (dpt(2,kt+ntota) .ne. 0.0) then 
C       
C           Compute the sensitivity dV/dQ.  
C       
            vk = dsqrt (e(kt) ** 2 + f(kt) ** 2) 
            dvdq = vk * dpt(2,kt+ntota) * intbas(kt) / bmva 
            write (outbuf, 169 ) dvdq   
  169       format (t80, f8.4, ' Sensitivity dV/dQ (KV/MVAR)')  
            call prtout (1) 
         endif  
        
      else if (typprm(2)(1:1) .eq. 'P') then
         valnew(2) = (pk + ploadu(kt)) * bmva      
         pnew = valnew(2)   
         pold = valold(2)   
         write (outbuf,170 ) pnew   
  170    format (t52, f8.1, ' P_gen (MW)')  
         call prtout (1)
        
      endif 
C       
C     Write results to .QVPT file   
C       
C     First time - enter base values to file
C       
      if (numprm .eq. 0) then   
         numprm = numprm + 1
         if (typprm(1)(1:2) .eq. '%P' .or. typprm(1)(1:2) .eq. '%Q')
     1      then
            if (index ('X ', typprm(1)(3:3)) .ne. 0) then   
               xvalue = valold(1)   
               yvalue = valold(2)   
               xpoint = typprm(1)(1:2)  
               ypoint = typprm(2)(1:1)  
            else
               yvalue = valold(1)   
               xvalue = valold(2)   
               ypoint = typprm(1)(1:2)  
               xpoint = typprm(2)(1:1)  
            endif   
         else if (typprm(2)(1:1) .eq. 'Q') then 
            if (index ('X ', typprm(2)(2:2)) .ne. 0) then   
               xvalue = vlqold(2,2) + vlqold(5,2)   
     1                + vlqold(7,2) + vlqold(8,2)   
               yvalue = valold(1)   
               xpoint = typprm(2)(1:1)  
               ypoint = typprm(1)(1:1)  
            else
               yvalue = vlqold(2,2) + vlqold(5,2)   
     1                + vlqold(7,2) + vlqold(8,2)   
               xvalue = valold(1)   
               ypoint = typprm(2)(1:1)  
               xpoint = typprm(1)(1:1)  
            endif   
         else   
            if (index ('X ', typprm(2)(2:2)) .ne. 0) then   
               xvalue = valold(2)   
               yvalue = valold(1)   
               xpoint = typprm(2)(1:1)  
               ypoint = typprm(1)(1:1)  
            else
               yvalue = valold(2)   
               xvalue = valold(1)   
               ypoint = typprm(2)(1:1)  
               xpoint = typprm(1)(1:1)  
            endif   
         endif  
         write (tempc1, '(bz, f20.10)') xvalue   
         write (tempc2, '(bz, f20.10)') yvalue   
C       
C        Left-justify encoded numbers   
C       
         tempc1 = ljstfy (tempc1)   
         tempc2 = ljstfy (tempc2)   
        
         i1 = lastch (tempc1)   
         i2 = lastch (tempc2)   
        
         write (23, 172) tempc1(1:i1), tempc2(1:i2) 
  172    format(a, 1x, a)   
        
         write (outbuf, 174) numprm, xpoint, tempc1(1:i1),  
     1      ypoint, tempc2(1:i2)
  174    format ('0 PLOT POINT ', i3, ' X (', a, ') = ', a, 1x, 
     1      ' Y (', a, ') = ', a)   
         call prtout (1)
      endif 
        
      numprm = numprm + 1   
      if (typprm(1)(1:2) .eq. '%P' .or. typprm(1)(1:2) .eq. '%Q') then  
         if (index ('X ', typprm(1)(3:3)) .ne. 0) then  
            xvalue = valnew(1)  
            yvalue = valnew(2)  
            xpoint = typprm(1)(1:2) 
            ypoint = typprm(2)(1:1) 
         else   
            yvalue = valnew(1)  
            yvalue = valnew(2)  
            ypoint = typprm(1)(1:2) 
            xpoint = typprm(2)(1:1) 
         endif  
      else if (typprm(2)(1:1) .eq. 'Q') then
         if (index ('X ', typprm(2)(2:2)) .ne. 0) then  
            xvalue = vlqnew(2,2) + vlqnew(5,2)  
     1             + vlqnew(7,2) + vlqnew(8,2)  
            yvalue = valnew(1)  
            xpoint = typprm(2)(1:1) 
            ypoint = typprm(1)(1:1) 
         else   
            yvalue = vlqnew(2,2) + vlqnew(5,2)  
     1             + vlqnew(7,2) + vlqnew(8,2)  
            xvalue = valnew(1)  
            ypoint = typprm(2)(1:1) 
            xpoint = typprm(1)(1:1) 
         endif  
      else  
         if (index ('X ', typprm(2)(2:2)) .ne. 0) then  
            xvalue = valnew(2)  
            yvalue = valnew(1)  
            xpoint = typprm(2)(1:1) 
            ypoint = typprm(1)(1:1) 
         else   
            yvalue = valnew(2)  
            xvalue = valnew(1)  
            ypoint = typprm(2)(1:1) 
            xpoint = typprm(1)(1:1) 
         endif  
      endif 
      write (tempc1, '(bz, f20.10)') xvalue  
      write (tempc2, '(bz, f20.10)') yvalue  
C       
C     Left-justify encoded numbers  
C       
      tempc1 = ljstfy (tempc1)  
      tempc2 = ljstfy (tempc2)  
        
      i1 = lastch (tempc1)  
      i2 = lastch (tempc2)  
        
      write (23, 172) tempc1(1:i1), tempc2(1:i2)
        
      write (outbuf, 174) numprm, xpoint, tempc1(1:i1), 
     1  ypoint, tempc2(1:i2)
      call prtout (1)   
        
  180 continue  
      return
      end   
