C    @(#)getprm.f	20.9 11/12/98
      subroutine getprm (bigbuf, error) 
      integer error
      character *(*) bigbuf
C 
C     process /CHANGE_PARAMETER commands. Following  
C     are bus records depicting the change values.   
C 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/bxlock.inc'
      include 'ipfinc/chgprm.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/snput.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/xdata.inc'
 
      double precision suscp, qsuscp, vnew, dv2
      integer find_bus, firstxstr
      character bus1*8, word(100)*30, capital*30, tempc*30,
     1          type*1, ljstfy*512
      logical plist
      external find_bus
 
      error = 0
      plist = .true.
 
      tempc = capital(bigbuf(1:30))
      if (index (tempc,'CHANGE_PAR') .ne. 0 .or.
     1    index (tempc,'CHANGEPAR') .ne. 0) then
C                                                                      *
C        / CHANGE_PARAMETERS, BUS = <name> <base>, VY=###, QX=? 
C                                                  VY=###, PX=?
C                                                  QX=###, VY=? 
C                                                  PX=###, VY=? 
C                                                  PX=###, QX=? 
C       
C        / CHANGE_PARAMETERS, BUS = <name> <base>, VY=###, QX=? -
C                                                  VY=###, PX=? -
C                                                  QX=###, VY=? -
C                                                  PX=###, VY=? -
C                                                  PX=###, QX=? -   
C                                                  VY=? <   
C        / CHANGE_PARAMETERS, %LOAD_CHANGE, %PX = <##>, %QY = <##>, -
C                                                  AREAS = NORTHWEST, - 
C                                                  ZONES = NB, -
C                                                  OWNERS = PSP, -  
C                                                  TYPES = N <  
C                                                                      *
C        / CHANGE_PARAMETERS, %GEN_CHANGE, %PX = <##>, %QY = <##>, -
C                                                  AREAS = NORTHWEST, - 
C                                                  ZONES = NB, -
C                                                  OWNERS = PSP, -  
C                                                  TYPES = N <  
C
C        / CHANGE_PARAMETERS, CHANGES = *
C        B M   <bus> <base>
C        + M   <bus> <base>
C        T M   <bus1> <base1> <bus2> <base2> <id> <sect>
C        L M   <bus1> <base1> <bus2> <base2> <id> <sect>
C        E M   <bus1> <base1> <bus2> <base2> <id> <sect>
C
C        TYPPRM(1) = Perturbed parameter
C              (2) = Monitored parameter
C       
         typprm(1) = ' '
         typprm(2) = ' '
        
         call space (1) 
         write (outbuf,110 ) buf(1:80)  
  110    format (' CHANGE_PAR text (', a, ')')  
         call prtout (1)
        
         call scan(bigbuf,word,nwrd)
        
         if (index (bigbuf, '%LOAD') .ne. 0) then   
C       
C           CHGLOD will process and strip the %LOAD commands in WORD(). 
C       
            call chglod (nwrd, word, pctp, pctq, oldtp, oldtq, totalp,  
     1         totalq, type, error) 
            if (pctp .ne. 0.0) then 
               typprm(1) = '%P' 
               valold(1) = oldtp
C       
C              Search for possible %PX or %PY.  
C       
               do i = 1, nwrd 
                  if (word(i)(1:2) .eq. '%P') typprm(1) = word(i)   
               enddo
            else if (pctq .ne. 0.0) then
               typprm(1) = '%Q' 
               valold(1) = oldtq
C       
C              Search for possible %QX or %QY.  
C       
               do i = 1, nwrd 
                  if (word(i)(1:2) .eq. '%Q') typprm(1) = word(i)   
               enddo
            else
               write (errbuf(1),112) pctp, pctq
  112          format ('CHANGE_PAR - %LOAD_CHANGE has meaningless ',
     &                 'percentages (', f6.1, ',', f6.1, ')')
               call prterx ('W', 1)
               error = 1
            endif   
            if (type .eq. 'P') then
               valnew(1) = totalp  
            else   
               valnew(1) = totalq  
            endif  
         endif  

         if (index (bigbuf, '%GEN') .ne. 0) then   
C       
C           CHGGEN will process and strip the %GEN commands in WORD(). 
C       
            call chggen (nwrd, word, pctp, pctq, oldtp, oldtq, totalp,  
     1         totalq, type, error) 
            if (pctp .ne. 0.0) then 
               typprm(1) = '%P' 
               valold(1) = oldtp
C       
C              Search for possible %PX or %PY.  
C       
               do i = 1, nwrd 
                  if (word(i)(1:2) .eq. '%P') typprm(1) = word(i)   
               enddo
            else if (pctq .ne. 0.0) then
               typprm(1) = '%Q' 
               valold(1) = oldtq
C       
C              Search for possible %QX or %QY.  
C       
               do i = 1, nwrd 
                  if (word(i)(1:2) .eq. '%Q') typprm(1) = word(i)   
               enddo
            else
               write (errbuf(1),114) pctp, pctq
  114          format ('CHANGE_PAR - %GEN_CHANGE has meaningless ',
     &                 'percentages (', f6.1, ',', f6.1, ')')
               call prterx ('W', 1)
               error = 1
            endif   
            if (type .eq. 'P') then
               valnew(1) = totalp  
            else   
               valnew(1) = totalq  
            endif  
         endif  
        
         iwrd = 2
         if (firstxstr(word(iwrd), 'FILE') .ne. 0) iwrd = iwrd +  2
         if (word(iwrd) .eq. 'BUS') then   
            bus1 = word(iwrd+1)  
            last = lastch(word(iwrd+2))
            base1 = ftn_atof (word(iwrd+2)(1:last))
            nb_prm = find_bus (bus1, base1)   
            if (nb_prm .le. 0) then 
               write (errbuf(1),130) bus1, base1
  130          format ('CHANGE_PAR (', a8, f6.1, ') is not in system.') 
               call prterx ('W', 1) 
               error = 1
            else
C                                                                      *
C              Process parameters                                      *
C                                                                      *
               kt = inp2opt(nb_prm)   
C       
C              Compute most recent values of PK, QK, etc.   
C       
               call nrpqv (kt,pk,dpk,qk,dqk,vk) 
               ntyp = kbsdta(1,nb_prm)  
               pload = ploadu(kt)                  
               qload = qloadu(kt)

C              Find TBX index.

               if (ntyp .eq. 6 .or. ntyp .eq. 7 .or. ntyp .eq. 8 .or.   
     1             ntyp .eq. 9 .or. ntyp .eq. 11) then  
                  do 132 i = 1, ntotb   
                      ltyp = tbx(1,i)  
                      if (ltyp .lt. 10) then
                         if (tbx(2,i) .eq. kt) then
                            ntbx_prm = i
                            go to 134   
                         endif  
                      endif 
  132             continue  
                  ntbx_prm = 0  
  134             continue  
               else 
                  ntbx_prm = 0  
               endif
C                                                                      *
C              Find Area or system slack bus index.                    *
C                                                                      *
               i1 = iflag(kt)   
               i2 = iflag(kt+1) - 1 
               do 136 j = i1, i2
                  if (jflag(1,j) .eq. 3) then   
                     nare_prm = jflag(2,j)  
                     go to 138  
                  endif 
  136          continue 
               nare_prm = 0 
  138          continue 
        
               do 260 i = iwrd+3, nwrd - 1, 2
                  word(i) = capital (word(i))
                  if (word(i)(1:1) .eq. 'V') then   
                     if (word(i+1) .ne. '?') then   
                        if (ntbx_prm .ne. 0) then   
                           call typno (type, ntyp)  
                           write (errbuf(1),140) bus1, base1, 'B'//type,
     1                        'BE'  
  140                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                             ') is changing bus type from ', a2, 
     2                             ' to ', a2)   
                           call prterx ('W', 1) 
                           call chgbty (nb_prm, 2, ntbx_prm, 0, plist) 
                           ntbx_prm = 0 

C                          Routine CHGBTY alters Tables. Retrieve 
C                          updated values.  

                        endif   
                        if (kvolt(kt) .eq. 0) then  
                           write (errbuf(1),142) bus1, base1
  142                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                            ') is changed to be V-constrained.')  
                           call prterx ('W', 1) 
                           kvolt(kt) = kt   
                           volt(kt) = 0.0   
                           ntyp = 2 
                           kbsdta(1,nb_prm) = ntyp  
                        endif   
                        if (typprm(1) .ne. ' ') then
                           write (errbuf(1),150) bus1, base1
  150                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                       ') has duplicate parameters perturbated.') 
                           call prterx ('W', 1) 
                           error = 1
                        else
                           typprm(1) = word(i)  
                           valold(1) = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                           read (word(i+1),144) valnew(1)   
  144                      format (bz, f10.0)   
                        endif   
                     else   
                        if (ntbx_prm .ne. 0) then   
                           call typno (type, ntyp)  
                           write (errbuf(1),170) bus1, base1, 'B'//type,
     1                        'B '  
  170                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                        ') is changing bus type from ', a2,   
     2                        ' to ', a2)   
                           call prterx ('W', 1) 
                           call chgbty (nb_prm, 1, ntbx_prm, 0, plist) 
                           ntbx_prm = 0 
                        endif   
                        if (kvolt(kt) .ne. 0) then  
                           write (errbuf(1),172 ) bus1, base1   
  172                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                            ') is changed to be Q-constrained.')  
                           call prterx ('W', 1) 
                           kvolt(kt) = 0
                           volt(kt) = 0.0   
                           ntyp = 1 
                           kbsdta(1,nb_prm) = ntyp  
                        endif   
                        if (typprm(2) .ne. ' ') then
                           write (errbuf(1),180 ) bus1, base1   
  180                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                        ') has duplicate parameters monitored.')  
                           call prterx ('W', 1) 
                           error = 1
                        else
                           typprm(2) = word(i)  
                           valold(2) = dsqrt (e(kt) ** 2 + f(kt) ** 2)
                           valnew(2) = -9.0e10  
                        endif   
                     endif  
        
                  else if (word(i)(1:1) .eq. 'Q') then  
                     if (word(i+1) .ne. '?') then   
                        if (ntbx_prm .ne. 0) then   
                           call typno (type, ntyp)  
                           write (errbuf(1),190) bus1, base1, 'B'//type,
     1                        'B '  
  190                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                        ') is changing bus type from ', a2,   
     2                        ' to ', a2)   
                           call prterx ('W', 1) 
                           call chgbty (nb_prm, 1, ntbx_prm, 0, plist) 
                           ntbx_prm = 0 
                        endif   
                        if (kvolt(kt) .ne. 0) then  
                           write (errbuf(1),192 ) bus1, base1   
  192                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                            ') is changed to be Q-constrained.')  
                           call prterx ('W', 1) 
                           kvolt(kt) = 0
                           volt(kt) = 0.0   
                           ntyp = 1 
                           kbsdta(1,nb_prm) = ntyp  
                        endif   
                        if (typprm(1) .ne. ' ') then
                           write (errbuf(1),150) bus1, base1
                           call prterx ('W', 1) 
                           error = 1
                        else
                           typprm(1) = word(i)  

C                          Store Q_gen in VALOLD and VALNEW.  

                           valold(1) = (qk + qload) * bmva  
                           call allocq (nb_prm, qk, qgen, qgnmax, 
     &                                  qgnmin, qld, totcap, usecap, 
     &                                  totrek, userek, unsked, qerr)   
                           vlqold(1,1) = qk 
                           vlqold(2,1) = qgen   
                           vlqold(3,1) = qld
                           vlqold(4,1) = totcap 
                           vlqold(5,1) = usecap 
                           vlqold(6,1) = totrek 
                           vlqold(7,1) = userek 
                           vlqold(8,1) = unsked 
                           vlqold(9,1) = qerr   
        
                           read (word(i+1),144) q   
                           valnew(1) = q
                        endif   
                     else   
                        if (ntbx_prm .ne. 0) then   
                           call typno (type, ntyp)  
                           write (errbuf(1),200) bus1, base1, 'B'//type,
     1                        'BE'  
  200                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                        ') is changing bus type from ', a2,   
     2                        ' to ', a2)   
                           call prterx ('W', 1) 
                           call chgbty (nb_prm, 2, ntbx_prm, 0, plist) 
                           ntbx_prm = 0 
                        endif   
                        if (kvolt(kt) .eq. 0) then  
                           write (errbuf(1),202) bus1, base1
  202                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                        ') is changed to be V-constrained.')  
                           call prterx ('W', 1) 
                           kvolt(kt) = kt   
                           volt(kt) = 0.0   
                           ntyp = 2 
                           kbsdta(1,nb_prm) = ntyp  
                        endif   
                        if (typprm(2) .ne. ' ') then
                           write (errbuf(1),180 ) bus1, base1   
                           call prterx ('W', 1) 
                           error = 1
                        else
                           typprm(2) = word(i)  

C                          Store Q_gen in VALOLD and VALNEW.  

                           valold(2) = (qk + qload) * bmva  
                           valnew(2) = -9.0e10  
                           call allocq (nb_prm, qk, qgen, qgnmax, 
     &                                  qgnmin, qld, totcap, usecap, 
     &                                  totrek, userek, unsked, qerr)   
                           vlqold(1,2) = qk 
                           vlqold(2,2) = qgen   
                           vlqold(3,2) = qld
                           vlqold(4,2) = totcap 
                           vlqold(5,2) = usecap 
                           vlqold(6,2) = totrek 
                           vlqold(7,2) = userek 
                           vlqold(8,2) = unsked 
                           vlqold(9,2) = qerr   
                        endif   
                     endif  
                  else if (word(i)(1:1) .eq. 'P') then  
                     if (word(i+1) .ne. '?') then   
                        if (nare_prm .ne. 0) then   
                           write (errbuf(1),230) bus1, base1
  230                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                        ') is not P-constrained.')
                           call prterx ('W', 1) 
                           error = 1
                        else if (typprm(1) .ne. ' ') then   
                           write (errbuf(1),150) bus1, base1
                           call prterx ('W', 1) 
                           error = 1
                        else
                           typprm(1) = word(i)  

C                          Store P_gen in VALOLD and VALNEW.

                           valold(1) = (pnetu(kt) + pload) * bmva   
                           read (word(i+1),144) p   
                           valnew(1) = p
                        endif   
                     else   
                        if (nare_prm .eq. 0) then   
                           write (errbuf(1),240) bus1, base1
  240                      format ('CHANGE_PARAMETER bus (', a8, f6.1,  
     1                        ') monitored parameter is P-constrained.' 
     2                        ) 
                           call prterx ('W', 1) 
                           error = 1
                        else if (typprm(2) .ne. ' ') then   
                           write (errbuf(1),180 ) bus1, base1   
                           call prterx ('W', 1) 
                           error = 1
                        else
                           typprm(2) = word(i)  

C                          Store P_gen in VALOLD and VALNEW.   

                           valold(2) = (pnetu(kt) + pload) * bmva   
                           valnew(2) = -9.0e10  
                        endif   
                     endif  
                  else  
                     last = lastch (word(i))
                     write (errbuf(1),250 ) word(i)(1:last) 
  250                format ('Unrecognized CHANGE_PARAMETER key (', a,  
     1                  ')')
                     call prterx ('E', 1)   
                     error = 1  
                  endif 
  260          continue 
               if (typprm(1) .ne. ' ' .and. typprm(2) .ne. ' ') then
               else if (typprm(1) .eq. ' ') then
                  write (errbuf(1),270 )
  270             format ('Incomplete CHANGE_PARAMETER specification: ',
     &                    'no perturbed quantity.')
                  call prterx ('E', 1)
                  error = 1
               else if (typprm(2) .eq. ' ') then
                  write (errbuf(1),280 )
  280             format ('Incomplete CHANGE_PARAMETER specification: ',
     1                    'no monitored quantity.')
                  call prterx ('E', 1)
                  error = 1
               endif
            endif   
         else   
            last = lastch (word(iwrd)) 
            write (errbuf(1),290 ) word(iwrd)(1:last)  
  290       format ('Unrecognized CHANGE_PARAMETER key (', a, ')')  
            call prterx ('E', 1)
            error = 1   
         endif  
      else  
         error = 1  
      endif 
        
      if (error .eq. 0) then
        
         if (typprm(1)(1:1) .eq. 'V') then  

C           Change V-constraints. 

            vold = valold(1)
            vnew = valnew(1)
            e(kt) = e(kt) * valnew(1) / valold(1)   
            f(kt) = f(kt) * valnew(1) / valold(1)   
            if (ntyp .eq. 2 .or. ntyp .eq. 3 .or. ntyp .eq. 7 .or.  
     1          ntyp .eq. 8 .or. ntyp .eq. 11) then 
               busdta(11,nb_prm) = vnew 
               busdta(12,nb_prm) = vnew 
               vlimn(kt) = vnew   
               vlimx(kt) = vnew   
            endif   
            if (ntbx_prm .gt. 0) then   
               ltyp = tbx(1,ntbx_prm)  
               ityp = tbx(2,ntbx_prm)  
               suscp = tbx(6,ntbx_prm)
               qsuscp = suscp * vnew ** 2   
               if (ltyp .eq. 2) then
                  dv2 = (vnew - vold) ** 2  
                  tbx(3,ntbx_prm) = tbx(3,ntbx_prm) 
     &                            - dv2 * dmin1 (0d0, suscp)  
                  tbx(4,ntbx_prm) = tbx(4,ntbx_prm) 
     &                            - dv2 * dmax1 (0d0, suscp)  
                  tbx(5,ntbx_prm) = vnew
                  if (ityp .eq. 3) then 

C                    Set new Q_min limit.   

                     if (suscp .gt. 0.0) then   
                        qnetu(kt) = tbx(4,ntbx_prm) + qsuscp   
                     else   
                        qnetu(kt) = tbx(4,ntbx_prm)
                     endif  
        
                  else if (ityp .eq. 4) then

C                    Set new Q_max limit.  

                     if (suscp .lt. 0.0) then   
                        qnetu(kt) = tbx(3,ntbx_prm) + qsuscp   
                     else   
                        qnetu(kt) = tbx(3,ntbx_prm)
                     endif  
                  endif 
               endif
            endif   
        
         else if (typprm(1)(1:1) .eq. 'Q') then 

C           Change Q-constraints.  
C           (Q_gen in MVAR is stored in VALOLD and VALNEW.)

            qold = valold(1)
            qnew = valnew(1)
            dq = (qnew - qold) / bmva   
            if (ntyp .eq. 1 .or. ntyp .eq. 43 .or. ntyp .eq. 6 .or. 
     1          ntyp .eq. 10 .or. ntyp .eq. 13) then
               busdta(9,nb_prm) = busdta(9,nb_prm) + qnew - qold
               qnetu(kt) = qnew / bmva - qload 
            endif   
            if (ntbx_prm .gt. 0) then   
               ltyp = tbx(1,ntbx_prm)  
               ityp = tbx(2,ntbx_prm)  
               if (ltyp .eq. 2) then
                  tbx(3,ntbx_prm) = tbx(3,ntbx_prm) + dq
                  if (ityp .eq. 3) then 
                  else if (ityp .eq. 4) then

C                    Set new Q_max limit.   

                     qnetu(kt) = qnetu(kt) +dq
                  endif 
               else if (ltyp .eq. 3) then   
                  tbx(3,ntbx_prm) = tbx(3,ntbx_prm) + dq
                  if (ityp .eq. 2) then 
                  else if (ityp .eq. 3) then

C                    Set new Q_max limit.  

                     qnetu(kt) = qnetu(kt) +dq
                  endif 
               endif
            endif   
         else if (typprm(1)(1:1) .eq. 'P') then 

C           Change P-constraints. 
C           (P_gen in MW is stored in VALOLD and VALNEW.) 

            pold = valold(1)
            pnew = valnew(1)
            busdta(8,nb_prm) = busdta(8,nb_prm) + pnew - pold   
            pnetu(kt) = pnew / bmva - pload
         endif  
         ntypu(kt) = ntyp
      endif  

C     Read next input record. 

  300 read (inp, 310, end=900) buf   
  310 format (a) 
      card = buf(1:1)
      write (outbuf,110 ) buf(1:80)  
      call prtout (1)
      if (card .eq. '.') then
         go to 300   
      endif  
      if (card .eq. '/') then   
         if (index (buf,'CHANGE_PAR') .ne. 0 .or.   
     1       index (buf, 'CHANGEPAR') .ne. 0) then  
         else   
            call ctlpow 
         endif  
      else if (card .eq. '(') then  
      else  
  380    read (inp, 310, end=900) buf   
         card = buf(1:1)
         write (outbuf,110 ) buf(1:80)  
         call prtout (1)
        
         if (card .eq. '(') then
            call ctlpow 
         else if (card .eq. '/') then   
            if (index (buf,'CHANGE_PAR') .ne. 0 .or.
     1          index (buf, 'CHANGEPAR') .ne. 0) then   
            else
               call ctlpow  
            endif   
         else if (card .eq. '.') then   
            go to 380   
         else   
            write (errbuf(1), 370) buf(1:80)
  370       format (' COMMAND IGNORED (', a, ')')   
            call prterx ('W', 1)
            go to 380   
         endif  
      endif 
      go to 910 
  900 buf = '( END ) GETPRM'
      card = buf(1:1)   
  910 return
      end   
