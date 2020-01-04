C    %W% %G%
      subroutine deriv 
 
C     This subroutine is the calling routine for the network 
C     solution.  It calls the various subroutines needed to calculate 
C     the injected current vector and then calls the routines needed 
C     to obtain a new network solution.  It is called by CNTRL.  It 
C     calls DCLIN, DWNBAK, GENCUR, GENUPD, KEYBRD, LODCUR, MDCLIN, new 
C     PRTERR, PRTOUT, REDUC1, XTIME, and ZNOSOL. 
 
      include 'tspinc/params.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/buslod.inc' 
      include 'tspinc/igentn.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/machd2.inc' 
      include 'tspinc/ldcc.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/contrl.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/search.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/lnk12a.inc' 
      include 'tspinc/link2.inc' 
      include 'tspinc/lnk1a.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/equiv.inc' 
      include 'tspinc/brake1.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/mdctbl.inc' 
      include 'tspinc/dcblk.inc' 
      include 'tspinc/ecstbb.inc' 
      include 'tspinc/ecstbh.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/delcr.inc' 
      include 'tspinc/ecstbj.inc' 
      include 'tspinc/bcur.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/znox.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/fltopt.inc' 
      include 'tspinc/iter.inc' 
      include 'tspinc/lowzno.inc' 
      include 'tspinc/deltfq.inc' 
 
      common /mytr/mytr 
      common /satsw/satsw 
 
      character*8 gname, busnam 
      character*1 gid 
      character*8 name 
 
      call xtime(4) 
 
C     Values of IILOCF and IINDXZ are offset to reflect relative 
C     locations of ILOCFT and INDXZ in /ECSTBJ/ 
 
      iilocf = 1 
      iindxz = 1 
      nmxt = nmx 
      if (iesc .ne. 1) nmxt = nmx 
      iter = 1 
      ido = 1 
      i5065 = 1 
      idvrg = 1 
      dresid = 999999.0 
      dsum = 0.0 
      icur = 0 
      imcur = 0 
      imax = 1 
      zero = 0.0 
 
C     YISOLN, NEWTON option 
 
C     setup code: IIDSW7 =  1 -> bypass reduce 
C     IIDSW7 =  2 -> xeq reduce 
 
      do while (.true.) 
        if (idsw .eq. 7 .and. iter .eq. 1) then 
          iidsw7 = 1 
        else 
          iidsw7 = 2 
        endif 
 
C       New timestep and first itereration 
 
        if (lppwr .eq. 0 .and. iter .eq. 1) then 
          newdt = 1 
        else 
          newdt = 2 
        endif 
 
C       Newton or Y-matrix solution option 
 
        if (iidsw7 .eq. 1 .or. idsw .ne. 7 .and. newdt .eq. 1) then 
          mdeyoj = 1 
        else 
          mdeyoj = 2 
        endif 
        cmax = 0.0 
 
C       Call GENCUR to calculate current injections at the machines 
 
        call gencur() 
 
C       IPWR = 1 means all loads are constant impedance, so skip 
C       load current injection calculations 
 
        if (ipwr .ne. 1) then 
 
C         If ITER=1 and LPPWR=0 set all loads to constant impedance 
 
          if (iter .eq. 1 .and. lppwr .eq. 0) then 
            cmax = 10.0 
            do i = 1, nmx 
              delcr(i) = 0.0 
            enddo 
          else 
 
C           Call LODCUR to calculate load current injection estimate 
 
            call lodcur() 
            goto 100 
          endif 
        endif 
 
C       Call ZNOSOL to find equivalent admittance of zno capacitor 
 
        do i = 1, nmx 
          bcurr(i) = 0.0 
          bcuri(i) = 0.0 
        enddo 
  100   if (iznmax .gt. 0) call znosol() 
 
 
C       Call DCLIN to solve two terminal dc line equations 
 
        call xtime(-4) 
        call xtime(7) 
        if (ldc .ne. 0) call dclin(iter) 
 
C       Call MDCLIN to solve multi-terminal dc line logic 
 
        if (ldc1 .ne. 0) then 
          ldcc = 1 
          jecs1 = kdc1 
          do while (.true.) 
            if (mdcblk(ldcc) .ne. 1) then 
              call redecs(dcb, jecs1, msizeb) 
              if (iter .eq. 1) then 
                if (lppwr .eq. 0) then 
 
C                 Initialize delta curr storage 
 
                  do i = 1, nterm 
                    i1 = lbt + i 
                    call ritecs(zero, idcb(i1)+40, 1) 
                  enddo 
                  if (ldc .eq. 0) cmax = 10.0 
                  if (idsw .ne. 7) then 
 
C                   Process commutation voltage 
 
                    do i = 1, nterm 
                      i1 = lbt + i 
                      call redecs(ecn, idcb(i1)+41, 1) 
                      call ritecs(ecn, idcb(i1)+42, 1) 
                    enddo 
                  endif 
                endif 
              endif 
 
C             Call MDCLIN to solve dc multiterminal line equations 
 
              call mdclin(iter) 
              if (link .gt. 10) goto 190 
 
C             Store last dc calculation 
 
              call ritecs(dcb, jecs1, ndimc) 
            endif 
 
C           Tally multi-terminal dc counter 
 
            ldcc = ldcc + 1 
            if (ldcc .gt. ldc1) goto 110 
            jecs1 = kdcm(ldcc) 
          enddo 
        endif 
  110   call xtime(-7) 
        call xtime(4) 
 
C       BUSP and BUSQ temporarily hold the load injection currents, but 
C       ulitmately will contain bus load in WRTHIS 
 
        do itrr = 1, nmx 
          busp(itrr) = bcurr(itrr) 
          busq(itrr) = bcuri(itrr) 
        enddo 
 
C       Add current injection from generators to current vector 
 
        do i = 1, isg 
          i1 = igentn(1, i) 
          bcurr(i1) = bcurr(i1) + eyri(i) 
          bcuri(i1) = bcuri(i1) + eyii(i) 
        enddo 
        if (keybrd(13) .ne. 0) then 
          write (outbuf, 10000) iter 
10000     format (' 0  CURRENT AT S1260 ITER, EYR AND EYI', i10) 
          call prtout(1) 
          do i1 = 1, nmx, 4 
            k = i1 + 3 
            do jjj = i1, k, 8 
              kkk = min0(jjj+7, k) 
              write (outbuf, 10010) i1, (bcurr(j), bcuri(j), j = jjj,  
     &         kkk) 
10010         format (1x, i4, 8e13.6) 
              call prtout(1) 
            enddo 
          enddo 
        endif 
 
C       YISOLN, NEWTON option 
 
        if (inewts .eq. 2) then 
 
C         Newtons method solution 
 
          if (iidsw7 .ne. 1) then 
 
C           NEWDT = 1 means the first iteration for a time step which uses 
C           the admittance matrix, so call REDUCE to factorize the Y-matrix. 
C           NEWDT = 2 means use Newtons method for the remaining iterations, 
C           so call NEWTSL to obtain the nework solution. 
 
            if (newdt .eq. 2) then 
              call newtsl() 
              goto 120 
            else 
              lfrst = 1 
              nordk = 1 
              call reduc1() 
            endif 
          endif 
 
C       Admittance matrix solution 
 
        elseif (ncomp .ne. 0) then 
          if (iidsw7 .ne. 1) then 
 
C           Test if Y-matrix needs to be factorized 
 
            if (lowzno .ne. nmx+1) then 
              lfrst = lowzno 
              if (ido .eq. 2) then 
                ido = 1 
              else 
                nordk = 1 
                call reduc1() 
              endif 
            endif 
          endif 
        endif 
        call xtime(-4) 
        call xtime(6) 
 
C       Call DWNBAK to obtain new voltage vector 
 
        call dwnbak() 
        call xtime(-6) 
        call xtime(4) 
  120   if (keybrd(11) .ne. 0) then 
          write (outbuf, 10020) 
10020     format ('0', 10x, 'volts') 
          call prtout(1) 
          do i1 = 1, nmx, 4 
            k = i1 + 3 
            do jjj = i1, k, 8 
              kkk = min0(jjj+7, k) 
              write (outbuf, 10010) i1, (eyr(j), eyi(j), j = jjj, kkk) 
              call prtout(1) 
            enddo 
          enddo 
        endif 
 
C       If all machines are classical and all loads constant z 
C       there is no need to iterate 
 
        if (igtmax .eq. 1 .and. ipwr .eq. 1) goto 160 
 
C       Test for tolerance in network solution 
 
        if (idsw .eq. 7) then 
          if (iter .ne. 1) then 
            if (cmax .le. toli) goto 130 
          endif 
 
C         NCHCK = 1 means this is the initial solution so check for 
C         divergence after 3 iterations 
 
          if (nchck .ne. 2) then 
            if (iter .le. 3) then 
              dresid = dsum 
            elseif (dresid .ge. dsum) then 
              dresid = dsum 
            else 
              goto 150 
            endif 
          endif 
          if (keybrd(15) .ne. 0 .or. nchek .eq. 1) then 
            if (imax .gt. nmx) then 
              igi = imax - nmx 
              ibs = igentn(1, igi) 
              gname = bname(ibs) 
              gid = igentc(igi) 
              gkv = buskv(ibs) 
              write (outbuf, 10030) gname, gid, gkv, igi, cmax, iter,  
     &         dsum 
10030         format (' *$JA,K15-GEN ', a8, a1, f6.1, 'KV', i5, ' IGI',  
     &         f12.6, ' DELTA I', i3, ' ITER', f12.4, ' ABSUM') 
              call prtout(1) 
            else 
              busnam = bname(imax) 
              bkv = buskv(imax) 
              write (outbuf, 10040) busnam, bkv, imax, cmax, iter, dsum 
10040         format (' *$JA,K15-BUS ', a8, 1x, f6.1, 'KV', i5, 'IBUS',  
     &         f12.6, ' DELTA I', i3, ' ITER', f12.4, ' ABSUM') 
              call prtout(1) 
            endif 
          endif 
 
C         Test for iteration limit 
 
          if (iter .lt. ilim) then 
            iter = iter + 1 
            goto 140 
          endif 
 
  130     if (keybrd(14) .ne. 0) then 
            if (imax .gt. nmx) then 
              igi = imax - nmx 
              ibs = igentn(1, igi) 
              gname = bname(ibs) 
              gid = igentc(igi) 
              gkv = buskv(ibs) 
              write (outbuf, 10050) gname, gid, gkv, igi, cmax, iter,  
     &         dsum 
10050         format (' *$JA,K14-GEN ', a8, a1, f8.1, 'KV', i5, 'IGI',  
     &         f12.6, ' DELTA I', i3, ' ITER', f12.4, ' ABSUM') 
              call prtout(1) 
            else 
              busnam = bname(imax) 
              bkv = buskv(imax) 
              write (outbuf, 10060) busnam, bkv, imax, cmax, iter, dsum 
10060         format (' *$JA,K14- BUS ', a8, f8.1, 'KV', i6, 'IBUS',  
     &         f12.6, ' DELTA I', i3, ' ITER', f12.4, ' ABSUM') 
              call prtout(1) 
            endif 
          endif 
        endif 
 
C       Convergence within network tolerance has been satisfied 
 
        if (nchck .eq. 2) goto 180 
        if (.not. (cmax .gt. 0.001 .and. iter .le. 15)) goto 170 
        iter = iter + 1 
  140   continue 
      enddo 
  150 write (errbuf(1), 10070) 
10070 format ('Divergence on inititial balance') 
      call prterr('E', 1) 
      link = 13 
      call xtime(-4) 
      goto 190 
  160 if (nchck .ne. 1) goto 180 
 
C     Initial solution (t = 0.0-) 
 
C     Compare swing voltage with power flow voltage (node 2 only) 
C     at initial network solution 
 
  170 a1 = abs(esoln-eyr(2)) + abs(fsoln-eyi(2)) 
      if (a1 .ge. 0.002) then 
        write (errbuf(1), 10080) 
10080   format ( 
     &   '0Unsuccessful initialization - Network voltages do not match P 
     &owerflow voltages' 
     &   ) 
        call prterr('E', 1) 
        write (outbuf, 10090) esoln, fsoln, eyr(1), eyi(1) 
10090   format (1x, 2f12.6, 'calc v', 2f12.6, 'soln v') 
        call prtout(1) 
        link = 13 
        call xtime(-4) 
        call erexit() 
        goto 190 
      else 
        write (outbuf, 10100) nmx, mytr 
10100   format ( 
     &   '0Successful initialization - commence with swing .... ', i4,  
     &   ' buses', 5x, i6, ' off-diag') 
        call prtout(1) 
 
C       Set TX, LD saturation SW = 1.0 
 
        satsw = 1.0 
        if (iter .gt. 2) then 
          write (errbuf(1), 10110) 
10110     format ('0 Error in initial network solution: zero minus ',  
     &     'network solution is taking more than two iterations.') 
          write (errbuf(2), 10120) 
10120     format ('  This may be an error condition.') 
          call prterr('W', 2) 
        endif 
      endif 
 
C     Call GENUPD to update generator power and terminal voltage 
C     following a network iteration 
 
  180 call genupd() 
      call xtime(-4) 
  190 return 
      end 
