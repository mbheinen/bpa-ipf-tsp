C    @(#)xbusum.f	20.5 7/24/96
      subroutine xbusum
c                                       
c     write an analysis report titled 'Summary of Type BX Buses'
c                                       
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tbxsrt.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/ordsta.inc'
 
      common /xinit/ xinit(MAXXDT)
      double precision xinit
c
        dimension ustep(8), umvar(8), sstep(8), smvar(8)
        real match, nup, ndown  
        integer flag, flgmax, sen, senmax
c
        diftol (a,b) = abs ((a - b) / b)
c
c       Set up pointers to X data (busxdtptr) and TBX data (ptrtbx)
c
        if (.not. xdt_flag) then
           do i = 1, ntot
              busxdtptr(i)  = 0
           enddo
           do i = 1, kxtot
              kxd = xdata(1,i)
              if (kxd .gt. 0) then
                 if (ordtbx .eq. 2) kxd = opt2inp(kxd)
                 busxdtptr(kxd) = i
              endif
           enddo
           xdt_flag = .true.
        endif
                                                                    
        if (tbx_loaded .ne. ordtbx) then
           do i = 1, ntot
              ptrtbx(i)  = 0
           enddo

           do i = 1, ntotb
              kxd = tbx(2,i)
              if (kxd .gt. 0) then
                 if (ordtbx .eq. 2) kxd = opt2inp(kxd)
                 ptrtbx(kxd) = i
              endif
              if (kxd .gt. 0) ptrtbx(kxd) = i
           enddo
           tbx_loaded = ordtbx
        endif
c
C       KXTOT = number of BX buses. If no BX buses exist,
C       jump to bottom of subroutine.

        if (kxtot .eq. 0) go to 900 

C       Print tailer, go to next page and print header.

        call forbtm 
        write (outbuf,110)  
  110   format (t53,' Summary of Type BX Buses ')   
        call shdlod(1)  
        write (outbuf,120)  
  120   format('0BX bus ',t18,'/ Controlled bus /', 
     1         t38,'/------- Shunt Susceptance --------/',  
     2         t76,'/--- V-sensitivity ---/',   
     3         t101,'Comments') 
        call shdlod(2)  
        write (outbuf,130)  
  130   format(t18,'/ Voltage limits /',
     1         t38,'/---  scheduled ---/   /-- used  --/',  
     2         t76,'d(step)  MVAR   d(volt)')   
        call shdlod(3)  
        write (outbuf,140)  
  140   format(t18,'min   max  actual/',
     1         t38,'steps MVAR @  MVAR @   steps MVAR @')   
        call shdlod(4)  
        write (outbuf,142)  
  142   format(t38,'    nominal V case V         case V')   
        call shdlod(5)  
        call fortop 

C       Process BX bus information.   

        do 620 jt = 1,kxtot 


C       K = external(alphabetical) bus number
C       KT = internal(optimal) bus number
C       M = external(alphabetical) remote controlled bus number  
C       MT = internal(optimal) remote controlled bus number  

        k = xdata(1,jt)
        kt = inp2opt(k)   

        ntbx = ptrtbx(k)
        if (ntbx .eq. 0) go to 620
        ltyp = tbx(1,ntbx)
        if (ltyp .ne. 5) go to 620

        m = xdata(2,jt)
        if (m .eq. 0) m = k 
        mt = inp2opt(m)   
        if (ltbxsl(1,ntbx) .gt. 0) then   
           dvdq = tbxslp(5,ntbx)
        else
           dvdq = xsen(jt)  
        endif   
C       
C       TOTREK = total reactors availble (MVARS).   
C       TOTCAP = total capacitors availble (MVARS). 
C       USEREK = amount of reactors used (MVARS).   
C       USECAP = amount of capacitors used (MVARS). 
C       
        vksq = e(kt)**2 + f(kt)**2  
        totrek = xdata(3,jt) * vksq 
        totcap = xdata(4,jt) * vksq 
        userek = xdata(5,jt) * vksq 
        usecap = xdata(6,jt) * vksq 
C       
C       ISW = 1, if reactors are used, or if no capacitors are  
C       available and no reactors are used. 
C       ISW = 2, if capacitors are used, or if no reactors are  
C       available and no capacitors are used.   
C       ISW = 0, if no capacitors or reactors are used. 
C       
        if ((userek .lt. 0.0 .and. totrek .le. userek) .or. 
     1      (totrek .lt. 0.0 .and. totcap .eq. 0.0)) then   
           isw = 1  
        else if ((usecap .gt. 0.0 .and. totcap .ge. usecap) .or.
     1      (totrek .eq. 0.0 .and. totcap .gt. 0.0)) then   
           isw = 2  
        else
           isw = 0  
        endif   
C       
C       Find index of first capacitor (ICAP), index of first reactor
C       (IREK), and total number of steps of capacitors and reactors
C       available (IS). 
C       
        icap = 0
        irek = 0
        do 150 i = 1,8  
          j = 2*i + 5   
          n = xdata(j,jt)   
          if (n .eq. 0) go to 160   
          if ((xdata(j+1,jt) .gt. 0.0) .and. (icap .eq. 0)) icap = i
          if ((xdata(j+1,jt) .lt. 0.0) .and. (irek .eq. 0)) irek = i
  150   continue
        i = 9   
  160   is = i - 1  
        if (icap .eq. 0) icap = is + 1  
        if (irek .eq. 0) irek = is + 1  
C       
C       Find Q-used and Q-scheduled.
C       
        match = 0.0 
        q2 = 0.0
        do 180 i = 1,is 
          j = 2*i + 5   
          n = xdata(j,jt)   
          qstep = xdata(j+1,jt) * vksq  
          ustep(i) = 0.0
          umvar(i) = 0.0
          sstep(i) = n  
          smvar(i) = n * qstep  
C       
C         If Reactors are used .AND. QSTEP contains Capacitors, jump
C         to bottom of this loop and process the next Reactor/  
C         Capacitor step.   
C       
          if ((isw .eq. 1) .and. (qstep .gt. 0.0)) go to 180
C       
C         If Capacitors are used .AND. QSTEP contains Reactors, jump
C         to bottom of this loop and process the next Reactor/  
C         Capacitor step.   
C       
          if ((isw .eq. 2) .and. (qstep .lt. 0.0)) go to 180
C       
C         If no capacitors or reactors are used, jump to bottom of  
C         this loop and process the next Reactor/Capacitor step.
C       
          if (isw .eq. 0) go to 180 
C       
C         If capacitors are used in the solution and this QSTEP 
C         contains capacitors, or if reactors are used and this QSTEP
C         contains reactors, determine number of steps and amount of
C         Q used.   
C       
          do 170 l = 1,n
C       
C           Q1 = Previous value of Q
C           Q2 = Current value of Q 
C       
            q1 = q2 
            q2 = q2 + qstep 
C       
C           If capacitors are used. 
C       
            if (isw .eq. 2) then
C       
C             USTEP = Number of steps of capacitors used
C             UMVAR = Amount of capacitors used from current step   
C       
              if (usecap .gt. q1 .and. usecap .ge. q2) then 
                ustep(i) = ustep(i) + 1.0   
                umvar(i) = umvar(i) + qstep 
                match = float(10*i + l) 
              else if (usecap .ge. q1 .and. usecap .lt. q2) then
                ratio = (usecap - q1) / qstep + 0.001   
                ustep(i) = ustep(i) + ratio 
                umvar(i) = umvar(i) + ratio * qstep 
                match = float(10*i + l - 1) + ratio 
                go to 180   
              else  
                go to 180   
              endif 
C       
C           If reactors are used.   
C       
            else
C       
C             USTEP = Number of steps of reactors used  
C             UMVAR = Amount of reactors used from current step 
C       
              if (userek .lt. q1 .and. userek .le. q2) then 
                ustep(i) = ustep(i) + 1.0   
                umvar(i) = umvar(i) + qstep 
                match = float(10*i + l) 
              else if (userek .le. q1 .and. userek .gt. q2) then
                ratio = (userek - q1) / qstep + 0.001   
                ustep(i) = ustep(i) + ratio 
                umvar(i) = umvar(i) + ratio * qstep 
                match = float(10*i + l - 1) + ratio 
                go to 180   
              else  
                go to 180   
              endif 
            endif   
  170     continue  
  180   continue
C       
C       Find permissible delta Q above and below quiescent point
C       
C       NUP is the number direction of unit QUP to increase 
C           reactive allocation.
C       
C           +1.0 denotes adding one step of QCAP.   
C           -1.0 denodes removing one step of QREAK.
C       
C       NDOWN is the number direction of unit QDOWN to decrease 
C           reacitive allocation.   
C       
C           -1.0 denotes removing one step of QCAP. 
C           +1.0 denodes adding one step of QREAK.  
C       
C       I = Main Step number
C       J = Substep number of main Step 
C       
        i = match / 10.0
        j = amod (match,10.0)   
C       
C       Determine reactor step up and down  
C       
        if (isw .eq. 1) then
           if (j .ge. 1 ) then  
              qup = smvar(i)/sstep(i)   
              nup = -1.0
           else 
              if (i-1 .ge. irek) then   
                 qup = smvar(i-1)/sstep(i-1)
                 nup = -1.0 
              else if (icap .lt. is) then   
                 qup = smvar(icap)/sstep(icap)  
                 nup = +1.0 
              else  
                 qup = 0.0  
                 nup = 0.0  
              endif 
           endif
           if (j .lt. sstep(i)) then
              qdown = smvar(i)/sstep(i) 
              ndown = +1.0  
           else 
              if (i .lt. is) then   
                 if (smvar(i+1) .lt. 0.0) then  
                    qdown = smvar(i+1)/sstep(i+1)   
                    ndown = +1.0
                 else   
                    qdown = 0.0 
                    ndown = 0.0 
                 endif  
              else  
                 qdown = 0.0
                 ndown = 0.0
              endif 
           endif
C       
C       Determine capacitor step up and down
C       
        else if (isw .eq. 2) then   
           if (j .ge. 1 ) then  
              qdown = smvar(i)/sstep(i) 
              ndown = -1.0  
           else 
              if (i-1 .ge. icap) then   
                 qdown = smvar(i-1)/sstep(i-1)  
                 ndown = -1.0   
              else if (irek .lt. is) then   
                 qdown = smvar(irek)/sstep(irek)
                 ndown = +1.0   
              else  
                 qdown = 0.0
                 ndown = 0.0
              endif 
           endif
           if (j .lt. sstep(i)) then
              qup = smvar(i)/sstep(i)   
              nup = +1.0
           else 
              if (i .lt. is) then   
                 if (smvar(i+1) .gt. 0.0) then  
                    qup = smvar(i+1)/sstep(i+1) 
                    nup = +1.0  
                 else   
                    qup = 0.0   
                    nup = 0.0   
                 endif  
              else  
                 qup = 0.0  
                 nup = 0.0  
              endif 
           endif
C       
C       If no reactors or capacitors are used, determine NUP, NDOWN,
C       QUP, and QDOWN, for V sensitivity purposes. 
C       
        else if (isw .eq. 0) then   
           if (irek .le. is) then   
              qdown = smvar(irek)/sstep(irek)   
              ndown = 1.0   
           else 
              qdown = 0.0   
              ndown = 0.0   
           endif
           if (icap .le. is) then   
              qup = smvar(icap)/sstep(icap) 
              nup = 1.0 
           else 
              qup = 0.0 
              nup = 0.0 
           endif
        endif   
C       
C       Print out results:  
C       
C       FLGMAX is the maximum number of diagnostic flags possible;  
C       SENMAX is the maximum number of +/- sensitivities possible; 
C       IS is the maximum number of BX entitites.   
C       
        flag = 0
        flgmax = 6  
        sen = 0 
        senmax = 2  
        
        vk = dsqrt(e(kt)**2 + f(kt)**2)  
        dvk = dim(vk,vlimx(kt)) - dim(vlimn(kt),vk)
        vm = dsqrt(e(mt)**2 + f(mt)**2)  
        dvm = dim(vm,vlimx(mt)) - dim(vlimn(mt),vm)
        do 610 i = 1, max0(flgmax,senmax,is+2)  
        if (i .eq. 1) then  
           qu = 0.0 
           if (ustep(i) .gt. 0.01) qu = umvar(i) / ustep(i) 
           qs = 0.0 
           if (sstep(i) .gt. 0) qs = smvar(i) / sstep(i)
           qsnom = qs / vksq
           write (outbuf,190) bus(k),base(k),vlimn(mt),   !uur
     1          vlimx(mt),vm,nint(sstep(i)),qsnom,qs,nint(ustep(i)),qu!u
  190      format ('0',a8,f6.1,t17,3f6.3,t38,i3,1x,2f8.1,3x,i3,1x,f8.1)
        else if (i .le. is) then
           qu = 0.0 
           if (ustep(i) .gt. 0.01) qu = umvar(i) / ustep(i) 
           qs = 0.0 
           if (sstep(i) .gt. 0) qs = smvar(i) / sstep(i)
           qsnom = qs / vksq
           write (outbuf,200) nint(sstep(i)),qsnom,qs,nint(ustep(i)),qu
  200      format (t38,i3,1x,2f8.1,3x,i3,1x,f8.1)   
        else
           if ((i .eq. is+1) .and. (totrek .lt. 0.0)) then  
              sumtot = 0.0  
              sumuse = 0.0  
              do 201 j = 1,is   
              sumtot = sumtot + amin1 (0.0,smvar(j))
              sumuse = sumuse + amin1 (0.0,umvar(j))
  201         continue  
              sumnom = sumtot / vksq
              write (outbuf,202) sumnom,sumtot,sumuse   
  202         format (t18,'   Total Reactors',t38,4x,2f8.1,7x,f8.1) 
           else if ((i .eq. is+1) .and. (totcap .gt. 0.0)) then 
              sumtot = 0.0  
              sumuse = 0.0  
              do 203 j = 1,is   
              sumtot = sumtot + amax1 (0.0,smvar(j))
              sumuse = sumuse + amax1 (0.0,umvar(j))
  203         continue  
              sumnom = sumtot / vksq
              write (outbuf,204) sumnom,sumtot,sumuse   
  204         format (t18,'   Total Capacitors',t38,4x,2f8.1,7x,f8.1)
           else if ((i .eq. is+2) .and. 
     1          (totcap .gt. 0.0) .and. (totrek .lt. 0.0)) then 
              sumtot = 0.0  
              sumuse = 0.0  
              do 205 j = 1,is   
              sumtot = sumtot + amax1 (0.0,smvar(j))
              sumuse = sumuse + amax1 (0.0,umvar(j))
  205         continue  
              sumnom = sumtot / vksq
              write (outbuf,206) sumnom,sumtot,sumuse   
  206         format (t18,'         Capacitors',t38,4x,2f8.1,7x,f8.1)
           else 
              outbuf = ' '  
           endif
        endif   
        if (sen .eq. 0) then
           sen = 1  
           if (qdown .eq. 0.0 .and. qup .ne. 0.0) then  
              dv = nup * dvdq * qup / bmva  
              write (outbuf(76:),210) nint(nup),qup,dv  
           else if (qdown .ne. 0.0) then
              dv = ndown * dvdq * qdown / bmva  
              write (outbuf(76:),210) nint(ndown),qdown,dv  
  210         format (i3,1x,f9.1,f9.3)  
           endif
        else if (sen .eq. 1) then   
           sen = 2  
           if (qdown .eq. 0.0 .and. qup .ne. 0.0) then  
           else if (qup .ne. 0.0) then  
              dv = nup * dvdq * qup / bmva  
              write (outbuf(76:),210) nint(nup),qup,dv  
           endif
        endif   
        if (flag .eq. 0) then   
           flag = 1 
           if (abs(dvm) .gt. 0.001) then
              write (outbuf(101:),220) dvm  
  220         format ('V violation  =',f7.3)
              go to 600 
           endif
        endif   
        if (flag .eq. 1)  then  
           flag = 2 
           if (dvm .lt. 0.0) then   
              if (nup * qup * dvdq .gt. 0.0) then   
                 ts = dvm / (dvdq * nup * qup) * bmva   
                 write (outbuf(101:),230) ts
  230            format ('X resolution =',f7.3,' steps')
                 go to 600  
              else if (ndown * qdown * dvdq .gt. 0.0) then  
                 ts = dvm / (dvdq * ndown * qdown) * bmva   
                 write (outbuf(101:),230) ts
                 go to 600  
              endif 
           else if (dvm .gt. 0.0) then  
              if (nup * qup * dvdq .lt. 0.0) then   
                 ts = dvm / (dvdq * nup * qup) * bmva   
                 write (outbuf(101:),230) ts
                 go to 600  
              else if (ndown * qdown * dvdq .lt. 0.0) then  
                 ts = dvm / (dvdq * ndown * qdown) * bmva   
                 write (outbuf(101:),230) ts
                 go to 600  
              endif 
           endif
        endif   
        if (flag .eq. 2)  then  
           flag = 3 
           if (isw .eq. 1) then 
              if (diftol (userek,totrek) .lt. 0.01) then
                 outbuf(101:) = 'Qmin limit reached'
                 go to 600  
              else if (userek .eq. 0.0 .and. totcap .eq. 0.0) then  
                 outbuf(101:) = 'Qmax limit reached'
                 go to 600  
              endif 
           else if (isw .eq. 2) then
              if (diftol (usecap,totcap) .lt. 0.01) then
                 outbuf(101:) = 'Qmax limit reached'
                 go to 600  
              else if (usecap .eq. 0.0 .and. totrek .eq. 0.0) then  
                 outbuf(101:) = 'Qmin limit reached'
                 go to 600  
              endif 
           endif
        endif   
        if (flag .eq. 3) then   
           flag = 4 
           if (kt .ne. mt) then 
              write (outbuf(101:),240) bus(m),base(m)   
  240         format ('Remote bus ',a8,f6.1)
              go to 600 
           endif
        endif   
        if (flag .eq. 4) then   
           flag = 5 
           if (kt .ne. mt) then 
              if (dvk .gt. 0.0) then
                 write (outbuf(101:),250) dvk   
  250            format ('Control V-Violation =',f6.3)  
                 go to 600  
              else if (dim (1.001*vk,vlimx(kt)) .gt. 0.0) then!uur
                 outbuf(101:) = 'Control Vmax limit reached'
                 go to 600  
              else if (dvk .lt. 0.0) then   
                 write (outbuf(101:),250) dvk   
                 go to 600  
              else if (dim (vlimn(kt),0.999*vk) .gt. 0.0) then!uur
                 outbuf(101:) = 'Control Vmin limit reached'
                 go to 600  
              endif 
           endif
        endif   
        if (flag .eq. 5) then   
           flag = 6 
           if (kspare(24) .eq. 1) then  
              dx = (userek + usecap) / vksq - sngl(xinit(jt)) 
              if (abs (dx) .gt. 1.0e-3) then
                 write (outbuf(101:),260) dx
  260            format ('Nominal excursion =',f8.1)
                 go to 600  
              endif 
           endif
        endif   
  600   if (outbuf .eq. ' ') go to 620  
        call prtout (1) 
  610   continue
        
  620   continue
        
        outbuf = '0End of BX Summary '  
        call prtout(1)  
        
        outbuf = ' '
        call rpnlod 
        call shdlod (1) 
        call shdlod (2) 
        call shdlod (3) 
        call shdlod (4) 
        call shdlod (5) 
        call forbtm 
  900   continue
        return  
        end 
