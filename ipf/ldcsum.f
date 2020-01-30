C    @(#)ldcsum.f	20.5 11/12/98
        subroutine ldcsum   

C       writes an analysis report to the power flow output (.PFO)
C       file titled 'Summary of Line Drop Compensation'. 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/lndpcp.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tbxsrt.inc'
 
        integer ltbxcp(20)
        real cv(1), ci(1), cz(1)
C
C       NUMLDC = number of buses with Line Drop Compensation. If
C       none exist, jump to bottom of subroutine.
C
        if (numldc .eq. 0) go to 900

C       Print tailer, go to next page and print header. 

        call forbtm 
        write (outbuf,110)  
  110   format (t53,' Summary of Line Drop Compensation Buses ')
        call shdlod(1)
        write (outbuf,120)
  120   format('0Generator',t18,'Remote bus ',
     1         t34,'/----------------------- Voltages and limits ',
     2             '-------------------------/',
     3         t107,'Comments')
        call shdlod(2)
        write (outbuf,130)
  130   format(t34,'/----- Generator -----/--- Controlled voltage ',
     &             '----/---- Remote Bus ---/')
        call shdlod(3)
        write (outbuf,140)
  140   format(t34,'  min    max  actual    min    max  actual    ',
     &             '%     min    max  actual')
        call shdlod(4)
        outbuf = ' '
        call shdlod(5)  
        call fortop 

C       Locate TBX indices. 

        do 200 jt = 1, ntotb
           if (tbx(1,jt) .eq. 3 .and. tbx(2,jt) .eq. tbx(8,jt)) then 
              if (ordtbx .eq. 1) then   
                 nb = tbx(2,jt)
              else  
                 nb = opt2inp(ifix(sngl(tbx(2,jt))))
              endif 
              do 190 i = 1, numldc  
                 if (lndpcp(1,i) .eq. nb) then  
                    ltbxcp(i) = jt  
                    go to 200   
                 endif  
  190         continue  
              write (errbuf(1), 192) bus(nb), base(nb)  
  192         format (' LTB entitity for bus ',a8, f6.1,
     1           ' could not be found.')
              call prterx ('E',1)   
           endif
  200   continue

C       Process Line Drop Compensation.  

        do 620 jt = 1, numldc   

C       K = external(alphabetical) bus number 
C       KT = internal(optimal) bus number 
C       M = external(alphabetical) remote controlled bus number   
C       MT = internal(optimal) remote controlled bus number   

        k = lndpcp(1,jt)
        kt = inp2opt(k)   
        m = lndpcp(2,jt)
        if (m .eq. 0) m = k 
        mt = inp2opt(m)   
        
        vk = dsqrt (e(kt) ** 2 + f(kt) ** 2) 
        if (lndp_type(jt) .eq. 1) then
          vm = dsqrt (e(mt) ** 2 + f(mt) ** 2) 
          vx = (1.0 - drppct(jt)) * vk + drppct(jt) * vm  
          dvx = dim(vx,vmax_ldc(jt)) - dim(vmin_ldc(jt),vx) 
          vxsch = vx - dvx
          if (vk .ne. vm) then
             pctact = (vk - vxsch) / vk
          else
             pctact = 1.0 
          endif   
        else
          cz(1) = xc_ldc(jt)
          ci(1) = qnetu(kt) / vk
          cv(1) = vk - ci(1) * cz(1)
          vm = vk
          vx = cv(1)
          dvx = dim(vx,vmax_ldc(jt)) - dim(vmin_ldc(jt),vx) 
          vxsch = vx - dvx
          if (vk .ne. vx) then
             pctact = (vk - vxsch) / vk
          else
             pctact = 1.0 
          endif   
        endif

C       Print out results:

        write (outbuf,210) bus(k), base(k), bus(m), base(m), vlimn(kt), 
     1     vlimx(kt), vk, vmin_ldc(jt), vmax_ldc(jt), vx, 
     2     100.0 * drppct(jt), vlimn(mt), vlimx(mt), vm
  210   format ('0', a8, f6.1, t18, a8, f6.1, t32, 3f7.3, 1x, 3f7.3, 
     &     f6.1, 2x, 3f7.3)
        if (abs(dvx) .gt. 0.001) then   
           write (outbuf(107:),220) dvx 
  220      format ('V violation =',f7.3)
           call prtout(1)   
           outbuf = ' ' 
        endif   
        jtbx = ltbxcp(jt)   
        if (jtbx .gt. 0) then   
           if (tbx(7,jtbx) .eq. 2) then
              outbuf(107:) = 'Qmin limit reached'   
              call prtout(1)
              outbuf = ' '  
           else if (tbx(7,jtbx) .eq. 3) then   
              outbuf(107:) = 'Qmax limit reached'   
              call prtout(1)
              outbuf = ' '  
           else if (tbx(7,jtbx) .eq. 4) then   
              outbuf(107:) = 'Vmin limit reached'   
              call prtout(1)
              outbuf = ' '  
           else if (tbx(7,jtbx) .eq. 5) then   
              outbuf(107:) = 'Vmax limit reached'   
              call prtout(1)
              outbuf = ' '  
           endif
        endif   
        if (kvolt(mt) .ne. 0) then  
           outbuf(107:) = 'Remote bus is PV'
           call prtout(1)   
           outbuf = ' ' 
        endif   
        if (outbuf .ne. ' ') then   
           call prtout (1)  
        endif   
        
  620   continue
        
        outbuf = '0End of Line Drop Compensation Summary '  
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
