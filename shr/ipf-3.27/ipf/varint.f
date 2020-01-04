C    @(#)varint.f	20.13 8/19/99
        subroutine varint   ! initializes the %var control buses.   
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/komps.inc'
        include 'ipfinc/lndpcp.inc'
        include 'ipfinc/pctvr2.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/svc.inc'
        include 'ipfinc/tbx.inc'
 
        common /is_batch / is_batch

        dimension pgen(25), qgen(25), percnt(25)
        character commnt*39
        external kmpsrt, swpsrt, kmpvar, swpvar
        integer ptr, count
C
C       Build percentage var control array PCTVR
C
        numpctvr = 0
        if (ntotb .eq. 0) go to 1880
        jpctvr = 0  
        kerrsw = 0  
        do 1600 jt = 1,ntotb
        ltyp = tbx(1,jt)   
        if (ltyp.ne.3) go to 1600   
        kt = tbx(2,jt) 

C       Exclude Line Drop Compensators.   

        do j = 1, numldc
          nb = lndpcp(1,j)
          if (inp2opt(nb) .eq. kt) go to 1600   
        enddo

C       Exclude SVC Compensators. 

        do j = 1, numsvc
          nb = svc(1,j)   
          if (inp2opt(nb) .eq. kt) go to 1600
        enddo
        mt = tbx(8,jt) 
        mt = iabs(mt)   
        if (mt .gt. 0) then
          jpctvr = jpctvr + 1 
          ksort(1,jpctvr) = mt
          ksort(2,jpctvr) = kt
          ksort(3,jpctvr) = jt
        endif
 1600   continue
        if (jpctvr .le. 1) go to 1880   
        call qiksrt(1,jpctvr,kmpsrt,swpsrt) 

C       %var generators are sorted consecutively in NBSORT.  

        jj = jpctvr - 1 
        i = 1
        do while (i .le. jj)
          i1 = ksort(1,i) 
          i2 = ksort(2,i) 
          i3 = ksort(3,i) 
          ii = i + 1  
          do j = ii,jpctvr   
            j1 = ksort(1,j) 
            j2 = ksort(2,j) 
            j3 = ksort(3,j) 
            if (j1 .ne. i1) then
              i = j - 1   
              go to 1610  
            else if (j .le. ii) then
              npctvr = npctvr + 1 
              if (npctvr .gt. MAXPVC) then
                write (errbuf(1),1630) MAXPVC
 1630           format('0 More than ',i4,
     &               ' percentage-var controlled buses.')
                if (is_batch .eq. 0) then
                  call prterx ('E',1)
                else
                  call prterx ('F',1)
                endif
                npctvr = 0   
                go to 1880   
              endif   
              numpctvr = numpctvr + 1
              if (numpctvr .gt. 20*MAXPVC) then
                write (errbuf(1),1632) 20*MAXPVC
 1632           format('0 More than ',i4,
     &               ' percentage-var controlling buses.')
                if (is_batch .eq. 0) then
                  call prterx ('E',1)
                else
                  call prterx ('F',1)
                endif
                npctvr = 0   
                go to 1880   
              endif   
              kpctvr(1,npctvr) = i1   
              kpctvr(2,npctvr) = numpctvr
              kpctvr(3,npctvr) = 0
              ptr = numpctvr
              pctvr(1,ptr) = i2   
              pctvr(2,ptr) = i3   
              do k = 3, 8
                pctvr(k,ptr) = 0
              enddo
              count = 1
            endif
            count = count + 1
            if (count .gt. 25) then   
              kerrsw = 1
              kt = kpctvr(1,npctvr)
              ptr = kpctvr(2,npctvr)
              k1 = pctvr(1,ptr)
              write (errbuf(1),1670) intbus(kt),intbas(kt),intbus(k1),
     &          intbas(k1)
 1670         format('Bus ', a8, f6.1,
     &          ' is controlled by more than 25 generators : ',a8,f6.1)
              nerr = 1
              ptr = pctvr(8,ptr)
              do while (ptr .gt. 0)
                last = lastch (errbuf(nerr))
                if (last .ge. 120-15) then
                  nerr = nerr + 1
                  errbuf(nerr) = ' '
                  last = 15
                endif
                k1 = pctvr(1,ptr)
                write (errbuf(nerr)(last+1:),1680) intbus(k1),intbas(k1)
 1680           format(a8,f6.1)
                ptr = pctvr(8,ptr)
              enddo
              if (is_batch .eq. 0) then
                call prterx ('E',nerr)
              else
                call prterx ('F',nerr)
              endif
              go to 1610
            else
              numpctvr = numpctvr + 1
              if (numpctvr .gt. 20*MAXPVC) then
                write (errbuf(1),1632) 20*MAXPVC
                if (is_batch .eq. 0) then
                  call prterx ('E',1)
                else
                  call prterx ('F',1)
                endif
                npctvr = 0   
                go to 1880   
              endif   
              pctvr(8,ptr) = numpctvr
              ptr = numpctvr
              pctvr(1,ptr) = j2 
              pctvr(2,ptr) = j3   
              do k = 3, 8
                pctvr(k,ptr) = 0
              enddo
            endif
          enddo
          i = jpctvr - 1  
 1610     i = i + 1   
        enddo

 1710   if (npctvr.eq.0) go to 1880 
        call qiksrt (1,npctvr,kmpvar,swpvar)

C       Assemble data from TBX array  

        call forbtm 
        outbuf = ' Initialization of %Var Controlled Buses '
        call rpnlod 
        write (outbuf,1720) 
 1720   format(t2,'Controlled bus ',t18,'Generators',t34,   
     &    '- Percentage -',t50,'--- Active power ---',t72,  
     &    '-- Reactive power --',t94,'Comments')
        call shdlod(1)  
        write (outbuf,1730) 
 1730   format(t34,'Scheduled Used',
     &         t50,'  (MW)  (% of Total)',  
     &         t72,' (MVAR) (% of Total)')  
        call shdlod(2)  
        outbuf= ' ' 
        call shdlod(3)  
        call shdlod(4)  
        call shdlod(5)  
        call fortop 
        do jt = 1,npctvr
          mt = kpctvr(1,jt)
          pcntot = 0.0
          ptot = 0.0  
          qtot = 0.0  
          ptr = kpctvr(2,jt)
          count = 0
          do while (ptr .gt. 0)
            kt = pctvr(1,ptr)
            jtbx = pctvr(2,ptr)
            count = count + 1
            percnt(count) = 0.01*tbx(5,jtbx)  
            if (percnt(count) .gt. 0.0 .and. percnt(count) .le. 1.0) 
     &         then   
               pcntot = pcntot + percnt(count)  
            else
               pcntot = pcntot - 1.0e10 
            endif   
            pgen(count) = bmva*dmax1(0.0d0, pnetu(kt)+ploadu(kt))   
            ptot = ptot + pgen(count)   
            qgen(count) = bmva*dmax1(tbx(3,jtbx),0.0d0) 
            qtot = qtot + qgen(count)   
            pctvr(5,ptr) = 0.0  
            pctvr(6,ptr) = tbx(3,jtbx)
            pctvr(7,ptr) = tbx(4,jtbx)
            ptr = pctvr(8,ptr)
          enddo
          kpctvr(3,jt) = count

          ptr = kpctvr(2,jt)
          pcntot2 = 0.0
          count = 0
          do while (ptr .gt. 0)
            kt = pctvr(1,ptr)
            count = count + 1
            pctx = 0.0  
            if (ptot .gt. 0.0) then 
              pct1 = pgen(count)/ptot  
              pctx = pct1  
            else
              pct1 = 0.0   
            endif   
            if (qtot .gt. 0.0) then 
              pct2 = qgen(count)/qtot  
              if (pctx .eq. 0.0) pctx = pct2   
            else
              pct2 = 0.0   
            endif   
            pct3 = 1.0/float(kpctvr(3,jt))
            if (pctx .eq. 0.0 .and. ptot .le. 0.0 .and. qtot .le. 0.0) 
     &        pctx = pct3  
            if (pcntot .le. 0.0) then   
              pct = pctx   
            else if (abs(pcntot - 1.00) .le. 0.01) then 
              pct = percnt(count)  
            else
              pct = percnt(count)/pcntot   
            endif   
            pcntot2 = pcntot2 + pct
            pctvr(5,ptr) = pct  
            ptr = pctvr(8,ptr)
          enddo

          ptr = kpctvr(2,jt)
          count = 0
          do while (ptr .gt. 0)
            kt = pctvr(1,ptr)
            count = count + 1
            commnt = ' '
            pctx = 0.0  
            if (ptot .gt. 0.0) then 
              pct1 = pgen(count)/ptot  
              pctx = pct1  
            else
              pct1 = 0.0   
            endif   
            if (qtot .gt. 0.0) then 
              pct2 = qgen(count)/qtot  
              if (pctx .eq. 0.0) pctx = pct2   
            else
              pct2 = 0.0   
            endif   
            pct3 = 1.0/float(kpctvr(3,jt))
            if (pctx .eq. 0.0 .and. ptot .le. 0.0 .and. qtot .le. 0.0) 
     &        pctx = pct3  
            pct = pctvr(5,ptr) / pcntot2
            pctvr(5,ptr) = pct  
            if (percnt(count) .eq. 0.0 ) then   
              commnt = '% allocated in proportion to P-gen'
            else if (percnt(count) .gt. 0.0 .and. 
     &               percnt(count) .le. 1.0) then  
            else
              commnt = 'Infeasible scheduled percentage (*)'   
            endif   
            if (commnt .eq. ' ') then   
              if (dim(pct,pct1+0.10) - dim(pct1-0.10,pct) .ne. 0.0) 
     &          then   
                commnt = '% used is not proportional to P-gen'
              else if (dim(pct,pct2+0.10) - dim(pct2-0.10,pct) .ne. 0.0)
     &          then  
                commnt = '% used is not proportional to Q-gen'
              endif
            endif   
            if (count .eq. 1) then  
              write (outbuf,1800) intbus(mt),intbas(mt),intbus(kt),
     &          intbas(kt),100.0*percnt(count),100.0*pct,pgen(count),
     &          100.0*pct1,qgen(count),100.0*pct2,commnt  
 1800         format('0',a8,f7.1,t18,a8,f7.1,t34,f5.1,f9.1,t50,f8.1,
     &          f12.1,t72,f8.1,f12.1,t94,a)  
              call prtout(1)   
            else
              write (outbuf,1810) intbus(kt),intbas(kt),
     &          100.0*percnt(count),100.0*pct,pgen(count),100.0*pct1,
     &          qgen(count),100.0*pct2,commnt 
 1810         format(t18,a8,f7.1,t34,f5.1,f9.1,t50,f8.1,f12.1, 
     &          t72,f8.1,f12.1,t94,a)  
              call prtout(1)   
            endif   
            ptr = pctvr(8,ptr)
          enddo
          if (pcntot .le. 0.0) then   
          else if (abs(pcntot - 1.00) .le. 0.01) then 
          else
            write (outbuf,1840) 100.0/pcntot 
 1840       format (t94,'% used is ', f7.1, '% of %scheduled') 
            call prtout (1)  
          endif   
        enddo
        outbuf = ' '
        call rpnlod 
        call shdlod (1) 
        call shdlod (2) 
        call shdlod (3) 
        call shdlod (4) 
        call shdlod (5) 
        outbuf = '0 End of Var Initialization ' 
        call prtout(1)  
        call forbtm 
 1880   continue
        return  
        end 
