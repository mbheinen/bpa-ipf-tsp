C    @(#)ycpsum.f	20.4 1/4/99
        subroutine ycpsum
C
C       summarize the status of the adjustable
C       var-compensation schemes.
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/ycomp.inc'
 
        character id * 1, nxid * 1, lntype(9)
        complex*16 y11tdc, y12tdc, v1tdc, v2tdc, v(2), y(2,2), a(2), 
     &             s(2)
        real*8 psec(0:10), qsec(0:10), isect(0:10), irate(0:10), 
     &         vsect(0:10)
c
        real ibase, iovld
c
        integer pctmax, pctmin, pctact, secnum(0:10),
     1          sectyp(0:10)
        data lntype / '*','M','L','R','T','P','D','E','V'/
C
        if (nycomp.eq.0) go to 900
 
        call forbtm
        write (outbuf,110)
  110   format (t53,' Summary of Series/Shunt Adjustable ',
     &          'Compensated Lines')
        call shdlod(1)
        write (outbuf,120)
  120   format('0Compensated line ',
     1         t40,'---- Adjustable Xij ----',  
     2         t66,'---- Adjustable Bis ----',  
     3         t92,' % Comp ',  
     4         t100,'--------- Pij ---------')  
        call shdlod(2)  
        write (outbuf,130)  
  130   format(t40,'   min   actual      max',  
     1         t66,'   min   actual      max ', 
     2         t100,' min    acutal     max ')  
        call shdlod(3)  
        write (outbuf,140)  
  140   format(t40,' (p.u.)  (p.u.)   (p.u.)',  
     1         t66,' (p.u.)  (p.u.)   (p.u.)',  
     2         t100,' (Mw)     (Mw)     (Mw)')  
        call shdlod(4)  
        outbuf= ' ' 
        call shdlod(5)  
        call fortop 
        
        do 200 jt = 1,nycomp
           k1 = kycomp(1,jt)
           kt = inp2opt(k1)
           k2 = kycomp(2,jt)
           mt = inp2opt(k2)
           id = char (kycomp(3,jt))
           ksect = kycomp(4,jt)
           xij = ycomp(32,jt)
           xijmin = ycomp(34,jt)
           xijmax = ycomp(35,jt)
           bis = ycomp(36,jt)
           bismin = ycomp(38,jt)
           bismax = ycomp(39,jt)
C
C       Compute Pij
C
           y(1,2) = cmplx (-1.0,0.0)/cmplx(0.0,xij)
           y(2,1) = y(1,2)
           y(1,1) = cmplx (0.0,bis) - y(1,2)
           y(2,2) = -y(2,1)
 
           i = numbrn (k1,k2,id,0)
           call pieqiv (i,y,ierr)
           v(1) = dcmplx (e(kt),f(kt))
           v(2) = dcmplx (e(mt),f(mt))
           y11tdc = dcmplx(dreal( y(1,1)),dimag(y(1,1)))
           y12tdc = dcmplx(dreal(y(1,2)),dimag(y(1,2)))
           v1tdc = dcmplx(dreal(v(1)),dimag(v(1)))
           v2tdc = dcmplx(dreal(v(2)),dimag(v(2)))
           a(1) = y11tdc * v1tdc  +  y12tdc * v2tdc
C **********************************************************
           s(1) = v(1) * dconjg (a(1))
           pij = dreal (s(1)) * bmva
           qij = dimag (s(1)) * bmva
           ibase = 1000.0 * bmva / (sqrt(3.0) * base(k1))
           nsect = 0
           psec(nsect) = pij
           qsec(nsect) = qij
           vsect(nsect) = cdabs (v(1))
           isect(nsect) = cdabs (a(1)) * ibase
           irate(nsect) = 0.0
           secnum(nsect) = 0
           sectyp(nsect) = 0
C
C       Compute % compensation, section quantities
C
           xtotc = 0.0
           xtotr = 0.0
 
           do 150 i = kbsdta(16,k1),kbsdta(16,k1+1)-1
           if (kbrnch(12,i) .eq. k2) then
 
           if (kbrnch(1,i) .eq. 1 .or. kbrnch(1,i) .eq. 4 .or.
     1         kbrnch(1,i) .eq. 9) go to 150
           call getchr(1,nxid,kbrnch(13,i)) 
           if (id .eq. nxid) then   
              call pieqiv (i,y,ierr)
              call abcd (v,a,y) 
              v(1) = v(2)   
              a(1) = -a(2)  
              s(1) = v(1) * dconjg (a(1))
              nsect = nsect + 1 
              psec(nsect) = dreal (s(1)) * bmva  
              qsec(nsect) = dimag (s(1)) * bmva 
              vsect(nsect) = cdabs (v(1))
              isect(nsect) = ibase * cdabs (a(1))
              irate(nsect) = brnch(4,i) 
              secnum(nsect) = kbrnch(14,i)  
              sectyp(nsect) = kbrnch(1,i)   
        
              if (kbrnch(14,i) .ne. ksect) then 
                 x = dreal (dcmplx(-1.0,0.0)/y(1,2))  
                 if (x .gt. 0.0) then   
                    xtotr = xtotr + x   
                 else   
                    xtotc = xtotc + x   
                 endif  
              endif 
              if (kbrnch(1,i) .eq. 5) then  
                 ibase = 1000.0 * bmva / (sqrt(3.0) * base(k2)) 
              endif 
           endif
        else if (kbrnch(12,i) .gt. k2) then 
           go to 160
        endif   
  150   continue
  160   continue
        
        if (xtotr + amax1(xijmin,0.0) .gt. 0.0) then
           pctmin = 100.0 * abs(xtotc + amin1 (xijmin,0.0)) /   
     1                         (xtotr + amax1 (xijmin,0.0)) 
        else
           pctmin = 0.0 
        endif   
        if (xtotr + amax1(xij,0.0) .gt. 0.0) then   
           pctact = 100.0 * abs(xtotc + amin1 (xij,0.0)) /  
     1                         (xtotr + amax1 (xij,0.0))
        else
           pctact = 0.0 
        endif   
        if (xtotr + amax1(xijmax,0.0) .gt. 0.0) then
           pctmax = 100.0 * abs(xtotc + amin1 (xijmax,0.0)) /   
     1                         (xtotr + amax1 (xijmax,0.0)) 
        else
           pctmax = 0.0 
        endif   
        
        pmin = ycomp(5,jt) * bmva   
        pmax = ycomp(6,jt) * bmva   
        
        write (outbuf,170) bus(k1),base(k1),bus(k2),base(k2),   
     1     id,ksect,xijmin,xij,xijmax,bismin,bis,bismax,
     2     pctact,pmin,pij,pmax
  170   format ('0',a8,f6.1,2x,a8,f6.1,1x,a1,1x,i1,t40,3f8.5,
     1          t66,3f8.5,t92,i6,t98,3(f7.1,2x))
        call prtout (1)
C
C       List section quantitites
C       
        call space (1)  
        do 190 i = 0,nsect  
        if (i .eq. 0) then  
           write (outbuf,172) bus(k1), base(k1), id, 0, 
     1        psec(i), qsec(i), isect(i), vsect(i)  
  172      format (t18,a8,f6.1,t34,a1,1x,i1,
     1             t40,2f8.1,' MVA in ',
     2             t66,f8.1,' amps ',t82,7x,' rating ', 
     3             t98,f5.3,' volts ')  
           dv = dim (sngl(vsect(i)),vlimx(kt))
     &        - dim(vlimn(kt),sngl(vsect(i)))
        else if (i .lt. nsect) then 
           write (outbuf,174) lntype(sectyp(i)), id, secnum(i), 
     1        psec(i), qsec(i), isect(i), irate(i), vsect(i)
  174      format (t16,a1,t34,a1,1x,i1,t40,2f8.1,   
     1             t66,f8.1,t82,f7.0,   
     2             t98,f5.3)
           dv = dim (sngl(vsect(i)),vlimx(kt)) 
     &        - dim(vlimn(kt),sngl(vsect(i)))
        else
           write (outbuf,176) lntype(sectyp(i)), bus(k2), base(k2), id, 
     1        secnum(i), psec(i), qsec(i), isect(i), irate(i), vsect(i)  
  176      format (t16,a1,t18,a8,f6.1,t34,a1,1x,i1, 
     2             t40,2f8.1,   
     3             t66,f8.1,t82,f7.0,   
     4             t98,f5.3)
           dv = dim (sngl(vsect(i)),vlimx(mt)) 
     &        - dim(vlimn(mt),sngl(vsect(i)))
        endif   
        
        if (i .eq. 0) then  
           iovld = 0.0  
        else if (lntype(sectyp(i)) .ne. 'T') then   
           if (irate(i) .gt. 0.0) then  
              iovld = dim (isect(i),irate(i))   
           else 
              iovld = 0.0   
           endif
        else
           if (irate(i) .gt. 0.0) then  
              iovld = dim(dsqrt(psec(i)**2 + qsec(i)**2),irate(i))   
           else 
              iovld = 0.0   
           endif
        endif   
        if (abs(iovld) .gt. 0.001) then 
           write (outbuf(112:),178) iovld   
  178      format ('Overload    =',f7.1)
           call prtout (1)  
           outbuf = ' ' 
        endif   
        
        if (abs(dv) .gt. 0.001) then
           write (outbuf(112:),180) dv  
  180      format ('V violation =',f7.3)
           call prtout (1)  
           outbuf = ' ' 
        endif   
        
        dp = dim (sngl(psec(i)),pmax) - dim (pmin,sngl(psec(i)))
        if (abs(dp) .gt. 0.1) then  
           write (outbuf(112:),182) dp  
  182      format ('P violation =',f7.1)
           call prtout (1)  
           outbuf = ' ' 
        endif   
        
        if (outbuf .ne. ' ') call prtout (1)
        
  190   continue
        
  200   continue
        
        outbuf = ' '
        call rpnlod 
        call shdlod (1) 
        call shdlod (2) 
        call shdlod (3) 
        call shdlod (4) 
        call shdlod (5) 
        
        outbuf = '0End of Series/Shunt Adjustable Compensated Lines Summ
     1ary'  
        call prtout(1)  
        call forbtm 
        
  900   continue
        
        return  
        end 
