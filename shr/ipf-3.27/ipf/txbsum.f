C    @(#)txbsum.f	20.4 7/18/96
      subroutine txbsum
C
C     summarize the status of all BQ - LTC  control schemes. 
C 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
 
      character type * 2, state * 4
      integer txtype

      if (ntbxtr .eq. 0) go to 170  
      call forbtm   
      write (outbuf,100)
  100 format (t53, ' Summary of LTC Reactive Utilization')  
      call shdlod(1)
      write (outbuf,110)
  110 format (t2, '------------ LTC ------------',  
     1       t32, ' ---- Taps ---   Flow',  
     2       t55,  ' Type     Capacitors     Reactors        Generation
     3       vmin   vmax  vact')
      call shdlod(2)
      write (outbuf,120)
  120 format (t2, '"***" flags controlled bus',
     1       t32, ' Min    Actual  (MVAR)',
     2       t55,   '         Sched   Used  Sched   Used    ',
     3            'Min    Max  Actual ---- (p.u.) -----')
      call shdlod(3)
      write (outbuf,122)
  122 format (t32, ' Max',
     & t64,'(MVAR) (MVAR) (MVAR) (MVAR) (MVAR) (MVAR) (MVAR) ')
      call shdlod(4)
      outbuf = ' '  
      call shdlod(5)
      call fortop   

C     Process tandem busses adjoined with an LTC transformer   

      do 160 j = 1, ntbxtr  
         jt = tbxtr(1,j)   
         jk = tbxtr(2,j)   
         jm = tbxtr(3,j)   
         k = ltran(1,jt)
         kt = inp2opt(k)  
         m = ltran(9,jt)
         mt = inp2opt(m)  
         txtype = mod(ltran(10,jt),100) 
         if (txtype .ne. 1 .and. txtype .ne. 5) go to 160   

C        Compute Tx flow Qkm

         ek = e(kt) 
         fk = f(kt) 
         em = e(mt) 
         fm = f(mt) 
         vk = sqrt (ek ** 2 + fk ** 2)  
         vm = sqrt (em ** 2 + fm ** 2)  
         la1 = ltran(3,jt)  
         gkm = gkmu(la1)                           
         bkm = bkmu(la1)                           

C        Compute 2-port admittances.  

         gkktx = -gkm / tap(jt) 
         bkktx = -bkm / tap(jt) 

C        Compute currents, injections 

         aim = em * gkm - fm * bkm  
         bim = em * bkm + fm * gkm  
         rh = -ek * bim + fk * aim  
         rj = -ek * aim - fk * bim  
         aim = aim + ek * gkktx - fk * bkktx
         bim = bim + ek * bkktx + fk * gkktx
         qkm = (-ek * bim + fk * aim) * bmva

C        Compute VARS used and scheduled. 

         call allocq (k,sngl(qnetu(kt)),qgenk,qgnmxk,qgnmnk,qloadk, 
     1                totcpk, usecpk, totrkk, userkk, unskek, qerrk)
         call allocq (m,sngl(qnetu(mt)),qgenm,qgnmxm,qgnmnm,qloadm, 
     1                totcpm, usecpm, totrkm, userkm, unskem, qerrm)
         tapk = tran(6,jt) * base(k)
         tapm = tran(6,jt) / tap(jt) * base(m)  
         tapmin = tran(6,jt) / tran(7,jt) * base(m) 
         tapmax = tran(6,jt) / tran(8,jt) * base(m) 
         write (outbuf,130 ) bus(k), base(k), bus(m), base(m), tapmin,
     1      tapm, qkm   
  130    format ('0', a8, f6.1, 1x, a8, f6.1, t32, 2f7.2, f7.1) 

C        Write Bus1 quantities.  

         state = ' '
         if (jk .gt. 0) then
            ltyp = tbx(1,jk)   
            ityp = tbx(7,jk)   
            if (ltyp .eq. 1) then   
               if (ityp .eq. 3) then
                  state = 'Vmin'
               else if (ityp .eq. 4) then   
                  state = 'Vmax'
               endif
            else if (ltyp .eq. 2) then  
               if (ityp .eq. 3) then
                  state = 'Qmin'
               else if (ityp .eq. 4) then   
                  state = 'Qmax'
               endif
            else if (ltyp .eq. 3) then  
               if (ityp .eq. 2) then
                  state = 'Qmin'
               else if (ityp .eq. 3) then   
                  state = 'Qmax'
               else if (ityp .eq. 4) then   
                  state = 'Vmin'
               else if (ityp .eq. 5) then   
                  state = 'Vmax'
               endif
            else if (ltyp .eq. 5) then  
               if (ityp .eq. 2) then
                  state = 'Qmin'
               else if (ityp .eq. 3) then   
                  state = 'Qmax'
               else if (ityp .eq. 4) then   
                  state = 'Qdis'
               endif
            endif   
         endif  
         vk = dsqrt (e(kt) ** 2 + f(kt) ** 2)
         type = 'B '
         call typno (type(2:2), kbsdta(1,k))
         write (outbuf(53:),140) type, state, totcpk, usecpk, totrkk,
     1    userkk, qgnmnk, qgnmxk, qgenk, vlimn(kt), vlimx(kt), vk   
  140    format (' 1 ', a2, 1x, a4, f6.1, 6f7.1, 1x, 3f6.3) 
         call prtout (1)

C        Write Bus2 quantities.

         state = ' '
         if (jm .gt. 0) then
            ltyp = tbx(1,jm)   
            ityp = tbx(7,jm)   
            if (ltyp .eq. 1) then   
               if (ityp .eq. 3) then
                  state = 'Vmin'
               else if (ityp .eq. 4) then   
                  state = 'Vmax'
               endif
            else if (ltyp .eq. 2) then  
               if (ityp .eq. 3) then
                  state = 'Qmin'
               else if (ityp .eq. 4) then   
                  state = 'Qmax'
               endif
            else if (ltyp .eq. 3) then  
               if (ityp .eq. 2) then
                  state = 'Qmin'
               else if (ityp .eq. 3) then   
                  state = 'Qmax'
               else if (ityp .eq. 4) then   
                  state = 'Vmin'
               else if (ityp .eq. 5) then   
                  state = 'Vmax'
               endif
            else if (ltyp .eq. 5) then  
               if (ityp .eq. 2) then
                  state = 'Qmin'
               else if (ityp .eq. 3) then   
                  state = 'Qmax'
               else if (ityp .eq. 4) then   
                  state = 'Qdis'
               endif
            endif   
         endif  
        
         vm = dsqrt (e(mt) ** 2 + f(mt) ** 2)
         type = 'B '
         call typno (type(2:2), kbsdta(1,m))
         write (outbuf,150 ) tapmax, type, state, totcpm, usecpm,   
     1      totrkm, userkm, qgnmnm, qgnmxm, qgenm, vlimn(mt), 
     2      vlimx(mt), vm                         
  150    format (t32, f7.2, t53, ' 2 ', a2, 1x, a4, f6.1, 6f7.1, 1x,
     1      3f6.3)  
         if (ltran(2,jt) .eq. -1) then  
            outbuf(2:4) = '***' 
         else if (ltran(2,jt) .eq. -2) then 
            outbuf(17:19) = '***'   
         endif  
         if (abs (tapm - tapmin) .le. 0.01) then
            outbuf(40:43) = 'Tmin'  
         else if (abs (tapm - tapmax) .le. 0.01) then   
            outbuf(40:43) = 'Tmax'  
         endif  
         call prtout (1)

C        Compute extraneous transformer VAR flow. 

         qnetk = qgenk - qloadk + userkk + usecpk
         qmink = qgnmnk - qloadk + totrkk
         qmaxk = qgnmxk - qloadk + totcpk
         qnetm = qgenm - qloadm + userkm + usecpm
         qminm = qgnmnm - qloadm + totrkm
         qmaxm = qgnmxm - qloadm + totcpm 
         if (qkm .gt. 0.0) then 
            if (jk .eq. 0 .and. jm .eq. 0) then 
               qc = 0.0 
            else if (jk .eq. 0 .and. jm .gt. 0) then
               qc = amin1 (qkm, dim(qmaxm,qnetm))   
            else if (jk .gt. 0 .and. jm .eq. 0) then
               qc = amin1 (qkm, dim(qnetk,qmink))   
            else
               qc = amin1 (dim(qnetk, qmink), qkm, dim(qmaxm,qnetm))
            endif   
         else if (qkm .lt. 0.0) then
            if (jk .eq. 0 .and. jm .eq. 0) then
               qc = 0.0 
            else if (jk .gt. 0 .and. jm .eq. 0) then 
               qc = amax1 (-dim(qmaxk,qnetk), qkm)  
            else if (jk .eq. 0 .and. jm .gt. 0) then 
               qc = amax1 (-dim(qnetm, qminm), qkm)  
            else
               qc = amax1 (-dim(qmaxk,qnetk), qkm, -dim(qnetm, qminm))
            endif   
         else   
            qc = 0.0
         endif  
         if (qc .ne. 0.0) then  
            write (outbuf,152 ) qc  
  152       format (t56, 'Extraneous TX VAR flow:',f7.1, ' MVAR')   
            call prtout (1) 
         endif  
  160 continue  
      outbuf =  
     1   'O End of Summary of LTC Reactive Utilization' 
      call prtout(1)
      outbuf = ' '  
      call rpnlod   
      call shdlod (1)   
      call shdlod (2)   
      call shdlod (3)   
      call shdlod (4)   
      call shdlod (5)   
      call forbtm   
  170 continue  
      return
      end   
