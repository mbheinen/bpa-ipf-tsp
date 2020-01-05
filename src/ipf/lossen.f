C    @(#)lossen.f	20.5 11/12/98
      subroutine lossen
C
C     compute the loss sensitivities
C                  
C     1. dLoss/dPi 
C     2. dLoss/dQi 
C     3. dLoss/dVi 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/loscom.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/tbx.inc'
 
      character tag(3) * 1
      real ikr, iki

C     Write loss sensitivity header.

      call forbtm
      call fortop
      write (outbuf,100)
  100 format (t11, ' Loss_sensitivities ',
     &              'computed with the following controls:')
      call prtout (1)
      call space(1)
      if (kspare(19) .eq. 0) then
         tag(1) = 'X'
         tag(2) = ' '
         tag(3) = ' '
      else if (kspare(19) .eq. 1) then
         tag(1) = ' '
         tag(2) = ' '
         tag(3) = 'X'
      else
         tag(1) = ' '
         tag(2) = 'X'
         tag(3) = ' '
      endif
      write (outbuf,110) tag(1)
  110 format (t18, 'LTC control', t43, '(', a, ')', 3x, 'Off')
      call prtout (1)
 
      write (outbuf,120) tag(2)
  120 format (t18, '--- -------', t43, '(', a, ')', 3x,
     1   'on (Full control)')
      call prtout (1)
 
      write (outbuf,130) tag(3)
  130 format (t43, '(', a, ')', 3x, 'On (No voltage control)')
      call prtout (1)
 
      call space (1)
        
      if (kspare(20) .eq. 0) then   
         tag(1) = 'X'   
         tag(2) = ' '   
         tag(3) = ' '   
      else if (kspare(20) .eq. 1) then  
         tag(1) = ' '   
         tag(2) = 'X'   
         tag(3) = ' '   
      else  
         tag(1) = ' '   
         tag(2) = ' '   
         tag(3) = 'X'   
      endif 
      write (outbuf,140) tag(1) 
  140 format (t18, 'AI control', t43, '(', a, ')', 3x, 'Off')   
      call prtout (1)   
        
      write (outbuf,150) tag(2) 
  150 format (t18, '-- -------', t43, '(', a, ')', 3x,  
     1   'Control (default)')   
      call prtout (1)   
        
      write (outbuf,160) tag(3) 
  160 format (t43, '(', a, ')', 3x, 'Monitor')  
      call prtout (1)   
      call space (1)
        
      if (numlsz .ne. 0) then   
         write (outbuf,162) (lszone(i),i=1,numlsz)  
  162    format (t18, 'Analysis restricted to following zones :',   
     1      20(1x,a2))  
         call prtout (1)
      else if (numlsa .ne. 0) then  
         num = min0 (numlsa,6)  
         write (outbuf,164) (lsarea(i),i=1,num) 
  164    format (t18, 'Analysis restricted to following areas :',   
     1      6(1x,a10))  
         call prtout (1)
         do 168 j = num+1, numlsa, 6
            num = min0 (numlsz,j+5) 
            write (outbuf,164) (lsarea(i),i=j,num)  
  166       format (t59, 6(1x,a10)) 
            call prtout (1) 
  168    continue   
      endif 
C                                                                      *
C     Recompute the Jacobian matrix. The previous Jacobian matrix      *
C     cannot be reused because                                         *
C                                                                      *
C     1.  only the upper-diagonal portion is stored and 
C     2.  common /AMTRX/ is not physically large enough to  
C         accomodate both upper and lower factors in double 
C         precision.
C                                                                      *
C     To circumvert the second obstacle, the Jacobian is refactored in *
C     single precision, which reduces the physical storage requirements*
C     by 50%.                                                          *
C                                                                      *
      call senfac   
C                                                                      *
C     Check sensitivity (debug only)                                   *
C                                                                      *
C     Compute dLoss/dPi, dLoss/dQi sensitivity in the following steps: *
C                                                                      *
C       1. Compute the objective function dLoss/dX                     *
C       2. Compute the LaGrange multipliers                            *
C                                                                      *
      call dldx 
      call baksen (1)   
        
      call forbtm   
      write (outbuf,190 )   
  190 format (t2, 'Bus name  Base', t20, 'Zone',
     1       t28, 'dLOSS/dP at PNET',   
     2       t51, 'dLOSS/dQ at QNET',   
     3       t80, 'dLOSS/dV at V',  
     4       t100, 'Bus',   
     5       t106, 'Comments')  
      call shdlod(1)
      write (outbuf,200 )   
  200 format (t12, '(KV)',  
     1        t28, '(MW/MW)    (MW)',   
     2        t51, '(MW/MVAR)  (MVAR)', 
     3        t72, '(MW/p.u.)  (p.u.)    (KV)', 
     4        t100,'type')  
      call shdlod(2)
        
      outbuf = ' '  
      do 210 i = 3, 5   
         call shdlod(i) 
  210 continue  
      call fortop   
        
      call space (2)
      ploss = 0.0   
      qloss = 0.0   
        
      do 540 ib = 1, ntot_alf
         nb = alf2inp(ib)   
         kt = inp2opt(nb) 
         pload = ploadu(kt)
         qload = qloadu(kt)
         ntyp = ntypu(kt)                          

C        Skip passive d-c busses 

         if (ntyp .eq. 12 .and. kmlen(kt) .eq. 0) go to 540
         vk = dsqrt(e(kt)**2 + f(kt)**2) 
         pnetx = (pnetu(kt) - inetr(kt)*vk) * bmva    
         qnetx = (qnetu(kt) + ineti(kt)*vk) * bmva   
         pgen = pnetx + pload * bmva
         qgen = qnetx + qload * bmva
         bskv = base(nb) * vk   
         ploss = ploss + pnetx  
         qloss = qloss + qnetx  
C       
C        Check whether bus is to be printed.
C       
         if (ikk(5,kt+ntota) .eq. 0) go to 540  
         jt = 0 
         if (kt .le. nbslck) then   
            dptk = 1.0
         else if (kspare(20) .eq. 1) then   
C                                                                      *
C           Check for Area interchange constraint.                     *
C                                                                      *
            i1 = iflag(kt)  
            i2 = iflag(kt+1) - 1
            kta = kt + ntota
            if (i1 .gt. 0) then 
               do 220 i = i1, i2
                  if (jflag(1,i) .eq. 3) then   
                     dptk = 1.0
                     jt = jflag(2,i)
                     go to 230  
                  endif 
  220          continue 
               dptk = 1.0 - dpt(1,kt+ntota) 
  230          continue 
            else
               dptk = 1.0 - dpt(1,kt+ntota) 
            endif   
         else   
            dptk = 1.0 - dpt(1,kt+ntota)
         endif  
         dqtk = -dpt(2,kt+ntota)
         if (kvolt(kt) .ne. 0) then 
            dvtk = -dpt(2,kt+ntota) * bmva  
         endif  
         if (jt .eq. 0) go to 290   

C        Area slack constraint

         xatot = 0.0
         j1 = karea(3,jt)   
         js = karea(4,jt) + j1 - 1  
         do 250 j = j1, js  
            ix = kaloc(j)   
            if (ix .lt. 0) go to 250
            k1 = tie(1,ix) 
            k2 = tie(7,ix) 
            ka1 = tie(2,ix)
            ka2 = tie(8,ix)
            kdc = tie(9,ix)
            if (kdc .gt. 0) then
               kd = kdc 
  240          k1x = dmin1 (dcline(1,kd),dcline(2,kd))
               k2x = dmax1 (dcline(1,kd),dcline(2,kd))
               if (k1x .ne. min0(k1,k2)) then   
                  if (kd .ne. kdc) call erexit  
                  if (mtdcln .eq. 0) call erexit
                  kd = kdc + mtdcln 
                  go to 240 
               else if (k2x .ne. max0(k1,k2)) then  
                  call erexit   
               endif
               if (k1 .eq. dcline(1,kd)) then   
                  l1 = dcline(8,kd) 
                  l2 = dcline(9,kd) 
               else 
                  l1 = dcline(9,kd) 
                  l2 = dcline(8,kd) 
               endif
               v1 = dcbus(20,l1)
               v2 = dcbus(20,l2)
               pin = v1 * (v1 - v2) / (dcline(4,kd)*bmva)   
               if (jt .ne. ka1) pin = -pin  
               xatot = xatot + pin  
            else
               ek = e(k1)   
               fk = f(k1)   
               em = e(k2)   
               fm = f(k2)   
               vksq = ek ** 2 + fk ** 2 
               g12 = tie(5,ix)  
               b12 = tie(6,ix)  
               ikr = g12 * em - b12 * fm
               iki = b12 * em + g12 * fm
               rh = -ek * iki + fk * ikr
               rn = ek * ikr + fk * iki 
               pin = rn + vksq * tie(3,ix)  
               if (ka1 .ne. jt) then
                  pin = -pin
               endif
               xatot = xatot + pin  
            endif   
  250    continue   
         xatot = xatot * bmva   
         if (kvolt(kt) .ne. 0) then 
C                               SV constraint   
            write (outbuf,260 ) bus(nb), base(nb), zone(nb), dptk,  
     1         xatot, qnetx, dvtk, vk, bskv, bustyp(ntyp)   
  260       format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1, ' AI ',
     1         t58, f10.1, t71, f10.4, f8.4, f8.2, t101, a1)
         else   

C           SQ constraint  

            write (outbuf,270 ) bus(nb), base(nb), zone(nb), dptk,  
     1         xatot, dqtk, qnetx, vk, bskv, bustyp(ntyp)   
  270       format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1, ' AI ',
     1         t50, f8.4, f10.1, t81, f8.4, f8.2, t101, a1) 
            dvpu = dim(vk,vlimx(kt)) - dim(vlimn(kt),vk)  
            if (dvpu .gt. 0001) then
               write (outbuf(112:),280) 'V > Vmax', dvpu
  280          format (a, f7.4, ' P.U.')
            else if (dvpu .lt. -0001) then  
               write (outbuf(112:),280) 'V < Vmin', dvpu
            endif   
         endif  
         go to 320  
  290    if (kvolt(kt) .eq. 0) then 

C           PQ constraint 

            write (outbuf,300 ) bus(nb), base(nb), zone(nb), dptk,  
     1         pnetx, dqtk, qnetx, vk, bskv, bustyp(ntyp) 
  300       format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1,
     1         t50, f8.4, f10.1, t81, f8.4, f8.2, t101, a1) 
            dvpu = dim(vk,vlimx(kt)) - dim(vlimn(kt),vk)  
            if (dvpu .gt. 0001) then
               write (outbuf(112:),280) 'V > Vmax', dvpu
            else if (dvpu .lt. -0001) then  
               write (outbuf(112:),280) 'V < Vmin', dvpu
            endif   
         else   

C           PV constraint  

            write (outbuf,310 ) bus(nb), base(nb), zone(nb), dptk,  
     1         pnetx, qnetx, dvtk, vk, bskv, bustyp(ntyp)   
  310       format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1,
     1         t58, f10.1, t71, f10.4, f8.4, f8.2, t101, a1)
         endif  

C        Check for special bus type  

  320    if (i1 .gt. 0) then
            do 330 i = i1, i2   
               if (jflag(1,i) .eq. 8) then  
                  jtbx = jflag(2,i) 
                  go to 340 
               endif
  330       continue
            go to 530   
  340       ltyp = tbx(1,jtbx) 
            ityp = tbx(7,jtbx) 
            go to (530,480,350,480,360) ltyp

C           BG or BX bus type 

  350       go to (370,420,440,370,370), ityp   

  360       go to (370,420,440,460,370,370), ityp   

  370       dv = dim(vk,0.999*vlimx(kt)) - 
     &           dim(1.001*vlimn(kt),vk)  
            mt = tbx(8,jtbx)   
            if (dv .lt. 0.0) then   
               write (outbuf(106:110),380)  
  380          format ('V min') 
            else if (dv .gt. 0.0) then  
               write (outbuf(106:110),390 ) 
  390          format ('V max') 
            endif   
            go to 530   

  420       write (outbuf(106:110),430) 
  430       format ('Q min')
            go to 530
 
  440       write (outbuf(106:110),450)
  450       format ('Q max')
            go to 530
 
  460       write (outbuf(106:110),470)
  470       format ('Q dis')
            go to 530   

C           BQ or BO bus type

  480       go to (530,530,490,510,530,530), ityp   
  490       write (outbuf(106:110),500) 
  500       format ('Q min')
            go to 530
 
  510       write (outbuf(106:110),520)
  520       format ('Q max')
         endif  
  530    call prtout (1) 
  540 continue  
      call space (1)
      write (outbuf, 542) ploss, qloss  
  542 format (t2,' TOTAL LOSSES ', f10.2, ' (MW)', f10.2, ' (MVAR)')
      call prtout (1)   
      call forbtm   
      outbuf = ' '  
      do 550 i = 1, 5   
         call shdlod(i) 
  550 continue  
      return
      end   
