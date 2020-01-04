C     ****************************************************************

C     Purpose: Routine to obtain a filtered loss sensitivity report
C     consisting of the sensitivites dLoss/dPi, dLoss/dQi, and
C     dLoss/dVi


C     Author: Walt Powell  Date: 15 July 1994
C     Called by: get_orpt22.f

C     ****************************************************************

      subroutine lossenrpt(in_buffer, out_buffer, scrfil)
      character in_buffer*(*), out_buffer*(*)
      integer scrfil

      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/com007.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/loscom.inc'
      include 'ipfinc/ordsta.inc'
      include 'ipfinc/owncom.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/sortuvov.inc'
      include 'ipfinc/tbx.inc'

      real ikr, iki
      integer apdoutbuf, o2
      logical finished, found, check_f, load_fltr, chkfltr
      character null*1, type*2, bus_type*10
      external bus_type

C     Write loss sensitivity header.
C     
C     Recompute the Jacobian matrix. The previous Jacobian matrix      
C     cannot be reused because                                         
C     
C     1.  only the upper-diagonal portion is stored and
C     2.  common /AMTRX/ is not physically large enough to accomodate 
C         both upper and lower factors in double precision.
C     
C     To circumvert the second obstacle, the Jacobian is refactored in
C     single precision, which reduces the physical storage requirements
C     by 50%.                                                          
C     
      call senfac()
C     
C     Check sensitivity (debug only)                                   
C     
C     Compute dLoss/dPi, dLoss/dQi sensitivity in the following steps: 
C     
C     1. Compute the objective function dLoss/dX                     
C     2. Compute the LaGrange multipliers                            
C     
      call dldx()
      call baksen(1)

      ploss = 0.0
      qloss = 0.0

      null = char(0)

      numrec = 0
      finished = .false.
      do while ( .not.finished )
        i1 = 1
        in_buffer(1:1) = null
        o2 = 1
        out_buffer(1:1) = null
        range_filter(1) = -9.0e10
        range_filter(2) = +9.0e10
        check_f = load_fltr(in_buffer, out_buffer, i1)
        if ( scrfil .gt. 0 ) then

C         Write Bus sensitivity header.

          call forbtm()

          write (outbuf, 10000) 
10000     format (' Loss Sensitivities')
          call rpnlod()

          write (outbuf, 10010) chase1(1), chase1(34), chase1(35)
10010     format ('Case: ', a10, ' Project: ', 2a10)
          call hedlod()
          call fortop()

        endif

        write (outbuf, 10020)
10020   format (' Type  Bus         Zone    dV/dP     dV/dQ     dQ/dP')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2+length
        if ( scrfil .gt. 0 ) call shdlod(1)

        write (outbuf, 10030)
10030   format (
     &   '                           kV/MW     kV/MVAR   MVAR/MW')
        length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
        o2 = o2+length
        if ( scrfil .gt. 0 ) call shdlod(2)

        if ( check_f ) then

          if ( scrfil .gt. 0 ) then
            outbuf = ' '
            call comlod(1)
            call comlod(2)
            call forbtm()
            call fortop()
            call prnt_fltr(in_buffer(1:))
          endif
          do nbx = 1, ntot_alf
            nb = alf2inp(nbx)
            type(1:1) = 'B'
            call typno (type(2:2), kbsdta(1,nb))
            found = chkfltr (arcnam(jarzn(nb)), zone(nb), owner(nb), 
     &                       base(nb), type, nb)
            if ( found ) then
              kt = inp2opt(nb)
              pload = ploadu(kt)
              qload = qloadu(kt)
              ntyp = ntypu(kt)

C             Skip passive d-c busses

              if ( ntyp .ne. 12 .or. kmlen(kt) .gt. 0 ) then
                vk = dsqrt(e(kt)**2+f(kt)**2)
                pnetx = (pnetu(kt)-inetr(kt)*vk)*bmva
                qnetx = (qnetu(kt)+ineti(kt)*vk)*bmva
                pgen = pnetx+pload*bmva
                qgen = qnetx+qload*bmva
                bskv = base(nb)*vk
                ploss = ploss+pnetx
                qloss = qloss+qnetx

                jt = 0
                if ( kt .le. nbslck ) then
                  dptk = 1.0
                elseif ( kspare(20) .eq. 1 ) then
C                   
C                 Check for Area interchange constraint.    
C                   
                  i1 = iflag(kt)
                  i2 = iflag(kt+1)-1
                  kta = kt+ntota
                  if ( i1 .gt. 0 ) then
                    do i = i1, i2
                      if ( jflag(1, i) .eq. 3 ) goto 100
                    enddo
                    dptk = 1.0-dpt(1, kt+ntota)
                    goto 110
  100               dptk = 1.0
                    jt = jflag(2, i)
                  else
                    dptk = 1.0-dpt(1, kt+ntota)
                  endif
                else
                  dptk = 1.0-dpt(1, kt+ntota)
                endif
  110           dqtk = -dpt(2, kt+ntota)
                if ( kvolt(kt) .ne. 0 ) dvtk = -dpt(2, kt+ntota)*bmva
                if ( jt .ne. 0 ) then

C                 Area slack constraint

                  xatot = 0.0
                  j1 = karea(3, jt)
                  js = karea(4, jt)+j1-1
                  do j = j1, js
                    ix = kaloc(j)
                    if ( ix .gt. 0 ) then
                      k1 = tie(1, ix)
                      k2 = tie(7, ix)
                      ka1 = tie(2, ix)
                      ka2 = tie(8, ix)
                      kdc = tie(9, ix)
                      if ( kdc .gt. 0 ) then
                        kd = kdc
                        do while ( .true. )
                          k1x = dmin1(dcline(1, kd), dcline(2, kd))
                          k2x = dmax1(dcline(1, kd), dcline(2, kd))
                          if ( k1x .eq. min0(k1, k2) ) goto 120
                          if ( kd .ne. kdc ) call erexit()
                          if ( mtdcln .eq. 0 ) call erexit()
                          kd = kdc+mtdcln
                        enddo
  120                   if ( k2x .ne. max0(k1, k2) ) call erexit()
                        if ( k1 .eq. dcline(1, kd) ) then
                          l1 = dcline(8, kd)
                          l2 = dcline(9, kd)
                        else
                          l1 = dcline(9, kd)
                          l2 = dcline(8, kd)
                        endif
                        v1 = dcbus(20, l1)
                        v2 = dcbus(20, l2)
                        pin = v1*(v1-v2)/(dcline(4, kd)*bmva)
                        if ( jt .ne. ka1 ) pin = -pin
                        xatot = xatot+pin
                      else
                        ek = e(k1)
                        fk = f(k1)
                        em = e(k2)
                        fm = f(k2)
                        vksq = ek**2+fk**2
                        g12 = tie(5, ix)
                        b12 = tie(6, ix)
                        ikr = g12*em-b12*fm
                        iki = b12*em+g12*fm
                        rh = -ek*iki+fk*ikr
                        rn = ek*ikr+fk*iki
                        pin = rn+vksq*tie(3, ix)
                        if ( ka1 .ne. jt ) pin = -pin
                        xatot = xatot+pin
                      endif
                    endif
                  enddo
                  xatot = xatot*bmva
                  if ( kvolt(kt) .eq. 0 ) then

C                   SQ constraint

                    write (outbuf, 10040) bus(nb), base(nb), 
     &               zone(nb), dptk, xatot, dqtk, qnetx, vk, bskv, 
     &               bustyp(ntyp)
10040               format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1, 
     &               ' AI ', t50, f8.4, f10.1, t81, f8.4, f8.2, t101, 
     &               a1)
                    dvpu = dim(vk, vlimx(kt))-dim(vlimn(kt), vk)
                    if ( dvpu .gt. 1 ) then
                      write (outbuf(112:), 10050) 'V > Vmax', dvpu
10050                 format (a, f7.4, ' P.U.')
                    elseif ( dvpu .lt. -1 ) then
                      write (outbuf(112:), 10050) 'V < Vmin', dvpu
                    endif
                  else
C                   SV constraint
                    write (outbuf, 10060) bus(nb), base(nb), 
     &                zone(nb), dptk, xatot, qnetx, dvtk, vk, bskv, 
     &                bustyp(ntyp)
10060               format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1, 
     &               ' AI ', t58, f10.1, t71, f10.4, f8.4, f8.2, 
     &               t101, a1)
                    length = apdoutbuf(o2, outbuf(1:132), 
     &                                 out_buffer(o2:))
                    o2 = o2+length
                    if ( scrfil .gt. 0 ) call prtout(1)
                    numrec = numrec+1
                  endif
                elseif ( kvolt(kt) .eq. 0 ) then

C                 PQ constraint

                  write (outbuf, 10070) bus(nb), base(nb), zone(nb), 
     &             dptk, pnetx, dqtk, qnetx, vk, bskv, bustyp(ntyp)
10070             format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1, 
     &             t50, f8.4, f10.1, t81, f8.4, f8.2, t101, a1)
                  dvpu = dim(vk, vlimx(kt))-dim(vlimn(kt), vk)
                  if ( dvpu .gt. 1 ) then
                    write (outbuf(112:), 10050) 'V > Vmax', dvpu
                  elseif ( dvpu .lt. -1 ) then
                    write (outbuf(112:), 10050) 'V < Vmin', dvpu
                  endif
                else

C                 PV constraint

                  write (outbuf, 10080) bus(nb), base(nb), zone(nb), 
     &             dptk, pnetx, qnetx, dvtk, vk, bskv, bustyp(ntyp)
10080             format (t2, a8, f6.1, t20, a2, t27, f8.4, f10.1, 
     &             t58, f10.1, t71, f10.4, f8.4, f8.2, t101, a1)
                endif

C               Check for special bus type

                if ( i1 .gt. 0 ) then
                  do i = i1, i2
                    if ( jflag(1, i) .eq. 8 ) goto 130
                  enddo
                  goto 170
  130             jtbx = jflag(2, i)
                  ltyp = tbx(1, jtbx)
                  ityp = tbx(7, jtbx)
                  if ( ltyp .ne. 1 ) then
                    if ( ltyp .ne. 2 .and. ltyp .ne. 4 ) then
                      if ( ltyp .ne. 5 ) then

C                       BG or BX bus type

                        if ( ityp .eq. 1 .or. ityp .eq. 4 .or.
     &                       ityp .eq. 5 ) goto 160
                        if ( ityp .eq. 2 ) goto 140
                        if ( ityp .eq. 3 ) goto 150
                      endif

                      if ( ityp .ne. 2 ) then
                        if ( ityp .eq. 3 ) goto 150
                        if ( ityp .ne. 4 ) goto 160

                        write (outbuf(106:110), 10090)
10090                   format ('Q dis')
                        goto 170
                      endif

  140                 write (outbuf(106:110), 10100)
10100                 format ('Q min')
                      goto 170

  150                 write (outbuf(106:110), 10110)
10110                 format ('Q max')
                      goto 170

  160                 dv = dim(vk, 0.999*vlimx(kt))
     &                   - dim(1.001*vlimn(kt), vk)
                      mt = tbx(8, jtbx)
                      if ( dv .lt. 0.0 ) then
                        write (outbuf(106:110), 10120)
10120                   format ('V min')
                      elseif ( dv .gt. 0.0 ) then
                        write (outbuf(106:110), 10130)
10130                   format ('V max')
                      endif

C                   BQ or BO bus type

                    elseif ( ityp .ne. 1 .and. 
     &                       ityp .ne. 2 .and.
     &                       ityp .ne. 5 .and.
     &                       ityp .ne. 6 ) then
                      if ( ityp .eq. 4 ) then

                        write (outbuf(106:110), 10140)
10140                   format ('Q max')
                      else
                        write (outbuf(106:110), 10150)
10150                   format ('Q min')
                      endif
                    endif
                  endif
                endif
  170           length = apdoutbuf(o2, outbuf(1:132), 
     &                             out_buffer(o2:))
                o2 = o2+length
                if ( scrfil .gt. 0 ) call prtout(1)
                numrec = numrec+1
              endif
            endif
          enddo
          call space(1)
          write (outbuf, 10160) ploss, qloss
10160     format (t2, ' TOTAL LOSSES ', f10.2, ' (MW)', f10.2, 
     &     ' (MVAR)')
          length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
          o2 = o2+length
          if ( scrfil .gt. 0 ) call prtout(1)
          numrec = numrec+1
        else
          finished = .true.
        endif
      enddo
      return
      end
