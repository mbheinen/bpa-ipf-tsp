C****************************************************************
C
C   File: bussenrpt.f
C
C   Purpose: Routine to obtain a filtered bus sensitivity report
C            (DVolt/DP and Dvolt/DQ)
C
C   Author: Walt Powell  Date: 15 July 1994
C   Called by: get_orpt22.f
C
C****************************************************************
C
        subroutine bussenrpt (in_buffer, out_buffer, scrfil)
        character in_buffer *(*), out_buffer *(*)
        integer scrfil

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/alpha2.inc'
        include 'ipfinc/amtrx.inc'
        include 'ipfinc/beta2.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/gamma.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/intbus.inc'
        include 'ipfinc/optim1.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/slnopt.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/sensit.inc'
        include 'ipfinc/tran.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/qksrt.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/ordsta.inc'

        common /file_name/ filename
        character filename*60

	character bus_type*10, null*1, linefeed*1, text*80
        logical found, gtfltr, change_f
        integer kmpuvov, apdoutbuf, findstr, o2
        real dpt_temp(2,MAXBUS+MAXLTC)
        external kompsv, swapsv, kmpuvov, swapuvov, bus_type
        
        save

        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        maxbuf_out = len( out_buffer ) - 400
c
c       Search and align to "WHERE" ...
c
        ix = findstr(in_buffer, 'WHERE') ! findstr is a case-insensitive
c                                        ! version of index
        found = .false.
        if (ix .gt. 0) then
           ix = ix + len('WHERE')
           change_f = gtfltr(in_buffer(ix:))
           found = .true.
        endif
        if (found) then
           ix = 1
           do while (ix .le. filter(6) .and. o2. le. maxbuf_out)
              nb = bus_filter(ix)
              if (ordvlt .eq. 1) then
                 kt = nb
              else
                 kt = inp2opt(nb)
              endif
              if (scrfil. gt. 0) then

C                Write Bus sensitivity header.

                 call forbtm()

                 write (outbuf, 10000) bus(nb), base(nb)
10000            format (' Bus Sensitivities for ', a8, f6.1)
                 call rpnlod()
    
                 write (outbuf, 10010) chase1(1), chase1(34), chase1(35)
10010            format('Case: ', a10, ' Project: ', 2a10)
                 call hedlod()
                 call fortop()

              endif

              write (outbuf, 10020) bus(nb), base(nb)
10020         format (' Effects on bus ', a8, f6.1, 
     &         ' of a 1.0 MW and 1.0 MVAR perturbation ')
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
              if (scrfil .gt. 0) call shdlod(1)

              write (outbuf, 10030)
10030         format (t2, 'Type Zone Owner', t18, 'Bus1', t33, 'Bus2',
     &                t48, '   Vm', 
     &                t56, ' dVk/dPm', 
     &                t68, ' dVk/dQm')
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
              if (scrfil .gt. 0) call shdlod(2)

              write (outbuf, 10040)
10040         format (t48, '   Tap', 
     &                t56, ' dTk/dPm', 
     &                t68, ' dTk/dQm')
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
              if (scrfil .gt. 0) call shdlod(3)

              write (outbuf, 10050)
10050         format(t48, ' (p.u.)', t56,'(kV/MW)', t68, 
     &          '(kV/MVAR)')
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
              if (scrfil .gt. 0) call shdlod(4)

              write (outbuf, 10060)
10060         format(t48, ' (Tap)', t56,'(Deg/MW)', T68, '(Deg/MVAR)')
              length = apdoutbuf(o2, outbuf(1:132), out_buffer(o2:))
              o2 = o2 + length
              if (scrfil .gt. 0) call shdlod(5)

              if (scrfil .gt. 0) then
                 outbuf = ' '
                 call comlod(1)
                 call comlod(2)
                 call forbtm()
                 call fortop()
                 call prnt_fltr (in_buffer(ix:))
              endif
c
c             Compute sensitivity dV/dP
c
              do i = 1,ntotx
                 dpt(1,i) = 0.0d0
                 dpt(2,i) = 0.0d0
              enddo
              dpt(1,kt+ntota) = 1.0d0
 
              call baksen (0)
              do i = 1,ntotx
                 dpt_temp(1,i) = dpt(2,i)
              enddo
c
c             Compute sensitivity dV/dQ
c
              do i = 1,ntotx
                 dpt(1,i) = 0.0d0
                 dpt(2,i) = 0.0d0
              enddo
              dpt(2,kt+ntota) = 1.0d0
 
              call baksen (0)
              do i = 1,ntotx
                 dpt_temp(2,i) = dpt(2,i)
              enddo
c
c             Retrieve 10 largest dV/dQ sensitivities
c
              call getmaxdpt (dpt, 2, 10, nt, maxsen(1), senmax(1))
              do i = 1, nt
                 maxsrt(i) = i
                 senmax(i) = 0.0
              enddo
              if (nt .gt. 1) then
                 call qiksrt (1, nt, kompsv, swapsv)
              endif

              i = 1
              do while (i .le. nt)
                 m = maxsrt(i)
                 kt = maxsen(m)
                 if (kt .gt. ntota) then
                    mt = kt - ntota
                    nbo = opt2inp(mt)
                    vold = dsqrt(e(mt)**2 + f(mt)**2)
                    text = bus_type(nbo)
                    dvdp = (dpt_temp(1,kt) * vold * base(nbo)) / bmva
                    dvdq = (dpt_temp(2,kt) * vold * base(nbo)) / bmva
                    write (outbuf, 10070) text(1:2), zone(nbo), 
     &                 owner(nbo), bus(nbo), base(nbo), vold, dvdp, 
     &                 dvdq
10070               format(t2, a2, 3x, a2, 3x, a3, t18, a8, f6.1, t48,
     &                 f6.3, t56, f8.4, t68, f8.4) 
                    length = apdoutbuf(o2, outbuf(1:132), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    if (scrfil .gt. 0) call prtout(1)
                 else
                    if (ordltc .eq. 1) then
                       k1 = ltran(1,kt)
                       k2 = ltran(9,kt)
                    else
                       k1 = opt2inp(ltran(1,kt))
                       k2 = opt2inp(ltran(9,kt))
                    endif
                    told = 57.2957795 * tap(kt)
                    dtdp = 57.2957795 * dpt_temp(1,kt) / bmva
                    dtdq = 57.2957795 * dpt_temp(2,kt) / bmva
                    write (outbuf,10080) 'RP', zone(k1), bus(k1),
     &                  base(k1), bus(k2), base(k2), told, dtdp, dtdq
10080               format(t2, a2, 3x, a2, t18, a8, f6.1, t32, a8, f6.1,
     &                  t48, f6.1, t56, f8.4, t68, f8.4)
                    length = apdoutbuf(o2, outbuf(1:132), 
     &                                 out_buffer(o2:))
                    o2 = o2 + length
                    if (scrfil .gt. 0) call prtout(1)
                 endif
c
c                Look ahead to see if next entity is for same bus.
c
                 if (i .lt. nt .and. maxsen(maxsrt(i+1)) .eq. kt) then
                    i = i + 1
                 endif
                 i = i + 1
              enddo
              ix = ix + 1
           enddo
        endif
        return
        end
