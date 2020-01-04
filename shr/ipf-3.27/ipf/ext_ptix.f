C    @(#)ext_ptix.f	20.10 5/3/00
C****************************************************************  
C  
C     File: ext_ptix.f  
C  
C     Purpose: Routine to extract switched reactance data in PTI   
C              format  
C  
C     Input parameters:  
C  
C             savfil   - the logical unit opened  
C             version  - "23" or "24"  
C  
C     Author: Walt Powell  Date: 21 May 1996  
C     Called by: saveptid.f  
C  
C****************************************************************  
      integer function ext_ptix (savfil, version, option)  
      integer savfil, version  
      character *(*) option(10)  
  
      include 'ipfinc/parametr.inc'  
  
      include 'ipfinc/blank.inc'  
      include 'ipfinc/area.inc'  
      include 'ipfinc/arcntl.inc'  
      include 'ipfinc/pti_data.inc'  
      include 'ipfinc/filnam.inc'  
      include 'ipfinc/lfiles.inc'  
      include 'ipfinc/bus.inc'  
      include 'ipfinc/cbus.inc'  
      include 'ipfinc/alpha.inc'  
      include 'ipfinc/prt.inc'  
      include 'ipfinc/xdata.inc'  
      include 'ipfinc/tbx.inc'  
      include 'ipfinc/tbxsrt.inc'  
      include 'ipfinc/ordsta.inc'  
   
      integer total1, ktemp(20), pti_type, status, write_ge_file, 
     &        gtptinum, add_ptiq

      character code*10, xbuf*132, base1c*4, base2c*4, cxdata(2,8)*8,
     &          cbtype*1
      real temp(26)
      equivalence (temp, ktemp)

      logical prt_names

      prt_names = (option(4) .eq. 'Y' .or. option(4) .eq. 'y')  

        total1 = 0
        do jt = 1, kxtot
          k1 = xdata(1,jt)
          if (ordtbx .eq. 2) k1 = opt2inp(k1)   
          k2 = xdata(2,jt)
          if (k2 .eq. 0) k2 = k1 
          base1c = code (base(k1), 4, 0)
          base2c = code (base(k2), 4, 0)
c  
c         Get PTI area, zone, and bus number  
c  
          status = gtptinum (k1, iptia1, iptiz1, iptib1)  
          status = gtptinum (k2, iptia2, iptiz2, iptib2)  
          iptio1 = add_ptiq (owner(k1))

          ntype = kbsdta(1,k1)
          qgen = 0.0
          call gtptibsv (k1, temp, version, pti_type, qgen)

          if (ntype .eq. 11) then
            modsw = 1               ! Discrete
          else if (ntype .eq. 2 .or. ntype .eq. 3) 
     &      then
            modsw = 2               ! Continuous
          else if (ntype .eq. 7) then
            if (temp(9) .gt. temp(10)) then
              modsw = -1            ! Disabled - shunt is in Qmax, Qmin
            else
              modsw = 2             ! Continuous
            endif
          else
            modsw = 0               ! Fixed
          endif
          vswhi = vlimx(inp2opt(k2))
          vswlo = vlimn(inp2opt(k2))
          if (k1 .ne. k2 .and. 
     &        (kbsdta(1,k2) .eq. 1 .or. kbsdta(1,k2) .eq. 4 .or.
     &         kbsdta(1,k2) .eq. 10)) then
            numptik2 = pti_num(iptib2)
          else
            numptik2 = 0
          endif

          btot = xdata(3,jt) + xdata(4,jt)
          binit = xdata(5,jt) + xdata(6,jt)
c
c         
c         All shunt B is fixed when PTI control mode MODSW = 0  
          if (modsw .eq. 0) then
            badj = 0.0
            bfixed = temp(17) + temp(18)
          else
            badj = temp(18)
            bfixed = temp(17)
          endif
          if (modsw .eq. 2) then
            disc = xdscrt (jt, k1, badj, b1, b2)     
            if (xdata(3,jt) .lt. -100.0 .or. xdata(4,jt) .gt. 100.0) 
     &        then
              tolerance = 1.0
            else
              tolerance = 0.1
            endif 
            if (abs(disc - badj) .gt. tolerance) then
              call typno (cbtype, ntype)
              write (errbuf(1), 10000) cbtype, bus(k1), base(k1), badj, 
     &          btot
10000         format (' Type B', a, ' bus ', a, f6.1, ' continuous B ',
     &          f8.1, ' does not match X-data ', f8.1)
              call prterx ('W', 1)
            endif
          else if (modsw .eq. 0 .and. abs(btot - bfixed) .gt. 2.0) then
            call typno (cbtype, ntype)
            write (errbuf(1), 10010) cbtype, bus(k1), base(k1), bfixed, 
     &        btot
10010       format (' Type B', a, ' bus ', a, f6.1, ' fixed B ',
     &        f8.1, ' does not match X-data ', f8.1)
            call prterx ('W', 1)
          endif
          do i = 1,8  
            j = 2*i + 5   
            n = xdata(j,jt)
            if (n .eq. 0) then
              cxdata(1,i) = ' '
              cxdata(2,i) = ' '
            else
              write (cxdata(1,i), '(i2)') n
              write (cxdata(2,i), '(f8.1)') xdata(j+1,jt)
            endif
          enddo

          if (modsw .ge. 0) then

            write (xbuf, 10020) pti_num(iptib1), modsw, vswhi, vswlo,
     &        numptik2, binit, (cxdata(1,i), cxdata(2,i), i = 1,8)
10020       format (i5, i2, 2f6.3, i6, f8.1, 8(a2, a8))
            if (prt_names)  then
              last = lastch (xbuf)
              write (xbuf(last+1:), 10030) bus(k1), base1c, 
     &          pti_znam(iptiz1), pti_onam(iptio1)
10030         format (' / ', a, 1x, a, 1x, a, 1x, a)
            endif

            last = lastch (xbuf)
            status = write_ge_file (0, xbuf(1:last))
            total1 = total1 + 1
          endif
        enddo

        xbuf = ' 0 / Begin Transformer Impedance Correction Data'
        last = lastch (xbuf)
        status = write_ge_file (0, xbuf(1:last))

        write ( errbuf(1), 10100) total1
10100   format (' Total switched reactance records extracted:', t45, i5)
        call prterx ('I', 1)
 
        ext_ptix = total1

        return
        end
