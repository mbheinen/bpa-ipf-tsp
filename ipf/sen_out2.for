C    @(#)sen_out2.f	20.4 5/27/98
      subroutine sen_out2 (num_brn, brn_ptr, pij, qij, threshold, 
     &           gen_flag, scrfil, excel_flag, excel_char, ps_flag,
     &           p_level)
      integer num_brn, brn_ptr(*), scrfil, p_level
      real pij(*), qij(*), threshold
      logical gen_flag, excel_flag,  ps_flag 
      character excel_char*1
      
      include 'ipfinc:parametr.inc'
      include 'ipfinc:alpha.inc'
      include 'ipfinc:alpha2.inc'
      include 'ipfinc:amtrx.inc'
      include 'ipfinc:arcntl.inc'
      include 'ipfinc:area.inc'
      include 'ipfinc:blank.inc'
      include 'ipfinc:bus.inc'
      include 'ipfinc:cbus.inc'
      include 'ipfinc:anlys.inc'
      include 'ipfinc:busanl.inc'
      include 'ipfinc:branch.inc'
      include 'ipfinc:dc.inc'
      include 'ipfinc:dtaiop.inc'
      include 'ipfinc:ecvar.inc'
      include 'ipfinc:lfiles.inc'
      include 'ipfinc:filnam.inc'
      include 'ipfinc:jobctl.inc'
      include 'ipfinc:pageno.inc'
      include 'ipfinc:header.inc'
      include 'ipfinc:prt.inc'
      include 'ipfinc:ordsta.inc'
      include 'ipfinc:slnopt.inc'
      include 'ipfinc:com007.inc'
      include 'ipfinc:phase.inc'
      include 'ipfinc:tran.inc'
      include 'ipfinc:ownhash.inc'
      include 'ipfinc:qksrt.inc'
      include 'ipfinc:optim1.inc'
      include 'ipfinc:slnphs.inc'

      common /basecase/casename(2), filename(2)
      character casename*10, filename*60

      common /scratch/ numdiff, ixref(MAXBUS), kdiff(2,MAXBUS),
     &                 fdiff(6,MAXBUS), cdiff(2,MAXBUS),
     &                 lphtx(2,MAXPHS), phtx(6,MAXPHS)
      character cdiff * 1

      character cbtype*1, cbown*3, id*1, cbyear*2, type*1, lntype(9)*2,
     &          text*40, query*1, excel_sep*3
      logical found, prt_flag
      integer sect, p, ptr, ownsch, kmpvltdif
      real total1(12), total2(2, 10), pct(8)
      external kmpvltdif, swpvltdif

      data lntype / 'L*', 'LM', 'L ', 'R ', 'T ', 'TP', 'E ', 'LM',
     &              'RZ' /

      excel_sep = ' ' // excel_char // ' '
      call forbtm()
c
c     Initialize analysis arrays
c
      do i = 1, 10
        total2(1, i) = 0.0
        total2(2, i) = 0.0
      enddo

      do i=1,MAXOWN
         do j=1,10
            syst(j,i) = 0.0
         enddo
      enddo

      do i = 1, 4
        total1(i) = 0.0
      enddo

      if (.not. excel_flag) then
        write (scrfil, 10100) casename(1)
10100   format (t2, 'Path Use Factors composition for case: ', a)

        write (scrfil, 10110) 
10110   format (t2, 'Type', t5, 'Branch ', t36, ' ----- Flow ----')

        write (scrfil, 10112) 
10112   format (t36, '  (MW)     (MVAR)')

        write (scrfil, '(1x)')
      endif

      totpij = 0.0
      totqij = 0.0
      do jt = 1, num_brn
        ptr = brn_ptr(jt)
        k1 = kx(ptr)
        k2 = ky(ptr)
        id = brid(ptr)
        sect = brsect(ptr)
        totpij = totpij + pij(jt)
        totqij = totqij + qij(jt)

        if (.not. excel_flag) then
          write (scrfil, 10120) lntype(brtype(ptr)), bus(k1), base(k1), 
     &      bus(k2), base(k2), id, pij(jt), qij(jt)
10120     format (t2, a, t5, a8, f7.1, 1x, a8, f7.1, 1x, a, t36, f7.1,
     &      t46, f7.1)
        endif
      enddo
      if (.not. excel_flag) then
        write (scrfil, 10130) 
10130   format (t35, '  -----', t45, '  -----')
        write (scrfil, 10140) totpij, totqij
10140   format (t2, 'Total', t35, f7.1, t45, f7.1)
           
        if (gen_flag) then
          text = 'Listing confined to Generators'
        else
          text = 'Listing includes Loads and Generators'
        endif

        write (scrfil, '(1x)')

        write (scrfil, 10150) casename(1), threshold, text
10150   format (t2, 'Path Use Factors composition for case: ', a, 1x,
     &    'Threshold = ', f6.4, 2x, a)

        write (scrfil, 10160)
10160   format (t2, 'Zone Owner Constraint type',  t51, 'dPij/dx',  
     &    t61, 'Pij Contribution', t81, 'Constraint', t110, 'Area')

        write (scrfil, 10170)
10170   format (t51, ' (MW/x)', t61, '   (MW)    (%)', t81, '   (x)')

        write (scrfil, '(1x)')
      endif
c
c     First pass - compute Linearization error
c
      do nbx = 1, ntot_alf
        nb = alf2inp(nbx)
        kt = inp2opt(nb)
        pload = (ploadu(kt)+inetr(kt)*vk)*bmva
        qload = (qloadu(kt)-ineti(kt)*vk)*bmva
        ntyp = ntypu(kt)

C       Skip passive d-c buses

        if ((ntyp .ne. 5 .and. ntyp .ne. 12) .or. kmlen(kt) .ne. 0) then
          vk = dsqrt(e(kt)**2+f(kt)**2)
          pgen = pnetu(kt)*bmva + pload
          qgen = qnetu(kt)*bmva + qload
          pnetx = pgen - pload
          qnetx = qgen - qload
          bskv = base(nb)*vk
          dptk =  -dpt(1, kt)
          datk = 0.0
          dqtk =  -dpt(2, kt)
          if (kvolt(kt) .ne. 0) dqtk = 0.0
          dvtk = 0.0
          jt = 0
          if (kt .gt. nbslck) then
            if (kspare(20) .eq. 1) then
C             
C             Check for Area interchange constraint.                   
C             
              i1 = iflag(kt)
              i2 = iflag(kt+1) - 1
              found = .false.
              do while (i1 .gt. 0 .and. i1 .le. i2 .and. .not. found) 
                if (jflag(1, i1) .eq. 3) then
                  found = .true.
                  dptk = 0.0
                  datk =  -dpt(1, kt)
                  jt = jflag(2, i1)
                  xatot = area(2, jt) * bmva
                else
                  i1 = i1 + 1
                endif
              enddo
            endif
          else
            dptk = 0.0
            dqtk = 0.0
            datk = 0.0
          endif

          if (jt .ne. 0) then

C           Area slack constraint

            if (kvolt(kt) .eq. 0) then

C             SQ constraint

              pct1 = datk
              pct2 = dqtk
              total1(1) = total1(1) + datk*xatot
              total1(2) = total1(2) + pct1
              total1(3) = total1(3) + dqtk*qnetx
              total1(4) = total1(4) + pct2

              total2(1, 3) = total2(1, 3) + xatot
              total2(2, 3) = total2(2, 3) + datk*xatot
              total2(1, 4) = total2(1, 4) + qgen
              total2(2, 4) = total2(2, 4) - dqtk*qgen
              total2(1, 5) = total2(1, 5) + qload
              total2(2, 5) = total2(2, 5) - dqtk*qload
            else

C             SV constraint

              pct1 = datk
              total1(1) = total1(1) + datk*xatot
              total1(2) = total1(2) + pct1

              total2(1, 3) = total2(1, 3) + xatot
              total2(2, 3) = total2(2, 3) + datk*xatot
            endif
          elseif (kvolt(kt) .eq. 0) then

C           PQ constraint

            pct1 = dptk
            pct2 = dqtk
            total1(1) = total1(1) + dptk*pnetx
            total1(2) = total1(2) + pct1
            total1(3) = total1(3) + dqtk*qnetx
            total1(4) = total1(4) + pct2
            type = bustyp(ntypu(kt))

            if (ntyp .eq. 5 .or. ntyp .eq. 12) then
              total2(1, 6) = total2(1, 6) + pnetx
              total2(2, 6) = total2(2, 6) + dptk*pnetx
              total2(1, 7) = total2(1, 7) + qnetx
              total2(2, 7) = total2(2, 7) + dqtk*qnetx
            else
              temp1 = -dptk*pload
              total2(1, 1) = total2(1, 1) + pgen
              total2(2, 1) = total2(2, 1) + dptk*pgen
              total2(1, 2) = total2(1, 2) + pload
              total2(2, 2) = total2(2, 2) - dptk*pload
              total2(1, 4) = total2(1, 4) + qgen
              total2(2, 4) = total2(2, 4) + dqtk*qgen
              total2(1, 5) = total2(1, 5) + qload
              total2(2, 5) = total2(2, 5) - dqtk*qload
            endif
          else

C           PV constraint

            pct1 = dptk
            total1(1) = total1(1) + dptk*pnetx
            total1(2) = total1(2) + pct1
            type = bustyp(ntypu(kt))

            if (ntyp .eq. 5 .or. ntyp .eq. 12) then
              total2(1, 6) = total2(1, 6) + pnetx
              total2(2, 6) = total2(2, 6) + dptk*pnetx
            else
              temp1 = -dptk*pload
              total2(1, 1) = total2(1, 1) + pgen
              total2(2, 1) = total2(2, 1) + dptk*pgen
              total2(1, 2) = total2(1, 2) + pload
              total2(2, 2) = total2(2, 2) - dptk*pload
            endif
          endif
        endif
      enddo

      if (ps_flag) then
        do jt = 1, jphno
          it = lphtx(1,jt)
          if (it .ne. 0) then
            k = jphid(1, jt)
            m = jphid(2, jt)
            if (ordvlt .eq. 1) then
              kt = k
              mt = m
            else
              kt = inp2opt(k)
              mt = inp2opt(m)
            endif
            call getchr(1, id, jphid(3, jt))
            sect = jphid(4, jt)
            p = numbrn (k, m, id, sect)
            if (brtype(p) .eq. 4) p = brnch_nxt(p)
            call getlfo (p, 1, pin, qin)
            nbr = brnch_ptr(p)
            if (nbr .gt. 0) then
               angle = brnch(9,nbr)
            else
               nbr = -nbr
               angle = -brnch(9,nbr)
            endif
            call getchr (3, cbown, kbrnch(3,nbr))
            itx = iabs(it)
            k1 = txtie(1,itx)
            k2 = txtie(2,itx)
            ltc = txtie(7,itx)
            ltcx = iabs(ltc)
            lt = txtie(8,itx)
            ltx = iabs(lt) + ntot + ntopt
  
            if (ltc .ne. 0) then
              if (k1 .lt. k2) then
                 dptk = -dpt(1, ltx)
              else
                 dptk = dpt(1, ltx)
              endif
              if (k1 .eq. kt .and. k2 .eq. mt) then
              else
                 dptk = -dptk
              endif
              pct1 = dptk

              total2(1, 9) = total2(1, 9) + pin
              total2(2, 9) = total2(2, 9) + dptk*pin
              total1(1) = total1(1) + dptk*pin
              total1(2) = total1(2) + pct1
            else
              if (k1 .lt. k2) then
                dptk = -0.0174532 * bmva * dpt(1, ltx)
              else
                dptk = 0.0174532 * bmva * dpt(1, ltx)
              endif
              if (k1 .eq. kt .and. k2 .eq. mt) then
              else
                 dptk = -dptk
              endif
              pct1 = dptk

              total2(1, 8) = total2(1, 8) + angle
              total2(2, 8) = total2(2, 8) + dptk*angle
              total1(1) = total1(1) + dptk*angle
              total1(2) = total1(2) + pct1
            endif
          endif
        enddo
      endif
c
c     Second pass - Compute sensitivity linearization error
c
      tot2 = 0.0
      do i = 1, 10
        tot2 = tot2 + total2(2, i)
      enddo
      pct(1) = 100.0 * tot2 / totpij
      err = 100.0*abs((totpij-tot2)/totpij)

      write (*, 10171) tot2, totpij, err
10171 format (' * Sensitivity computed flow = ', f7.1, ' MW ', /
     &        ' * Actual flow               = ', f7.1, ' MW ', /
     &        ' * Linearization error       = ', f7.1, ' %')
      factor = 1.0
c
c     Re-initialize analysis arrays
c
      do i = 1, 10
        total2(1, i) = 0.0
        total2(2, i) = 0.0
      enddo

      do i=1,MAXOWN
         do j=1,10
            syst(j,i) = 0.0
         enddo
      enddo

      do i = 1, 4
        total1(i) = 0.0
      enddo

      do nbx = 1, ntot_alf
        nb = alf2inp(nbx)
        kt = inp2opt(nb)
        pload = (ploadu(kt)+inetr(kt)*vk)*bmva
        qload = (qloadu(kt)-ineti(kt)*vk)*bmva
        ntyp = ntypu(kt)

C       Skip passive d-c buses

        if ((ntyp .ne. 5 .and. ntyp .ne. 12) .or. kmlen(kt) .ne. 0) then
          vk = dsqrt(e(kt)**2+f(kt)**2)
          pgen = pnetu(kt)*bmva + pload
          qgen = qnetu(kt)*bmva + qload
          pnetx = pgen - pload
          qnetx = qgen - qload
          bskv = base(nb)*vk
          dptk =  -dpt(1, kt) * factor
          datk = 0.0
          dqtk =  -dpt(2, kt) * factor
          if (kvolt(kt) .ne. 0) dqtk = 0.0
          dvtk = 0.0
          jt = 0
          if (kt .gt. nbslck) then
            if (kspare(20) .eq. 1) then
C             
C             Check for Area interchange constraint.                   
C             
              i1 = iflag(kt)
              i2 = iflag(kt+1) - 1
              found = .false.
              do while (i1 .gt. 0 .and. i1 .le. i2 .and. .not. found) 
                if (jflag(1, i1) .eq. 3) then
                  found = .true.
                  dptk = 0.0
                  datk =  -dpt(1, kt) * factor
                  jt = jflag(2, i1)
                  xatot = area(2, jt) * bmva
                else
                  i1 = i1 + 1
                endif
              enddo
            endif
          else
            dptk = 0.0
            dqtk = 0.0
            datk = 0.0
          endif

C         Find Owner index. 

          iob = ownsch(owner(nb))
          if (ntyp .eq. 5 .or. ntyp .eq. 12) then
            syst(3,iob) = syst(3,iob) + dptk * pgen
            syst(4,iob) = syst(4,iob) + pgen
            syst(7,iob) = syst(7,iob) -dptk * pload
            syst(8,iob) = syst(8,iob) + pload
          else if (iob .gt. 0) then
            temp2 = -dptk * busdta(3,nb)
            syst(1,iob) = syst(1,iob) + dptk * busdta(8,nb)
            syst(2,iob) = syst(2,iob) + busdta(8,nb)
            syst(5,iob) = syst(5,iob) - dptk * busdta(3,nb)
            syst(6,iob) = syst(6,iob) + busdta(3,nb)
          endif
c
c         Loop through continuation buses
c
          ptr = kbsdta(15,nb) 
          do while (ptr .gt. 0) 
            call getchr (1, cbtype, kbctbl(8,ptr))
            call getchr (3, cbown, kbctbl(10,ptr))
            call getchr (2, cbyear, kbctbl(9,ptr))
            ioc = ownsch (cbown)
            if (ioc .gt. 0) then
              pload2 = bctbl(2,ptr)
              qload2 = bctbl(3,ptr)
              pgen2 = bctbl(6,ptr)
              qgen2 = bctbl(11,ptr)
              skcon2 = bctbl(4,ptr) * vk ** 2
              sksus2 = bctbl(5,ptr) * vk ** 2
              if (cbtype .eq. 'A') then
                if (cbyear .eq. '01' .or. cbyear .eq. '*I') then
                  pload2 = pload2*vk
                  qload2 = qload2*vk
                  if (cbown .eq. '***') then
                  else
                    pload2 = pload2 + skcon2
                    qload2 = qload2 - sksus2
                  endif
                elseif (cbyear .eq. '02' .or. cbyear .eq. '*P') then
                  if (cbown .eq. '***') then
                  else
                    pload2 = pload2 + skcon2
                    qload2 = qload2 - sksus2
                  endif
                endif
              elseif (cbyear .eq. '*I') then
                pload2 = pload2*vk
                qload2 = qload2*vk
                if (cbown .eq. '***') then
                else
                  pload2 = pload2 + skcon2
                  qload2 = qload2 - sksus2
                endif
              elseif (cbyear .eq. '*P') then
                if (cbown .eq. '***') then
                else
                  pload2 = pload2 + skcon2
                  qload2 = qload2 - sksus2
                endif
              endif
              temp3 = -dptk * pload2
              syst(1,ioc) = syst(1,ioc) + dptk * pgen2
              syst(2,ioc) = syst(2,ioc) + pgen2
              syst(5,ioc) = syst(5,ioc) -dptk * pload2
              syst(6,ioc) = syst(6,ioc) + pload2
            endif
            ptr = bctbl_nxt(ptr)
          enddo

          if (jt .ne. 0) then

C           Area slack constraint

            if (kvolt(kt) .eq. 0) then

C             SQ constraint

              pct1 = datk
              pct2 = dqtk

              total1(1) = total1(1) + datk*xatot
              total1(2) = total1(2) + pct1
              total1(3) = total1(3) + dqtk*qnetx
              total1(4) = total1(4) + pct2

              if (excel_flag) then
                write (outbuf, 10180) zone(nb), excel_sep, owner(nb), 
     &            excel_sep, excel_sep, bus(nb), base(nb), 
     &            arcnam(jt), excel_sep, datk, excel_sep, 
     &            datk*xatot, excel_sep, pct1, excel_sep, xatot,
     &            excel_sep, arcnam(jarzn(nb))
10180           format (t2, a2, a, a3, a, 'A ', a, a8, 1x, f6.1, 
     &            1x, '(', a, ')', t51, a, f8.2, t62, a, f8.1, 
     &            a, f8.1, t84, a, f7.1, ' MW', t110, a, a)
              else
                write (outbuf, 10182) zone(nb), owner(nb), bus(nb), 
     &            base(nb), arcnam(jt), datk, datk*xatot, pct1, xatot,
     &            arcnam(jarzn(nb))
10182           format (t2, a2, 3x, a3, 3x, 'A', 2x, a8, f6.1, 1x, 
     &            '(', a, ')', t51, f8.2, t62, f8.1, f8.1, t81, f7.1, 
     &            ' MW', t110, a)
              endif
              tol = amax1 (abs(pct1), abs(pct2))
              if (tol .gt. threshold) then
                 write (scrfil, '(a)') outbuf
              endif

              total2(1, 3) = total2(1, 3) + xatot
              total2(2, 3) = total2(2, 3) + datk*xatot
              total2(1, 4) = total2(1, 4) + qgen
              total2(2, 4) = total2(2, 4) - dqtk*qgen
              total2(1, 5) = total2(1, 5) + qload
              total2(2, 5) = total2(2, 5) - dqtk*qload

            else

C             SV constraint

              pct1 = datk

              total1(1) = total1(1) + datk*xatot
              total1(2) = total1(2) + pct1

              if (excel_flag) then
                write (outbuf, 10200) zone(nb), excel_sep, owner(nb), 
     &            excel_sep, excel_sep, bus(nb), base(nb), 
     &            arcnam(jt), excel_sep, datk, excel_sep, datk*xatot, 
     &            excel_sep, pct1, excel_sep, xatot, excel_sep, 
     &            arcnam(jarzn(nb))
10200           format (t2, a2, a, a3, a, 'A ', a, a8, 1x,
     &            f6.1, 1x, '(', a, ')', t51, a, f8.2, t62, a,
     &            f8.1, a, f8.1, t84, a, f7.1, ' MW', t110, 
     &            a, a)
              else
                write (outbuf, 10202) zone(nb), owner(nb), bus(nb), 
     &            base(nb), arcnam(jt), datk, datk*xatot, pct1, xatot,
     &            arcnam(jarzn(nb))
10202           format (t2, a2, 3x, a3, 3x, 'A', 2x, a8, f6.1, 1x, 
     &            '(', a, ')', t51, f8.2, t62, f8.1, f8.1, t81, f7.1, 
     &            ' MW', t110, a)
              endif

              tol = abs(pct1)
              if (tol .gt. threshold) then
                 write (scrfil, '(a)') outbuf
              endif

              total2(1, 3) = total2(1, 3) + xatot
              total2(2, 3) = total2(2, 3) + datk*xatot
            endif
          elseif (kvolt(kt) .eq. 0) then

C           PQ constraint

            pct1 = dptk
            pct2 = dqtk
            total1(1) = total1(1) + dptk*pnetx
            total1(2) = total1(2) + pct1
            total1(3) = total1(3) + dqtk*qnetx
            total1(4) = total1(4) + pct2
            type = bustyp(ntypu(kt))
            if (excel_flag) then
              write (outbuf, 10210) zone(nb), excel_sep, owner(nb), 
     &          excel_sep, type, excel_sep, bus(nb), 
     &          base(nb), excel_sep, dptk, excel_sep, 
     &          dptk*pnetx, excel_sep, pct1, excel_sep, pnetx, 
     &          excel_sep, arcnam(jarzn(nb))
10210         format (t2, a2, a, a3, a, 'B', a, a, a8, 1x, f6.1,
     &          t51, a, f8.2, t62, a, f8.1, a, f8.1, t84, 
     &          a, f7.1, ' MW', t110, a, a)
            else
              write (outbuf, 10212) zone(nb), owner(nb), type, bus(nb), 
     &          base(nb), dptk, dptk*pnetx, pct1, pnetx, 
     &          arcnam(jarzn(nb))
10212         format (t2, a2, 3x, a3, 3x, 'B', a, 1x, a8, f6.1, t51, 
     &          f8.2, t62, f8.1, f8.1, t81, f7.1, ' MW', t110, a)
            endif
            tol = amax1 (abs(pct1), abs(pct2))
            prt_flag = ((gen_flag .and. pnetx .gt. 0.01) .or. 
     &                  (.not. gen_flag))
            if ((tol .gt. threshold) .and. (prt_flag)) then
               write (scrfil, '(a)') outbuf
            endif

            if (ntyp .eq. 5 .or. ntyp .eq. 12) then
              total2(1, 6) = total2(1, 6) + pnetx
              total2(2, 6) = total2(2, 6) + dptk*pnetx
              total2(1, 7) = total2(1, 7) + qnetx
              total2(2, 7) = total2(2, 7) + dqtk*qnetx
            else
              temp1 = -dptk*pload
              total2(1, 1) = total2(1, 1) + pgen
              total2(2, 1) = total2(2, 1) + dptk*pgen
              total2(1, 2) = total2(1, 2) + pload
              total2(2, 2) = total2(2, 2) - dptk*pload
              total2(1, 4) = total2(1, 4) + qgen
              total2(2, 4) = total2(2, 4) + dqtk*qgen
              total2(1, 5) = total2(1, 5) + qload
              total2(2, 5) = total2(2, 5) - dqtk*qload
            endif
          else

C           PV constraint

            pct1 = dptk
            total1(1) = total1(1) + dptk*pnetx
            total1(2) = total1(2) + pct1
            type = bustyp(ntypu(kt))
            if (excel_flag) then
              write (outbuf, 10220) zone(nb), excel_sep, owner(nb), 
     &          excel_sep, type, excel_sep, bus(nb), 
     &          base(nb), excel_sep, dptk, excel_sep, dptk*pnetx, 
     &          excel_sep, pct1, excel_sep, pnetx, excel_sep, 
     &          arcnam(jarzn(nb))
10220         format (t2, a2, a, a3, a, 'B', a, a, a8, 1x, f6.1,
     &          t51, a, f8.2, t62, a, f8.1, a, f8.1, t84, 
     &          a, f7.1, ' MW', t110, a, a)
            else
              write (outbuf, 10222) zone(nb), owner(nb), type, bus(nb), 
     &          base(nb), dptk, dptk*pnetx, pct1, pnetx, 
     &          arcnam(jarzn(nb))
10222         format (t2, a2, 3x, a3, 3x, 'B', a, 1x, a8, f6.1, t51, 
     &          f8.2, t62, f8.1, f8.1, t81, f7.1, ' MW', t110, a)
            endif

            tol = abs(pct1)
            prt_flag = ((gen_flag .and. pnetx .gt. 0.01) .or. 
     &                  (.not. gen_flag))
            if ((tol .gt. threshold) .and. (prt_flag)) then
               write (scrfil, '(a)') outbuf
            endif

            if (ntyp .eq. 5 .or. ntyp .eq. 12) then
              total2(1, 6) = total2(1, 6) + pnetx
              total2(2, 6) = total2(2, 6) + dptk*pnetx
            else
              temp1 = -dptk*pload
              total2(1, 1) = total2(1, 1) + pgen
              total2(2, 1) = total2(2, 1) + dptk*pgen
              total2(1, 2) = total2(1, 2) + pload
              total2(2, 2) = total2(2, 2) - dptk*pload
            endif
          endif
        endif
      enddo

      do jt = 1, jphno
        it = lphtx(1,jt)
        if (it .ne. 0) then
          k = jphid(1, jt)
          m = jphid(2, jt)
          if (ordvlt .eq. 1) then
            kt = k
            mt = m
          else
            kt = inp2opt(k)
            mt = inp2opt(m)
          endif
          call getchr(1, id, jphid(3, jt))
          sect = jphid(4, jt)
          p = numbrn (k, m, id, sect)
          if (brtype(p) .eq. 4) p = brnch_nxt(p)
          call getlfo (p, 1, pin, qin)
          nbr = brnch_ptr(p)
          if (nbr .gt. 0) then
             angle = brnch(9,nbr)
          else
             nbr = -nbr
             angle = -brnch(9,nbr)
          endif
          call getchr (3, cbown, kbrnch(3,nbr))
          itx = iabs(it)
          k1 = txtie(1,itx)
          k2 = txtie(2,itx)
          ltc = txtie(7,itx)
          ltcx = iabs(ltc)
          lt = txtie(8,itx)
          ltx = iabs(lt) + ntot + ntopt

          if (ltc .ne. 0) then
            if (k1 .lt. k2) then
               dptk = -dpt(1, ltx) * factor
            else
               dptk = dpt(1, ltx) * factor
            endif
            if (k1 .eq. kt .and. k2 .eq. mt) then
            else
               dptk = -dptk
            endif
            pct1 = dptk

            if (excel_flag) then
              write (outbuf, 10330) zone(k), excel_sep, cbown, 
     &          excel_sep, excel_sep, bus(k), base(k), bus(m), 
     &          base(m), id, sect, excel_sep, dptk, excel_sep, 
     &          dptk*pin, excel_sep, pct1, excel_sep, pin, excel_sep, 
     &          angle, excel_sep, arcnam(jarzn(k))
10330         format (t2, a2, a, a3, a, 'RP', a, a8, 1x, f6.1, 1x, 
     &          a8, 1x, f6.1, 1x, a1, 1x, i1, t51, a, f8.2, t62, 
     &          a, f8.1, a, f8.1, t84, a, f7.1, ' MW @ ', a,
     &          f7.1, ' degrees', t110, a, a)
            else
              write (outbuf, 10332) zone(k), cbown, bus(k), base(k), 
     &          bus(m), base(m), id, sect, dptk, dptk*pin, pct1, pin, 
     &          angle, arcnam(jarzn(k))
10332         format (t2, a2, 3x, a3, 3x, 'RP', 1x, a8, f6.1, 1x, a8,
     &          f6.1, 1x, a1, 1x, i1, t51, 1x, f8.2, t62, f8.1, f8.1,
     &          t81, 1x, f7.1, ' MW @ ', f7.1, ' degrees', t110, 
     &          1x, a)
            endif

            tol = abs(pct1)
            if (tol .gt. threshold) then
               write (scrfil, '(a)') outbuf
            endif

            total2(1, 9) = total2(1, 9) + pin
            total2(2, 9) = total2(2, 9) + dptk*pin
            total1(1) = total1(1) + dptk*pin
            total1(2) = total1(2) + pct1
          else
            if (k1 .lt. k2) then
              dptk = -0.0174532 * bmva * dpt(1, ltx) * factor
            else
              dptk = 0.0174532 * bmva * dpt(1, ltx) * factor
            endif
            if (k1 .eq. kt .and. k2 .eq. mt) then
            else
               dptk = -dptk
            endif
            pct1 = dptk
            if (excel_flag) then
              write (outbuf, 10340) zone(k), excel_sep, cbown, 
     &          excel_sep, excel_sep, bus(k), base(k), bus(m), 
     &          base(m), id, sect, excel_sep, dptk, excel_sep, 
     &          dptk*angle, excel_sep, pct1, excel_sep, angle, 
     &          excel_sep, arcnam(jarzn(k))
10340         format (t2, a2, a, a3, a, 'TP', a, a8, 1x, f6.1, 
     &          1x, a8, 1x, f6.1, 1x, a1, 1x, i1, t51, a, f8.2, t62, 
     &          a, f8.1, a, f8.1, t84, a, f7.1, ' Degrees',
     &          t110, a, a)
            else
              write (outbuf, 10341) zone(k), cbown, bus(k), base(k), 
     &          bus(m), base(m), id, sect, dptk, dptk*angle, pct1, 
     &          angle, arcnam(jarzn(k))
10341         format (t2, a2, 3x, a3, 3x, 'TP', 1x, a8, f6.1, 1x, a8, 
     &          f6.1, 1x, a1, 1x, i1, t51, f8.2, t62, f8.1, f8.1, t81, 
     &          f7.1, ' Degrees', t110, a)
            endif
            tol = abs(pct1)
            if (tol .gt. threshold) then
               write (scrfil, '(a)') outbuf
            endif
            total2(1, 8) = total2(1, 8) + angle
            total2(2, 8) = total2(2, 8) + dptk*angle
            total1(1) = total1(1) + dptk*angle
            total1(2) = total1(2) + pct1
          endif
        endif
      enddo

      if (.not. excel_flag) then
        write (scrfil, 10270) 
10270   format (t62, '  ------  ------')

        write (scrfil, 10280) total1(1), total1(2)
10280   format (t2, 'Total flow:', t62, f8.1, f8.1)

        write (scrfil, 10282) totpij
10282   format (t2, 'Actual flow: ', t62, f8.1)

        write (scrfil, '(1x)')

        ptr = brn_ptr(1)
        k1 = kx(ptr)
        k2 = ky(ptr)
        id = brid(ptr)
        sect = brsect(ptr)

        write (scrfil, 10286) bus(k1), base(k1), bus(k2), base(k2), id
10286   format (t2, 'Summary of Line Analysis for branch: ', a8, f7.1, 
     &   1x, a8, f7.1, 1x, a1)

        write (scrfil, '(1x)')

        write (scrfil, 10360)
10360   format (t2, 'Summary:', t12, 'Computed Pij', t30, 
     &   'Component source')

        write (scrfil, 10380)
10380   format (t12, ' (MW)      (%)')

        write (scrfil, '(1x)')

        pct(1) = 100.0*total2(2, 3)/totpij
        write (scrfil, 10388) total2(2, 3), pct(1), total2(1, 3)
10388   format (t9, f8.1, f9.1, t30, f8.1, 
     &    ' MW total area export')

        pct(1) = 100.0*total2(2, 1)/totpij
        write (scrfil, 10390) total2(2, 1), pct(1), total2(1, 1)
10390   format (t9, f8.1, f9.1, t30, f8.1, 
     &   ' MW total system generation')

        pct(1) = 100.0*total2(2, 4)/totpij
        write (scrfil, 10400) total2(2, 4), pct(1), total2(1, 4)
10400   format (t9, f8.1, f9.1, t30, f8.1, 
     &   ' MVAR total system generation')

        pct(1) = 100.0*total2(2, 2)/totpij
        write (scrfil, 10410) total2(2, 2), pct(1), total2(1, 2)
10410   format (t9, f8.1, f9.1, t30, f8.1, 
     &   ' MW total system load')

        pct(1) = 100.0*total2(2, 5)/totpij
        write (scrfil, 10420) total2(2, 5), pct(1), total2(1, 5)
10420   format (t9, f8.1, f9.1, t30, f8.1, 
     &    ' MVAR total system load')

        pct(1) = 100.0*total2(2, 6)/totpij
        write (scrfil, 10430) total2(2, 6), pct(1), total2(1, 6)
10430   format (t9, f8.1, f9.1, t30, f8.1, 
     &    ' MW total d-c converters')

        pct(1) = 100.0*total2(2, 7)/totpij
        write (scrfil, 10440) total2(2, 7), pct(1), total2(1, 7)
10440   format (t9, f8.1, f9.1, t30, f8.1, ' MVAR total d-c converters'
     &    )

        pct(1) = 100.0*total2(2, 8)/totpij
        write (scrfil, 10450) total2(2, 8), pct(1), total2(1, 8)
10450   format (t9, f8.1, f9.1, t30, f8.1,
     &   ' degrees total fixed phase shift')

        pct(1) = 100.0*total2(2, 9)/totpij
        write (scrfil, 10460) total2(2, 9), pct(1), total2(1, 9)
10460   format (t9, f8.1, f9.1, t30, f8.1,
     &   ' MW total LTC phase shift')

        write (scrfil, 10470)
10470   format (t9, '  ------    -----')

      endif
      tot2 = 0.0
      do i = 1, 10
        tot2 = tot2 + total2(2, i)
      enddo
      pct(1) = 100.0 * tot2 / totpij
      err = 100.0*abs((totpij-tot2)/totpij)
      if (.not. excel_flag) then
        write (scrfil, 10480) tot2, pct(1)
10480   format (t2, 'Total:', t9, f8.1, f9.1)

        write (scrfil, 10482) totpij
10482   format (t2, 'Actual:', t9, f8.1)

        write (scrfil, 10484) err
10484   format (t2, 'Error:', t9, f8.1, ' %')

      endif
      write (*, 10486) tot2, totpij, err
10486 format (' * Sensitivity computed flow = ', f7.1, ' MW ', /
     &        ' * Actual flow               = ', f7.1, ' MW ', /
     &        ' * Linearization error       = ', f7.1, ' %')

      if (.not. excel_flag) then
        write (scrfil, '(1x)')

        write (scrfil, 10488) bus(k1), base(k1), bus(k2), base(k2), id,
     &    totpij, totqij
10488   format (t2, 'Ownership Analysis for branch: ', a8, f7.1, 1x, 
     &    a8, f7.1, 1x, a1, ' Flow = ', f7.1, ' MW, ', f7.1, ' MVAR)')

        write (scrfil, 10490)
10490   format (t2, 'Ownership /----- Owner Net ------/ /----- Generatio
     &n -----/ /-------- Load --------/ /------D-C System------/')

        write (scrfil, 10492)
10492   format (t2, '            Pij             Owner    Pij           
     &  Owner    Pij             Owner    Pij             Owner')

        write (scrfil, 10494)
10494   format (t2, '            Flow    (%)     Total    Flow    (%)   
     &  Total    Flow    (%)     Total    Flow    (%)     Total')

        write (scrfil, 10496)
10496   format (t2, '            (MW)            (MW)     (MW)          
     &  (MW)     (MW)            (MW)     (MW)            (MW)')

        write (scrfil, '(1x)')

        do i = 1, 12
           total1(i) = 0.0
        enddo
        totx = 0.0
        toty = 0.0
        do io = 1, numown
           jo = alf2own(io)
           do j = 1, 8
             pct(j) = 100.0 * syst(j,jo) / totpij
           enddo
           tot1 = syst(1,jo) + syst(3,jo) + syst(5,jo) + syst(7,jo)
           tot2 = syst(2,jo) + syst(4,jo) - syst(6,jo) - syst(8,jo)
           pct1 = 100.0 * tot1 / totpij
           totx = totx + tot1
           toty = toty + tot2
           do i = 1, 8
             total1(i) = total1(i) + syst(i,jo)
           enddo
           write (scrfil, 10498) owner_o(jo), tot1, pct1, tot2, 
     &       syst(1,jo), pct(1), syst(2,jo), syst(5,jo), pct(5), 
     &       syst(6,jo), syst(3,jo)+syst(7,jo), pct(3)+pct(7), 
     &       syst(4,jo)-syst(8,jo)
10498      format (t5, a3, t9, f9.1, f8.1, f9.1,  
     &       4(f9.1, f8.1, f9.1))
        enddo

        write (scrfil, 10500) 
10500   format (t9,  '   ------  ------  -------', 
     &          t35, '   ------  ------  -------', 
     &          t61, '   ------  ------  -------',
     &          t87, '   ------  ------  -------')

        pct(1) = 100.0 * total1(1) / totpij
        pct(3) = 100.0 * (total1(3) + total1(7)) / totpij
        pct(5) = 100.0 * total1(5) / totpij
        pct1 = 100.0 * totx / totpij
        write (scrfil, 10502) totx, pct1, toty, total1(1), pct(1), 
     &    total1(2), total1(5), pct(5), total1(6), total1(3)+total1(7), 
     &    pct(3), total1(4)-total1(8)
10502   format (t2, 'Total', t9, f9.1, f8.1, f9.1, 
     &    4(f9.1, f8.1, f9.1))
      endif

      return
      end
