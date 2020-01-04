C    %W% %G%
      subroutine svssol
 
c     This subroutine solves the static var source differential
c     equations.  It is called by cntrl.
 
      integer ksv, ibus, ire, jsv1, jsv2, ndiv, itr
      real    x1, x1a, x2, x3, x4, x5, x6, sig1n, ctr,
     &        cti, gt, bt,  c1r, c1i, c2r, c2i, 
     &        presa, pasta, delan, delfeq, delvt, dels1, 
     &        dels2, eterm, sig1, sig2, verr 

c     solve supplemental control in double precision
      double precision x1s, x2s, x3s, x4s, x5s, x6s

      include 'tspinc/params.inc'
      include 'tspinc/outaux.inc'

      include 'tspinc/busvolt.inc'
c     include 'tspinc:wstequ.inc'

      include 'tspinc/vrgov.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/svs.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/spare1.inc'

      data x1s, x2s, x3s, x4s, x5s, x6s / 6 * 0.0 /
      data sig1n, sig2n / 2 * 0.0 /


c     get index to svs tables
      ksv = igndta(2, ispf)

c     if svs output has been frozen then do not calculate states

      if (isvsfz(ksv) .eq. 1) then
        angl(ispf) = angl(1)
        return
      endif

c     get bus number of input bus
      ibus = isvno(ksv)
      if (iremte(ksv) .ne. 0) ibus = iremte(ksv)


c     calculate svs input, |Vt|
      svsetn(ksv) = sqrt(eyr(ibus)*eyr(ibus)+eyi(ibus)*eyi(ibus))

c     calculate supplemental signals if there are any
      if (ispcde(ksv) .ne. 0) then

        if (isupb(ksv) .ne. 0) then
c         subtype d, e, and f logic

          if (isupb(ksv) .eq. 2) then
c           megavar input option (e)
            ibus  = isvno(ksv)
            sig1n = bsvs(ksv)*(eyr(ibus)*eyr(ibus)+eyi(ibus)*eyi(ibus))

          elseif (isupb(ksv) .eq. 3) then
c           current input option (f)
            ibus  = isvno(ksv)
            ctr   = bsvs(ksv)*eyi(ibus)
            cti   =  - bsvs(ksv)*eyr(ibus)
            sig1n = sqrt(ctr*ctr+cti*cti)

          else
c           voltage magnitude input option (d)
            ire   = iremte(ksv)
            sig1n = sqrt(eyr(ire)*eyr(ire)+eyi(ire)*eyi(ire))
          endif
        endif

        if (isupt(ksv) .ne. 0) then
          if (isupt(ksv) .eq. 2) then
c           supplemental signal options a,b,c

c           line power option (b)
            gt   = svgio(ksv) + svgij(ksv)
            bt   = svbio(ksv) + svbij(ksv)

            jsv1 = irmote(ksv)
            jsv2 = jrmote(ksv)

            c1r  = eyr(jsv1)*gt - eyi(jsv1)*bt
            c1i  = eyr(jsv1)*bt + eyi(jsv1)*gt
            c2r  = eyr(jsv2)*svgij(ksv) - eyi(jsv2)*svbij(ksv)
            c2i  = eyr(jsv2)*svbij(ksv) + eyi(jsv2)*svgij(ksv)
            ctr  = c1r - c2r
            cti  = c1i - c2i

            sig2n = eyr(jsv1)*ctr + eyi(jsv1)*cti


          elseif (isupt(ksv) .eq. 3) then

c           delta frequency option (c)
            ire = irmote(ksv)
            if (eyr(ire) .eq. 0.0 .and. eyi(ire) .eq. 0.0) then
              sig2n = sigo2(ksv)
            else
              presa = atan2(eyi(ire), eyr(ire))
              if (idsw .eq. 7) then
                sig2n = sigo2(ksv)
                if (lppwr .eq. 0) svango(ksv) = presa
              else
                pasta = svango(ksv)
                delan = presa - pasta
                if (lppwr .eq. 0) svango(ksv) = presa
                if (delan .lt. -3.14159) delan = delan + 6.28318
                if (delan .gt. 3.14159) delan = delan - 6.28318
                delfeq = delan/edt
                sig2n = delfeq/6.28318
              endif
            endif
          else

c           acceleration power option (a)
            sig2n = govpwr(irbus(ksv)) - genp(irbus(ksv))
          endif
        endif
      endif



c     start lppwr=0 case
      if (lppwr.eq.0) then

c       assign values to be used in the sub time loop
        ndiv    = edt/DTSVS
        delvt   = ( etermo(ksv) - etermoo(ksv) )*DTSVS/edt
        dels1   = ( sigo1(ksv) - sigoo1(ksv) ) *DTSVS/edt
        dels2   = ( sigo2(ksv) - sigoo2(ksv) ) *DTSVS/edt
        

c       start sub time step loop
        do itr = 0, ndiv-1

c         calculated extrapolation of |Vt| and Vscs
          if (idsw.ge.3.and.idsw.le.5.and.itr.ne.0) then
c           at network discontinuity, use new inputs
            eterm = svsetn(ksv)
            sig1  = sig1n
            sig2  = sig2n
          else
c           extrapolate input from previous values
            eterm = etermo(ksv) + itr*delvt
            sig1  = sigo1(ksv)  + itr*dels1
            sig2  = sigo2(ksv)  + itr*dels2
          endif

c         calculate supplemental states if there are any
          if (ispcde(ksv) .ne. 0) then

c           clear any value in x2s or x4s
            x2s = 0
            x4s = 0

            if (isupb(ksv) .ne. 0) then

c             block logic for options d,e,f
              x3s = (bs10(ksv)+cks2(ksv)*sig1)/as10(ksv)
              x4s = (bs1112(ksv)+x3s*as11(ksv))/as12(ksv)

c             past values
              bs10(ksv)   = (as10(ksv)-2.)*x3s + cks2(ksv)*sig1
              bs1112(ksv) = (as12(ksv)-2.)*x4s - (as11(ksv)-2.)*x3s

            endif

            if (isupt(ksv) .ne. 0) then
c             block logic of options a,b,c
              x1s = (bs7(ksv)+cks1(ksv)*sig2)/as7(ksv)
              x2s = (bs89(ksv)+x1s*as8(ksv))/as9(ksv)

c             past values
              bs7(ksv)  = cks1(ksv)*sig2 + x1s*(as7(ksv)-2.0)
              bs89(ksv) = x2s*(as9(ksv)-2.) - x1s*(as8(ksv)-2.)
            endif

c           block 1314 logic
            x5s = x2s + x4s
            x6s = (bs1314(ksv) + x5s*as13(ksv))/as14(ksv)

c           past values
            bs1314(ksv) =   x6s*(as14(ksv)-2.) - x5s*as13(ksv)

c           gain multiplication and limit
            vscs(ksv) = cks3(ksv)*x6s
            vscs(ksv) = amax1(amin1(vscsmx(ksv),vscs(ksv)),-vscsmx(ksv))
          endif

c         start svs calculation

c         calculate filter state
          x1 = (bsv1(ksv)+eterm)/as1(ksv)

c         past value
          bsv1(ksv) = (as1(ksv)-2.0)*x1 + eterm

c         sum incoming signals
          verr = svref(ksv) - x1
          x1a = verr + vscs(ksv)

c         limit here if type 'W'
          if (svstyp(ksv).eq.2) then
            x2 = amin1(amax1(x1a, vemin(ksv)), vemax(ksv))
          else
            x2 = x1a
          endif

c         calculate regulator states
          x3 = (x2*as2(ksv)+bsv2(ksv))/as3(ksv)

c         limit here if type 'N'
          if (svstyp(ksv).eq.1) 
     &      x3 = amin1(amax1(x3, vmin(ksv)), vmax(ksv))

          x4 = (x3*as4(ksv)+bsv3(ksv))/as5(ksv)

c         limit here if type 'N'
          if (svstyp(ksv).eq.1)
     &      x4 = amin1(amax1(x4, vemin(ksv)), vemax(ksv))

c         past values
          bsv2(ksv) =  (as3(ksv)-2.0)*x3 - (as2(ksv)-2.0)*x2
          bsv3(ksv) =  (as5(ksv)-2.0)*x4 - (as4(ksv)-2.0)*x3

c         gain multiplication
          br = x4*cksvs(ksv)

c         process fast override
          if (dv(ksv).ge.2.0) then
            brp(ksv) = br
          else
            if (verr.ge.dvlo(ksv)) then
              brp(ksv) = bpmax(ksv) + cksd(ksv)*(verr - dv(ksv))
            else if (verr.gt.dvhi(ksv).and.verr.lt.dvlo(ksv)) then
              brp(ksv) = br
            else if (verr.le.dvhi(ksv)) then
              brp(ksv) = bpmin(ksv)
            endif
          endif

c         process thyristor delay
          b = (bsv4(ksv)+brp(ksv))/as6(ksv)
          b = amin1(amax1(b,bmin(ksv)), bmax(ksv))

c         past value
          bsv4(ksv) = (as6(ksv)-2.0)*b + brp(ksv)

c         csw. temp test. assign spare points
c         sparpt(1) = sig2n
c         sparpt(2) = x1s
c         sparpt(3) = x2s 
c         sparpt(4) = x5s
c         sparpt(5) = x6s
c         sparpt(6) = vscs(ksv)
c         sparpt(7) = x1a
c         sparpt(8) = x2
c         sparpt(9) = x3

c       end sub time step loop
        enddo

c       assign previous values for use in extrapolation
        etermoo(ksv) = etermo(ksv)
        sigoo1(ksv)  = sigo1(ksv)
        sigoo2(ksv)  = sigo2(ksv)

c     end lppwr=0 case
      endif

c     calculate supplemental states if there are any
      if (ispcde(ksv) .ne. 0) then

c       clear any value in x2s or x4s
        x2s = 0
        x4s = 0

        if (isupb(ksv) .ne. 0) then
c         block logic for options d,e,f
          x3s = (bs10(ksv)+cks2(ksv)*sig1n)/as10(ksv)
          x4s = (bs1112(ksv)+x3s*as11(ksv))/as12(ksv)
        endif

        if (isupt(ksv) .ne. 0) then
c         block logic of options a,b,c
          x1s = (bs7(ksv)+cks1(ksv)*sig2n)/as7(ksv)
          x2s = (bs89(ksv)+x1s*as8(ksv))/as9(ksv)
        endif

c       block 1314 logic
        x5s = x2s + x4s
        x6s = (bs1314(ksv)+x5s*as13(ksv))/as14(ksv)

c       gain multiplication and limit
        vscs(ksv) = cks3(ksv)*x6s
        vscs(ksv) = amax1(amin1(vscsmx(ksv),vscs(ksv)),-vscsmx(ksv))
      endif

c     calculate filter state
      x1 = (bsv1(ksv)+svsetn(ksv))/as1(ksv)

c     sum incoming signals
      verr = svref(ksv) - x1
      x1a = verr + vscs(ksv)

c     limit here if type 'W'
      if (svstyp(ksv).eq.2) then
        x2 = amin1(amax1(x1a, vemin(ksv)), vemax(ksv))
      else
        x2 = x1a
      endif

c     calculate regulator states
      x3 = (x2*as2(ksv)+bsv2(ksv))/as3(ksv)

c     limit here if type 'N'
      if (svstyp(ksv).eq.1) 
     &  x3 = amin1(amax1(x3, vmin(ksv)), vmax(ksv))

      x4 = (x3*as4(ksv)+bsv3(ksv))/as5(ksv)

c     limit here if type 'N'
      if (svstyp(ksv).eq.1)
     &  x4 = amin1(amax1(x4, vemin(ksv)), vemax(ksv))

c     gain multiplication
      br = x4*cksvs(ksv)

c     process fast override
      if (dv(ksv).ge.2.0) then
        brp(ksv) = br
      else
        if (verr.ge.dvlo(ksv)) then
          brp(ksv) = bpmax(ksv) + cksd(ksv)*(verr - dv(ksv))
        else if (verr.gt.dvhi(ksv).and.verr.lt.dvlo(ksv)) then
          brp(ksv) = br
        else if (verr.le.dvhi(ksv)) then
          brp(ksv) = bpmin(ksv)
        endif
      endif

c     process thyristor delay
      b = (bsv4(ksv)+brp(ksv))/as6(ksv)

c     limit output
      b = amin1(amax1(b,bmin(ksv)), bmax(ksv))

c     add bias to output
      bsvs(ksv) = b + svsbias(ksv)

c     switch in shunt if verr >= dv2 for more than tdelay cycles.
      if (verr.ge.dv2(ksv)) then
        if (timeover(ksv).ge.tdelay(ksv)) then
          bsvs(ksv) = bsvs(ksv) + bshunt(ksv)
        endif
        timeover(ksv) = timeover(ksv) + edt 
      else
        timeover(ksv) = 0.0
      endif

c     assign previous values
      etermo(ksv)  = svsetn(ksv)
      sigo1(ksv)   = sig1n
      sigo2(ksv)   = sig2n

c     assign output
      govpwr(ispf) = bsvs(ksv)/100.0

c     store other outputs
      vfldtn(1, ispf) = x2
      supout(ispf)    = x3
      regout(ispf)    = x4
      angl(ispf)      = angl(1)

      return
      end
