C    %W% %G%
      subroutine solgovn (igr, mgvecs, iecs, idm, igovc)
      character*1 idm
c      
c     This subroutine solves the governor differential equations.
c     It serves as the control module for the solution
c      
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/search.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/brake1.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbc.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/ecstbj.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/gentblc.inc'
      include 'tspinc/dateq.inc'
      include 'tspinc/lshed1.inc'
      include 'tspinc/busdta.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/amorts.inc'
      include 'tspinc/prate.inc'
      include 'tspinc/vtchkc.inc'
      include 'tspinc/busvolt.inc'

      common /vregl/ em,vmqt,vmdt
  
      include 'tspinc/comvar.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/spare1.inc'

      common /ckpnt/ dnxmin,d13
      equivalence (creg(9), dr), (creg(21), rcex), (creg(23),
     1    xcex), (creg(3), vref), (creg(27), vco), (creg(7), cckp,
     2    ckpr), (creg(28), theta, ckpi), (creg(20), xl), (creg(10),
     3    ckc), (creg(11), v1max), (creg(13), va), (creg(30), vi),
     4    (creg(15), hcb), (creg(16), ckj), (creg(17), db),
     5    (creg(18), dc), (creg(19), vgmax), (creg(1), vrmax),
     6    (creg(2), vrmin), (creg(22), vr), (creg(8), cka), (creg(24),
     7    ckg), (creg(25), da), (creg(26), hba), (creg(4), efdmax),
     8    (creg(5), cki), (creg(29), efd), (creg(6), hbr),
     9    (creg(12), v1min), (creg(14),vfldo)

      include 'tspinc/deltfq.inc'

      common /govminmax/ govmn(MAXGEN), govmx(MAXGEN)

      equivalence (aeps,ar), (aes,brs,af1), (bes,bf1), (ba1s,br),
     1            (baps,a1), (bfs,a2), (ae, vfdo), (aep, bfx), 
     2            (be, ba1x), (bf1, bapx)
      dimension istem(40)

      equivalence (istem,isorti)
      equivalence(csupp(26),x5o)

      dimension ktemp1(9), temp1(9)
      equivalence (ktemp1,temp1), (pgo,pgor), (sat,rt), (sater,dtc),
     1            (isg2,istp), (csupp(27),ivcsw)

      save
       
      call redecs (cgov,mgvecs,igv)

      if (mgov.eq.1) then
c -------------------------- Process GG Governor --------------------
        if (lppwr.eq.0) then

c --      update states using converged input and output
          st1 =(b1 - sgkp1*wnow*a2p)/a1p
          st3 = pgo + st1
          if (st3.gt.pmax) st3=pmax
          if (st3.lt.0.) st3=0.0
          st4 = (b3 + st3)/a3
          st5 = (b4 + st4)/a4
          st6 = (b5 + st5*a6)/a5

c --      update time constant variables if time step changed
          if (al .ne. 0.0) then
            a3    = a3*tfac - tfac + 1.
            a4    = a4*tfac - tfac + 1.
            a1p   = a1p*tfac - tfac + 1.
            a2p   = a2p*tfac - tfac + 1.
            a6    = a6 *tfac - tfac + 1.
            a5    = a5 *tfac - tfac + 1.
          endif

c --      update past state values
          b1 = sgkp1*wnow*(a2p - 2.) + st1*(a1p - 2.)
          b3 = st4*(a3-2.) + st3
          b4 = st5*(a4-2.) + st4
          b5 = st6*(a5-2.) - st5*(a6 - 2.)

          c1 = a6/(a3*a4*a5)
          c2 = b3*c1 + b4*c1*a3 + b5/a5

          govmx(ispf) = pmax*c1 + c2
          govmn(ispf) = c2

          a1gov = (-sgkp1*a2p*c1)/a1p
          a2gov = c2 + pgo*c1 + (b1*c1)/a1p
        endif

        govpow = a1gov*wnew + a2gov
        if (govpow.gt.govmx(ispf)) govpow = govmx(ispf)
        if (govpow.lt.govmn(ispf)) govpow = govmn(ispf)

      else if (mgov.eq.2) then
c -------------------------- Process GH Governor --------------------

        if (lppwr.eq.0) then

c --      update states using converged input and output
          st5 = govpwr(ispf)
          st3 = (b5 - a4*st5)/(2.0*a4 - 3.)
          if (st3.gt.pmax) st3 = pmax
          if (st3.lt.0.0) st3 = 0.0
          st4 = (b4 + a5*st3)/a3
          errsig = pgor - r*st3 - st4 - sgkp1*wnow
          st1 = a1p*(errsig + b1)/a2p
          if (st1 .gt. velmax) st1 = velmax
          if (st1 .lt. velmin) st1 = velmin

c --      update time constant variables if time step changed
          if (al .ne. 0.0) then
            a2p = a2p*tfac - tfac + 1.
            a3  = a3 *tfac - tfac + 1.
            a4  = a4 *tfac - tfac + 1.
            a5  = a5 *tfac
            a1gov  = -sgkp1/(120.0/edt*a2p/a1p + r + a5/a3)
            a1turb = -(2*a4 - 3.0)/a4
          endif

c --      update past state values
          if (st1.lt.velmax.and.st1.gt.velmin) then
             b1 = st1*(a2p-2.0)/a1p + errsig
          else
             b1 = st1*(a2p-1.0)/a1p
          endif 
          if (st3.lt.pmax.and.st3.gt.0.0) then
            b3 = st1 + st3*120.0/edt
          else
            b3 = st3*120.0/edt
          endif 
          b4 = st4*(a3-2.) - st3*a5
          b5 = st3*(2.0*a4-1.) + st5*(a4-2.)

          a2gov = (pgor - b4/a3 + b1 + b3*a2p/a1p) /
     &            (120.0/edt*a2p/a1p + r + a5/a3)
          a2turb = b5/a4

          govmx(ispf) = pmax
          govmn(ispf) = 0.0
          st3h = (b3 + velmax)*edt/120.0
          st3l = (b3 + velmin)*edt/120.0
          if (govmx(ispf).gt.st3h) govmx(ispf) = st3h
          if (govmn(ispf).lt.st3l) govmn(ispf) = st3l

        endif

        govpow = a1gov*wnew + a2gov
        if (govpow.gt.govmx(ispf)) govpow = govmx(ispf)
        if (govpow.lt.govmn(ispf)) govpow = govmn(ispf)
        govpow = a1turb*govpow + a2turb

      else if (mgov.eq.3) then
c -------------------------- Process GC Governor --------------------

        if (lvref .ne. 0) then
          vhd = eyri(ispf)
          vhq = eyii(ispf)
          if (xt .gt. 0.0 .and. rt .gt. 0.0) then
            curd = oid
            curq = oiq
            vld  = vhd + rt*curd - xt*curq
            vlq  = vhq + rt*curq + xt*curd
          endif
          vmt = sqrt(vld*vld + vlq*vlq)
          vmt = sqrt(vhd*vhd + vhq*vhq)
        else
          vmt = vmagt(ispf)
        endif

        wnow  = angp1(ispfh)
        wnew  = angp2(ispfh)
        wnewp = wnew - angp2(ispfl)

        if (lppwr.eq.0) then

c --      update states using converged input and output
          st1 = (b1 - sgk*wnow)/a1p
          st3 = pgo + st1
          if (st3.gt.pmax) st3 = pmax
          if (st3.lt.0.)   st3 = 0.0
          st4 = (b3 + st3)/a3
          st5 = (b4 + st4)/a4
          st6 = (b5 + st5*a6)/a5

c --      update time constant variables if time step changed
          if (al .ne. 0.0) then
            a3    = a3 *tfac - tfac + 1.
            a4    = a4 *tfac - tfac + 1.
            a1p   = a1p*tfac - tfac + 1.
            a6    = a6 *tfac - tfac + 1.
            a5    = a5 *tfac - tfac + 1.
          endif

c --      update past state values
          b1 = -sgk*wnow + st1*(a1p - 2.)
          b3 =  st4*(a3 - 2.) + st3
          b4 =  st5*(a4 - 2.) + st4
          b5 =  st6*(a5 - 2.) - st5*(a6 - 2.)

          c1 = a6/(a3*a4*a5)
          c2 = b3*c1 + b4*c1*a3 + b5/a5

          govmx(ispf) = pmax*c1 + c2
          govmn(ispf) = c2

          a1gov = (-sgk*c1)/a1p
          a2gov = c2 + pgo*c1 + (b1*c1)/a1p

        endif

        govpow = a1gov*wnew + a2gov
        if (govpow.gt.govmx(ispf)) govpow = govmx(ispf)
        if (govpow.lt.govmn(ispf)) govpow = govmn(ispf)
        govpow = govpow - a2p*vmt*vmt*wnewp

      else if (mgov.eq.4) then
c -------------------------- Process GW Governor --------------------
        if (lppwr.eq.0) then

c --      update states using converged input and output
          st1 = (b1 - sgkp1*wnow*a2p)/a1p
          st2 = (st1 + b3)/a3
          st3 =  st2 + pgo
          if (st3.gt.pmax) st3 = pmax
          if (st3.lt.pmin) st3 = pmin
          st4 = (st3 + b4)/a4
          st5 = (st4 + b5)/a5

c --      update time constant variables if time step changed
          if (al.ne.0.0) then
            a1p    = a1p*tfac - tfac + 1.
            a2p    = a2p*tfac - tfac + 1.
            a3     = a3 *tfac - tfac + 1.
            a4     = a4 *tfac - tfac + 1.
            a5     = a5 *tfac - tfac + 1.
            a1gov  = -sgkp1*a2p/(a1p*a3)
            a1turb = (3./a5-2.)/a4
          endif

c --      update past state values
          b1 = sgkp1*wnow*(a2p - 2.) + st1*(a1p - 2.)
          b3 = st1 + (a3 - 2.)*st2
          b4 = st3 + st4*(a4 - 2.)
          b5 = st4 + st5*(a5 - 2.)

          a2gov  = pgo + b3/a3 + b1/(a1p*a3)
          a2turb = 3.*b5/a5 + (b4/a4)*(3./a5 - 2.)

        endif

        govpow = a1gov*wnew + a2gov
        if (govpow.gt.pmax) govpow=pmax
        if (govpow.lt.pmin) govpow=pmin
        govpow = a1turb*govpow + a2turb

      else if (mgov.ge.5) then
c -------------------------- Process GS Governor --------------------
        if (lppwr.eq.0) then

c --      update states using converged input and output
          st1 = (b1 - sgkp1*wnow*a2p)/a1p

          st3 = a1gov*wnow + a2gov
          if (st3.gt.govmx(ispf)) st3 = govmx(ispf) 
          if (st3.lt.govmn(ispf)) st3 = govmn(ispf) 

          st2 = (pgo + st1 - st3)/((a3 - 1.0)*(ddt2/2.0))
          if (st2.gt.velmax) st2=velmax
          if (st2.lt.velmin) st2=velmin

          st3 = (st2 + b3)*ddt2/2.
          if (st3.gt.pmax) st3=pmax
          if (st3.lt.pmin) st3=pmin

          st4=(st3+b4)/a4

          if (mgov.ge.6) then
            st5 = (st4 + b5 )/a5
            st6 = (st5 + b6p)/a6p
            st7 = (st6 + b7 )/a7
          endif

c --      update time constant variables if time step changed
          if (al.ne.0.0) then
            a1p = a1p*tfac - tfac + 1.
            a2p = a2p*tfac - tfac + 1.
            a3  = a3 *tfac - tfac + 1.
            a4  = a4 *tfac - tfac + 1.
            a5  = a5 *tfac - tfac + 1.

            if (mgov.ge.6) then
              a6p = a6p*tfac - tfac + 1.
              a7  = a7 *tfac - tfac + 1.
            endif

            a1gov  = -sgkp1*a2p/(a1p*a3)

            if (mgov.eq.5) then
              a1turb = 1./a4
            else if (mgov.eq.6) then
              a1turb = (sgk1 + (sgk3 + sgk5/a6p)/a5)/a4
            else if (mgov.eq.7) then
              a1turb = (sgk1 + (sgk3 + (sgk5+sgk7/a7)/a6p)/a5)/a4
            else if (mgov.eq.8) then
              a1turb = (sgk1 + sgk5/a6p/a5)/a4
              a1turl = (sgk4 + sgk6/a6p)/a5/a4
            else if (mgov.eq.9) then
              a1turb = (sgk1 + sgk3/a5)/a4
              a1turl = sgk6/a6p/a5/a4
            else if (mgov.eq.10) then
              a1turb = (sgk1 + (sgk5+sgk7/a7)/a6p/a5)/a4
              a1turl = (sgk4 + (sgk6+sgk8/a7)/a6p)/a5/a4
            endif
          endif

c --      update past state values
          b1 = sgkp1*wnow*(a2p-2.) + st1*(a1p-2.)
          if (st3.lt.pmax.and.st3.gt.pmin) then
            b3 = st2 + (2./edt)*st3
          else
            b3 = (2./edt)*st3
          end if
          b4 = st3 + st4*(a4 - 2.)
          if (mgov.ge.6) then
            b5  = st4 + st5*(a5  - 2.)
            b6p = st5 + st6*(a6p - 2.)
            b7  = st6 + st7*(a7  - 2.)
          endif

          govmx(ispf) = pmax
          govmn(ispf) = pmin
          st3h = (b3 + velmax)*edt/2.
          st3l = (b3 + velmin)*edt/2.
          if (st3h.lt.govmx(ispf)) govmx(ispf) = st3h
          if (st3l.gt.govmn(ispf)) govmn(ispf) = st3l

          a2gov  = (pgo + b1/a1p + b3*(a3 - 1.)*(edt/2.))/a3

          if (mgov.eq.5) then
            a2turb = b4/a4
          else if (mgov.eq.6) then
            sgk6p  = sgk5/a6p
            sgk5p  = (sgk3+sgk6p)/a5
            a2turb = b4*a1turb + b5*sgk5p + b6p*sgk6p
          else if (mgov.eq.7) then
            sgk7p  = sgk7/a7
            sgk6p  = (sgk5 + sgk7p)/a6p
            sgk5p  = (sgk3 + sgk6p)/a5
            a2turb = b4*a1turb + b5*sgk5p + b6p*sgk6p + b7*sgk7p
          else if (mgov.eq.8) then
            sgk6p  = sgk5/a6p
            sgk5p  = sgk6p/a5
            a2turb = b6p*sgk6p + b5*sgk5p + b4*a1turb
            sgk6p  = sgk6/a6p
            sgk5p  = (sgk4 + sgk6p)/a5
            a2turl = b6p*sgk6p + b5*sgk5p + b4*a1turl
          else if (mgov.eq.9) then
            a2turb = (sgk3/a5)*b5 + b4*a1turb
            a2turl = sgk6*(b6p + (b5 + b4/a4)/a5)/a6p
          else if (mgov.eq.10) then
            sgk7p  = sgk7/a7
            sgk6p  = (sgk5 + sgk7p)/a6p
            sgk5p  = sgk6p/a5
            a2turb = b7*sgk7p + b6p*sgk6p + b5*sgk5p + b4*a1turb
            sgk7p  = sgk8/a7
            sgk6p  = (sgk6 + sgk7p)/a6p
            sgk5p  = (sgk4 + sgk6p)/a5
            a2turl = b7*sgk7p + b6p*sgk6p + b5*sgk5p + b4*a1turl
          endif

        endif

        govpow = a1gov*wnew + a2gov
        if (govpow.gt.govmx(ispf)) govpow = govmx(ispf)
        if (govpow.lt.govmn(ispf)) govpow = govmn(ispf)
        govpow = a1turb*govpow + a2turb

      endif 

      govpwr(ispf) = govpow 

      if (mgov .ge. 8) then
        ispl = idatat(4)
        govpwr(ispl) = a1turl*govpow + a2turl
      endif

      return
      end
