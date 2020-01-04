C    %W% %G%
      subroutine solfa(a)
C 
C THIS SUBROUTINE CALCULATES FIELD VOLTAGE FOR IEEE MODEL FA
C 
      include 'tspinc/params.inc'
      include 'tspinc/param.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/comvar.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/lnk1a.inc'

      dimension array(40),a(40)

       equivalence         (array(1),  vrmax), (array(2),  vrmin),
     1 (array(3),   vref), (array(4),  esatx), (array(5),  csatx),
     2 (array(6),    hbr), (array(7),    cke), (array(8),    cka),
     3 (array(9),     dr), (array(10),   se2), (array(11),   ckf),
     4 (array(12),    df), (array(13),   akf), (array(14),   hbf),
     5 (array(15),hcb),(array(16),ckap),(array(17),db),
     6 (array(18),    dc), (array(19),   hbe), (array(20),    de),
     7 (array(21),  rcex), (array(22),   vro), (array(23),  xcex),
     8 (array(24),   vao), (array(25),    da), (array(26),   hba),
     9 (array(27),   vco), (array(28),   slp), (array(29),   efd),
     * (array(30),   dep), (array(31),efdmin), (array(32),efdmax),
     * (array(33),    a1), (array(34),    a2)
 
C TRANSFER CITER DATA TO ARRAY
      do itr = 1,40
        array(itr) = a(itr)
      end do
  
C CALCULATE VOLTAGE TRANSDUCER CIRCUIT
      zcd = rcex*oid - xcex*oiq
      zcq = rcex*oiq + xcex*oid
      vcd = zcd + vtd
      vcq = zcq + vtq
      vc = sqrt(vcd**2 + vcq**2)
  
C TEST FOR NEW TIMESTEP
      if (lppwr .eq. 0) then
  
C RECALCULATE STATE VECTORS FOR TIME = T
          efdo = efd
          vc1 = (hbr + vco)/dr
          if (dr .eq. 1.0) vc1 = vco
          vf = (efdo*akf + hbf)/df
          vai = x7o - vf + vref - vc1
          va = (dc*vai + hcb)/db
          slp = va - vao
          vr = dep*efd-hbe
          vr = amax1(vrmin,amin1(vr,vrmax))

          ck1 = csatx * exp (esatx * efd)*(1.0 +  efd*esatx)
          ck2 = efd**2 * esatx * csatx * exp (esatx * efd)
 
C CHANGE TIME FACTORS FOR TIME STEP CHANGE
          if (al .ne. 0.0) then
             da = (da - 1.0)*tfac + 1.0
             db = (db - 1.0)*tfac + 1.0
             dc = (dc - 1.0)*tfac + 1.0
             de = de*tfac
             df = (df - 1.0)*tfac + 1.0
             akf = akf*tfac
             dr = (dr - 1.0)*tfac + 1.0
          endif
  
C UPDATE PAST VALUE PARAMETERS
          if (dr .eq. 1.0) vc1 = vc
          hbr = (dr - 2.0)*vc1 + vc
          hcb = (db - 2.0)*va - (dc - 2.0)*vai
          dep = de + ck1 + cke
          hbe = (dep - 2.0*cke - 2.0*ck1)*efdo + vr + 2.0*ck2
          hbf = (df - 2.0)*vf - akf*efdo
          efdmax = (vrmax + hbe)/dep
          efdmin = (vrmin + hbe)/dep
          hba = (da - 2.0)*vr + cka*va
          ckap = cka
          if(vr .ge.  vrmax .and. slp .gt. 0.0)then
          ckap = 0.0
          hba = vrmax *da
          else if(vr .le. vrmin .and. slp .lt. 0.0)then
          ckap = 0.0
          hba = vrmin*da
          endif
          vao = va
  
C STORE REDUCED SOLUTION: EFD(t) = A1*X7(t) + A1*VC1'(t) + A2
          denom = da*db*dep*df + ckap*dc*akf
          a1 = (ckap*dc*df) / denom
          a2 = (hbe*da*db*df + hba*db*df + ckap*hcb*df - ckap*dc*hbf)
     1         / denom
      endif
  
C CALCULATE STATE VECTORS FOR TIME = T + DT
      if(lppwr .ne. 0) vc = 0.5*vc + 0.5*vco
      vc1 = (vc + hbr)/dr
      vc1p = vref - vc1
      efd = a1*x7 + a1*vc1p + a2
      efd = amin1(efdmax,amax1(efdmin,efd))
      vco = vc

c X3 is V_reg for hist file storage.
      x3 = vr
  
C TRANSFER EXCITER VARIABLES BACK TO CITER
      do itr = 1,40
        a(itr) = array(itr)
      enddo

      return
      end
