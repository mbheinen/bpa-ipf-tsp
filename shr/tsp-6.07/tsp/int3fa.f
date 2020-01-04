C    %W% %G%
      subroutine int3fa(a)
C 
C  THIS SUBROUTINE CALCULATES INITIAL STATE VECTORS AND PAST VALUES
C  FOR EXCITER TYPE FA
C 
      include 'tspinc/param.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/params.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/int3.inc'
      dimension array(40),a(40)
       equivalence         (array(1),  vrmax), (array(2),  vrmin),
     1 (array(3),   vref), (array(4),  esatx), (array(5),  csatx),
     2 (array(6),    hbr), (array(7),    cke), (array(8),    cka),
     3 (array(9),     dr), (array(10),   se2), (array(11),   ckf),
     4 (array(12),    df), (array(13),   akf), (array(14),   hbf),
     5 (array(15),   hcb),                     (array(17),    db),
     6 (array(18),    dc), (array(19),   hbe), (array(20),    de),
     7 (array(21),  rcex), (array(22),    vr), (array(23),  xcex),
     8 (array(24),   vao), (array(25),    da), (array(26),   hba),
     9 (array(27),   vco), (array(28),    slp), (array(29),   efd),
     * (array(30),   dep), (array(31),efdmin), (array(32),efdmax)
c  
      logical debug                                                     !dem
      logical dbghere                                                   !dem
c  
c     -     begin     begin     begin     begin     begin     begin 
      debug = dbghere ('INT3FA  ')                                      !dem
C 
C    -  Transfer CITER data to local array
      do 100 itr = 1,40
100   array(itr) = a(itr)
  
      efd = efdo
C 
C         CALCULATE VOLTAGE TRANSDUCER CIRCUIT
C     -  Despite suffixes, VT below is on d,q coords
      if (xt .eq. 0.0)  then
        vtr = vhd
        vti = vhq
      else
        vtr = vhd + rt*agend - xt*agenq
        vti = vhq + rt*agenq + xt*agend
      endif
      crt = agend
      cit = agenq
      zcr = rcex*crt - xcex*cit
      zci = rcex*cit + xcex*crt
      vcr = zcr + vtr
      vci = zci + vti
      vc = sqrt(vcr**2 + vci**2)
      vco = vc
      vc1 = vc
C 
C         CALCULATE R-BLOCK
C 
      dr = 2.0*dr/dt + 1.0
      hbr = (dr - 2.0)*vc + vc
C 
C     -  CALCULATE SATURATION BLOCK
C 
      ckep = csatx*exp(esatx*abs(efdo))
      ck1 = 0.0
      ck2 = 0.0
      if (ckep .ne. 0.0) then
        ck1 = ckep + ckep*efdo*esatx
        ck2 = efdo**2 * esatx*ckep
      endif
      if (cke .eq. 0.0) cke = -ckep
      vr = (cke+ckep)*efdo
      x3 = vr
      de = 2.0*de/dt
      dep = de +ck1 + cke
      hbe = (dep - 2.0*ck1 - 2.0*cke)*efdo + vr + 2.0*ck2
C 
C         CHECK REGULATOR LIMITS
C 
      if (vrmin .gt. vrmax) then
        iabort=1
        write (errbuf,1860) name,base,id,vrmax,vrmin
        call prterr ('E',1)
 1860   format ('0 REGULATOR LIMITS IMPROPER FOR ',a8,2x,f5.1,
     1    2x,a1,' VRMAX,VRMIN = ',2f5.2)
        go to 2300
      endif
      if (vr .ge. vrmax .or. vr .le. vrmin) then
        write (errbuf, 2036) name, base, id, vr
        call prterr ('E',1)
 2036     format('0 INITIAL VR VIOLATES LIMITS FOR ', a8,
     1           2x, f5.1, 2x, a1, 5x, 'INITIAL VR = ', f6.3)
        iabort = 1
        go to 2300
      endif
C 
C         CALCULATE F-BLOCK
C 
      vfo = 0.0                                                         !dem
C     VF = 0.0
      df = 2.0*df/dt + 1.0
      akf = 2.0*ckf/dt
      hbf = (df - 2.0)*vfo - akf*efdo
C 
C         CALCULATE A-BLOCK (REGULATOR)
C 
      va = vr/cka
      da = 2.0*da/dt + 1.0
      hba = (da - 2.0)*vr + cka*va
      vao = va
      slp = 0.0
C 
C         B-C BLOCK
C 
      vai = va
      db = 2.0*db/dt + 1.0
      dc = 2.0*dc/dt + 1.0
      hcb = (db - 2.0)*va - (dc - 2.0)*vai
      verr = vai
      vref = verr + vc1
C 
C         TRANSFER DATA TO CITER AND RETURN TO INITAL3
C 
 2300 do itr = 1,40
        a(itr) = array(itr)
      enddo
c 
      if (debug) then                                                   !dem
        call dbgeko ('INT3FA - at end:')                                !dem
        call dbgwrf ('  TO /time/           = ',to)                     !dem
        call dbgwrf ('  VC /xducer volt/    = ',vc)                     !dem
        call dbgwrf ('  VREF /ref voltage / = ',vref)                   !dem
        call dbgwrf ('  VFO /feedback volt/ = ',vfo)                    !dem
        call dbgwrf ('  VAI /lead-lag inp/  = ',vai)                    !dem
        call dbgwrf ('  VA /lag input/      = ',va)                     !dem
        call dbgwrf ('  VR /regulator out/  = ',vr)                     !dem
        call dbgwrf ('  EFDO /field volt/   = ',efdo)                   !dem
      endif                                                             !dem
      return
      end
