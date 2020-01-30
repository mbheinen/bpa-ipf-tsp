C    @(#)kmtxbs.f	20.3 2/13/96
       integer function kmtxbs (iotxa,iotxb)
 
c      Performs the ar-zn-ow then tfrmr comparisons for QUIKSORT
c      iotxa and iotxb are pointers to the OVEREX arrays.
c      KMTXBS is comparison result indicator,
c
c              0 = matched
c             -1 = if (iotxa) smaller
c             +1 = if (iotxa) larger
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/area.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/overex.inc'
c
      character*4 char4
      integer*4 int4, iotxa, iotxb, ma, mb, jbra, jbrb
      integer qa
      equivalence (char4,int4)
      character ownra*3,ownrb*3,zona*2,zonb*2
c
c          Get tfrmrs' table pointers & branch IDs early since used
c           - often later on
c
      ma = iotxptr(iotxa)
      mb = iotxptr(iotxb)
      jbra = itxbdptr(ma)
      jbrb = itxbdptr(mb)
c
c                                  First sort criterion
c
      if (jsort .eq. 4) then
c                                  When sorting by areas
         idbusa = kx(jbra)
         iareaa = jarzn(idbusa)
         idbusb = kx(jbrb)
         iareab = jarzn(idbusb)
         kmtxbs = iareaa - iareab
c
      elseif (jsort .eq. 3) then
c                                  When sorting by zones
c                                  - Set KMTXBS by comparison of 
c                                  Bus1_zone_iotxa vs. Bus1_zone_iotxb
         idbusa = kx(jbra)
         zona = zone(idbusa)
         idbusb = kx(jbrb)
         zonb = zone(idbusb)
         kmtxbs = kompr(zona,zonb,kmtxbs)
c
      elseif (jsort .eq. 1) then
c                                When sorting by owners
c                                - Set KMTXBS by comparison of 
c                                tfrmr_owner_iotxa vs. tfrmr_owner_iotxb
         qa = iabs(brnch_ptr(jbra))
         int4 = kbrnch(3,qa)
         ownra = char4
         qa = iabs(brnch_ptr(jbrb))
         int4 = kbrnch(3,qa)
         ownrb = char4
         kmtxbs = kompr(ownra,ownrb,kmtxbs)
c
      else
c                                  Otherwise call it a tie
         kmtxbs = 0
      endif
c                             Done with Case of primary sort comparison
c
      if (kmtxbs .eq. 0) then
c
c           Still a tie, so set KMTXBS by comparison of alpha sort of 
c           the over_excited_tx bus 1 id.
c
         kmtxbs = inp2alf(kx(jbra)) - inp2alf(kx(jbrb))
      endif
c
      return
      end
