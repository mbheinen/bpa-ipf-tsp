C    @(#)space.inc	4.3 9/21/93
      common/space0/ simms(200000,7)
      dimension buf4k(23*maxbus),ibuf4k(23*maxbus)
      dimension ecn(maxecn, 8),ecnn(maxecn)
      equivalence (buf4k,ibuf4k,ecn,ecnn,simms)
      real simms, buf4k, ecn, ecnn
      integer ibuf4k 
