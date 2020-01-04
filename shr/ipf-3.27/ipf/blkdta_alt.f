C    @(#)blkdta_alt.f	20.3 2/13/96
      subroutine blkdta_alt

      include 'ipfinc/parametr.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/alt_flag.inc'

      common /obcdflg/ obldtbx, olowx, oxlinmn, onpctq, opctq(MAXBUS)
      logical obldtbx, olowx
      integer onpctq

      alt_base_loaded = 0
      tbx_flag = .false.

      obrnch_nxt(0) = 0
      obrtype(0) = 0

      obldtbx = .false.
      olowx = .false.
      onpctq = 0
      oxlinmn = 0.0
      do i = 1 , MAXBUS
         opctq(i) = 100.0
      enddo

      return
      end

