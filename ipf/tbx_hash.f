C    @(#)tbx_hash.f	20.3 2/13/96
      integer function tbx_hash (busname, basekv)

      character*(*)  busname
      real           basekv

      include 'ipfinc/parametr.inc'

      include 'ipfinc/oldtbx.inc'
      integer        k, i
      
      k = 0
      do i = 1, 8
         k = k + ichar (busname(i:i))
      end do

      k = k + int( basekv )
      tbx_hash = mod (k, TBX_HASHSIZE) + 1

      return
      end
