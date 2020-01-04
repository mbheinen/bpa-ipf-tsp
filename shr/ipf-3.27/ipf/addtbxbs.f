C    @(#)addtbxbs.f	20.3 2/13/96
      integer function addtbxbs (busname, basekv)

c     install a new entry into tbx hashtable

      character*(*)  busname
      real           basekv

      include 'ipfinc/parametr.inc'

      include 'ipfinc/oldtbx.inc'

      integer        h, tbx_hash
      logical        status

      status = (nextsymbol_tbx .le. TBX_MAXSYMBOL)

      if (status) then
         h = tbx_hash (busname, basekv)
         nextsymbol_tbx = nextsymbol_tbx + 1
         nextptr_tbx(nextsymbol_tbx) = htable_tbx(h)
         htable_tbx(h) = nextsymbol_tbx
         tbxbus(nextsymbol_tbx) = busname
         tbxbase(nextsymbol_tbx) = basekv
         addtbxbs = nextsymbol_tbx
      else
         addtbxbs = 0
      endif
      return
      end
