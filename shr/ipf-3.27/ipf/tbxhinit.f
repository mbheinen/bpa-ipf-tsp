C    @(#)tbxhinit.f	20.3 2/13/96
      subroutine tbxhinit ()

c     tbxhinit  -  initialize oldtbx hashgen tables

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/oldtbx.inc'
      include 'ipfinc/tbx.inc'

      integer  i
      integer addtbxbs

      do i = 1, TBX_HASHSIZE
         htable_tbx(i) = 0
      end do
      do i = 1, TBX_MAXSYMBOL
         nextptr_tbx(i) = 0
         tbxxref(i) = 0
      end do

      nextsymbol_tbx = 0
      do i = 1, ntotb
         nb = tbx(2,i)
         indb = addtbxbs (bus(nb), base(nb))
         tbxxref(indb) = i
         oldtbx(1,i) = tbx(1,i)
         oldtbx(2,i) = tbx(2,i)
         oldtbx(7,i) = tbx(7,i)
      enddo
      numtbx = ntotb

      return
      end
