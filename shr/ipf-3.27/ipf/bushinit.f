C    @(#)bushinit.f	20.3 2/13/96
      subroutine bushinit ()

c        bushinit  -  initialize bus hashgen tables

      include 'ipfinc/parametr.inc'
      include 'ipfinc/bushasht.inc'

      integer  i

      do i = 1, BUS_HASHSIZE
         htable_b(i) = 0
      end do
      do i = 1, MAXBUS
         nextptr_b(i) = 0
      end do

      return
      end
