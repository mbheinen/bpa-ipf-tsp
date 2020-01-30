C    @(#)ptihinit.f	20.3 2/28/00
C****************************************************************
C
C     File: ptihinit.f
C
C     Purpose: Routine to initialize PTI hashgen tables
C
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      subroutine ptihinit ()

      include 'ipfinc/parametr.inc'
      include 'ipfinc/pti_data.inc'

      integer  i

      do i = 1, PTI_HASHSIZE
         htable_n(i) = 0
         htable_b(i) = 0
      end do
      do i = 1, MAXCAR
         htable_a(i) = 0
         htable_c(i) = 0
         nextptr_a(i) = 0
         nextptr_c(i) = 0
      end do
      do i = 1, MAXCZN
         htable_z(i) = 0
         htable_y(i) = 0
         nextptr_y(i) = 0
      end do
      do i = 1, MAXBUS
         nextptr_b(i) = 0
         nextptr_n(i) = 0
      end do
      num_hashn = 0
      num_hashb = 0
      num_znam = 0
      num_anam = 0

      return
      end
