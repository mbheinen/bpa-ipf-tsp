C    @(#)hndoff.f	20.3 2/13/96
      subroutine hndoff (a,s,v)
C
C     Subroutine to prepare for calculating line flows in the next
C       section by "handing off" (as in the mens's 1600 meter relay)
C       the conditions at the far end of the previous line section to
C       the near end of the present line section.
C     A is for complex amps, S for complex power, V for complex voltage.
C
      complex a(2),s(2),v(2)
C
      v(1)=v(2)
      a(1)=-a(2)
      s(1)=-s(2)
      return
      end
