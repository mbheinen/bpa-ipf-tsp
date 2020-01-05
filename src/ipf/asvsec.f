C    @(#)asvsec.f	20.3 2/13/96
      subroutine asvsec (a,s,v,y)
C
C     Subroutine to calculate the line flow and voltage conditions at
C       the far end of a series line section from conditions at its
C       near end.
C     A is for complex amps, S for complex power, V for complex voltage.
C     Y is a two-port admittance matrix between neighboring buses.
C     Side 1 is the near end.  Side 2 is the far end.
C
      complex a(2),s(2),v(2),y(2,2)
C
      v(2)=(a(1)-y(1,1)*v(1))/y(1,2)
      a(2)=y(2,1)*v(1)+y(2,2)*v(2)
      s(2)=v(2)*conjg(a(2))
      return
      end
