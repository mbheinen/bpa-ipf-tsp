C    @(#)himva.f	20.3 2/13/96
      real function himva (s,basmva)
C
C     Function to calculate the higher of the two MVA's in or out of a
C       transformer for analysis reporting purposes.
C     S is for complex power.  BASMVA is the system base MVA.
C
      complex s(2)
C
      a1 = cabs(s(1))*basmva
      a2 = cabs(s(2))*basmva
      himva = amax1(a1,1.0e-6)
      himva = amax1(a2,himva)
      return
      end
