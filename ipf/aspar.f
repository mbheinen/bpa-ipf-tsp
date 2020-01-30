C    @(#)aspar.f	20.3 2/13/96
      subroutine aspar (a,s,v,y,e,f,kb1,kb2)
C
C     Subroutine to calculate current and power flows at the ends of a
C     solo branch, i.e., one with no other series sections.
C     A is for complex amps, S for complex power, V for complex voltage.
C     KB-1 & -2 are internal bus numbers.
C
      complex a(2),s(2),v(2),y(2,2)
      double precision e(*),f(*)
C
      v(1)=cmplx(e(kb1),f(kb1))
      v(2)=cmplx(e(kb2),f(kb2))
      do 170 i=1,2
         a(i)=cmplx(0.0,0.0)
         do 160 j=1,2
            a(i)=a(i)+y(i,j)*v(j)
  160    continue
         s(i)=v(i)*conjg(a(i))
  170 continue
      return
      end
