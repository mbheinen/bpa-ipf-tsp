C    %W% %G%
      subroutine ssinit (vmag)
c     -  Establishes initial conditions for PSS state vars at t = 0.-
c
      include 'tspinc/demfix.inc'
c
c     -     begin     begin     begin     begin     begin     begin
      ssvt0 = vmag
      ssvt(1) = 0.
      ssvt(1) = 0.
      ssvt(2) = 0.
      ssvfr(1) = 0.
      ssvfr(2) = 0.
      ssvv(1) = 0.
      ssvv(2) = 0.
      ssvi(1) = 0.
      ssvi(2) = 0.
      ssvq(1) = 0.
      ssvq(2) = 0.
      ssv1(1) = 0.
      ssv1(2) = 0.
      ssv2(1) = 0.
      ssv2(2) = 0.
      ssv3(1) = 0.
      ssv3(2) = 0.
      ssvps(1) = 0.
      ssvps(2) = 0.
c 
      return
      end
