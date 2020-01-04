C    %W% %G%
      subroutine ssupdt
c     -  Updates past values of PSS state vars in preparation for start
c        of a new time step.
c
      include 'tspinc/demfix.inc'
c
c     -     begin     begin     begin     begin     begin     begin
        ssvfr(1) = ssvfr(2)                                             !dem
        ssvv(1) = ssvv(2)                                               !dem
        ssvi(1) = ssvi(2)                                               !dem
        ssvq(1) = ssvq(2)                                               !dem
        ssv1(1) = ssv1(2)                                               !dem
        ssv2(1) = ssv2(2)                                               !dem
        ssv3(1) = ssv3(2)                                               !dem
        ssvps(1) = ssvps(2)                                             !dem
c 
      return
      end
