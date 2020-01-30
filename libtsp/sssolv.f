C    %W% %G%
      subroutine sssolv (win,vin,dlt,pssout)
c     -  Calculates the PSS state vars at the end of a time step.
c     -
c     -  _win_ is step end bus frequency or shaft slip of accel power.
c        _vin_ is step end bus voltage
c        _dlt_ is step length (seconds)
c        _pssout_ is the final output to send into the exciter.
c
      include 'tspinc/demfix.inc'
c
c     -     begin     begin     begin     begin     begin     begin
c     -  Temp defeat of frequency input
c     ssvfr(2) = tzlag (sskqs,sstqs,dlt,ssdfr(1),ssdfr(2),ssvfr(1),1)   !dem
      ssvfr(2) = 0.0                                                    !dem
c
      ssdvt(2) = ssvt0 - vin                                            !dem
      ssvv(2) = tzlag (sskqv,sstqv,dlt,ssdvt(1),ssdvt(2),ssvv(1),1)     !dem
      ssvi(2) = ssvfr(2) - ssvv(2)                                      !dem
      ssvq(2) = tzlead (sstq,sstq,dlt,ssvi(1),ssvi(2),ssvq(1),1)        !dem
      ssv1(2) = tzldlg (sstpq1,sstq1,dlt,ssvq(1),ssvq(2),ssv1(1),1)     !dem
      ssv2(2) = tzldlg (sstpq2,sstq2,dlt,ssv1(1),ssv1(2),ssv2(1),1)     !dem
      ssv3(2) = tzldlg (sstpq3,sstq3,dlt,ssv2(1),ssv2(2),ssv3(1),1)     !dem
      ssvsmin = -ssvsmax                                                !dem
      ssvps(2) = amax1 (ssvsmin, amin1(ssvsmax,ssv3(2)))                !dem

c 
      return
      end
