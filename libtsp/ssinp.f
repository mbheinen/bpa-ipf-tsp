C    %W% %G%
      subroutine ssinp (temp)
        real temp(13)
c     -  Sets up constants for the temporary new PSS device
c
      include 'tspinc/demfix.inc'
c
c     -     begin     begin     begin     begin     begin     begin
      sskqv  = temp(1)
      sstqv  = temp(2)
      sskqs  = temp(3)
      sstqs  = temp(4)
      sstq   = temp(5)
      sstq1  = temp(6)
      sstpq1 = temp(7)
      sstq2  = temp(8)
      sstpq2 = temp(9)
      sstq3  = temp(10)
      sstpq3 = temp(11)
      ssvsmax = temp(12)
      ssvscut = temp(13)
c 
      return
      end
