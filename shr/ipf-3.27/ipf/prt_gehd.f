C    @(#)prt_gehd.f	20.2 8/19/99
      subroutine prt_gehd (options)
      integer options(*)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/prt.inc'

      character tag(4)*1
C
      lprtsw = 1
      if (kspare(16) .ge. 0) fichsw = 1
      call forbtm
      outbuf = '* * * GE Data Import Options * * *'
      call rpnlod
      outbuf = ' '
      do i = 1, 5
         call shdlod(i)
      enddo
      call fortop
      call space (2)
      write (outbuf, 610)
  610 format(' ', 131('*'))
      call prtout(1)
      write (outbuf, 620)
  620 format(' *', t132, '*')
      do i = 1,4
        call prtout(1)
      enddo
      write (outbuf, 622)
  622 format (' *', t11, 'GE Import Options ')
      call prtout(1)
      write (outbuf, 620)
      call prtout(1)
      call prtout(1)

      write (outbuf, 630)
  630 format (' *', t43, 'RateA  RateB  RateC', t132, '*')
      call prtout(1)
      do i = 1, 3
        tag(i) = ' '
      enddo
      if (options(1) .gt. 0) tag(options(1)) = 'X'
      write (outbuf, 640) (tag(i), i = 1,3)
  640 format (' *', t11, 'Transformer ratings:  Nominal',
     &        t44, '(', a, ')    (', a, ')    (', a, ')', t132, '*')
      call prtout(1)
      do i = 1, 3
        tag(i) = ' '
      enddo
      if (options(2) .gt. 0) tag(options(2)) = 'X'
      write (outbuf, 650) (tag(i), i = 1,3)
  650 format (' *', t33, 'Thermal', 
     &        t44, '(', a, ')    (', a, ')    (', a, ')', t132, '*')
      call prtout(1)
      do i = 1, 3
        tag(i) = ' '
      enddo
      if (options(3) .gt. 0) tag(options(3)) = 'X'
      write (outbuf, 660) (tag(i), i = 1,3)
  660 format (' *', t33, 'Emergency', 
     &        t44, '(', a, ')    (', a, ')    (', a, ')', t132, '*')
      call prtout(1)
      do i = 1, 3
        tag(i) = ' '
      enddo
      if (options(4) .gt. 0) tag(options(4)) = 'X'
      write (outbuf, 670) (tag(i), i = 1,3)
  670 format (' *', t33, 'Bottleneck', 
     &        t44, '(', a, ')    (', a, ')    (', a, ')', t132, '*')
      call prtout(1)
      write (outbuf, 620)
      call prtout(1)
      do i = 1, 3
        tag(i) = ' '
      enddo
      if (options(5) .gt. 0) tag(options(5)) = 'X'
      write (outbuf, 680) (tag(i), i = 1,3)
  680 format (' *', t11, 'Line ratings:         Nominal',
     &        t44, '(', a, ')    (', a, ')    (', a, ')', t132, '*')
      call prtout(1)
      do i = 1, 3
        tag(i) = ' '
      enddo
      if (options(6) .gt. 0) tag(options(6)) = 'X'
      write (outbuf, 690) (tag(i), i = 1,3)
  690 format (' *', t33, 'Thermal', 
     &        t44, '(', a, ')    (', a, ')    (', a, ')', t132, '*')
      call prtout(1)
      do i = 1, 3
        tag(i) = ' '
      enddo
      if (options(7) .gt. 0) tag(options(7)) = 'X'
      write (outbuf, 700) (tag(i), i = 1,3)
  700 format (' *', t33, 'Bottleneck', 
     &        t44, '(', a, ')    (', a, ')    (', a, ')', t132, '*')
      call prtout(1)
      write (outbuf, 620)
      call prtout(1)
      call prtout(1)

      write (outbuf, 710)
  710 format (' *', t43, 'High    Low    Ave   Range', t132, '*')
      call prtout(1)
      do i = 1, 4
        tag(i) = ' '
      enddo
      if (options(8) .gt. 0) tag(options(8)) = 'X'
      write (outbuf, 720) tag
  720 format (' *', t11, 'Voltage hold: Type BQ buses',
     &        t44, '(', a, ')    (', a, ')    (', a, ')    (', a, ')', 
     &        t132, '*')
      call prtout(1)
      write (outbuf, 730) tag
  730 format (' *', t25, 'Type BC buses',
     &        t44, '(', a, ')    (', a, ')    (', a, ')    (', a, ')', 
     &        t132, '*')
      call prtout(1)
      write (outbuf, 740) tag
  740 format (' *', t25, 'Type BT buses',
     &        t44, '(', a, ')    (', a, ')    (', a, ')    (', a, ')', 
     &        t132, '*')
      call prtout(1)
      write (outbuf, 620)
      call prtout(1)
      call prtout(1)

      if (options(9) .eq. 0) then
        tag(1) = 'X'
        tag(2) = ' '
      else
        tag(1) = ' '
        tag(2) = 'X'
      endif     
      write (outbuf, 750) tag(1), tag(2)
  750 format (' *', t11, 'Retain Pseudo-buses?  Yes (', a, ') No (',
     &   a, ')')
      call prtout(1)

      write (outbuf, 620)
      do i = 1,4
        call prtout(1)
      enddo
      write (outbuf, 610)
      call prtout(1)

      return
      end
