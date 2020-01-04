C    @(#)lis_cmde.f	20.4 2/13/96

      subroutine lis_cmde( cmde_num, flag )
      implicit none
      integer cmde_num, flag

      include 'ipfinc/parametr.inc'

c      include 'ipfinc:alpha.inc'
c      include 'ipfinc:apcom.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
c      include 'ipfinc:cont.inc'
      include 'ipfinc/intbus.inc'
c      include 'ipfinc:outxrf.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/comm_mode.inc'
c      include 'ipfinc:time1.inc'

      common /o2i/ o2i(MAXBUS) ! optimal to original (non reduced) input
      integer o2i

      integer ptr, qptr, pbr, kt, mt
      character br_own * 3, rec_type(4) * 1, chg_type(2) * 1
      integer fmt

      data rec_type / 'B', '+', 'L', 'T' /
      data chg_type / 'D', 'M' /

      if ( flag .eq. 1 ) then
         write (outbuf, 10) 
   10    format (t26, 'Results in:')
         call prtout (1)
         call space(1)
         write (outbuf, 20)
   20    format (t26, 'Ty  Ch  Own  Zones  Bus1           Bus2',
     &      '            /----- Load -----//----- Shunt ----/',
     &      '/-- Generation --/') 
         call prtout (1)
         write (outbuf, 30)
   30    format (t84, 'MW     MVAR       MW     MVAR       ',
     &           'MW     MVAR') 
         call prtout (1)
         assign 930 to fmt
      else
         outbuf = '  --- Ty  Ch  Own  Zones  Bus1           Bus2'
         call prtout (1)
         assign 940 to fmt
      endif

      ptr = comm_ptr(cmde_num)
      do while (ptr .gt. 0)
        qptr = change_ptr(ptr)
        do while (qptr .gt. 0)
          if (orig_type(1,qptr) .eq. 1 .or. 
     &        orig_type(1,qptr) .eq. 2) then
            kt = orig_type(3,qptr)
            if ( flag .eq. 1 ) then
              write (outbuf, 910) rec_type(orig_type(1,qptr)),
     &              chg_type(orig_type(2,qptr)),owner(o2i(kt)),
     &              zone(o2i(kt)), intbus(kt), intbas(kt),
     &              orig_val(3,qptr) * bmva, orig_val(4,qptr) * bmva,
     &              orig_val(5,qptr) * bmva, orig_val(6,qptr) * bmva,
     &              (orig_val(1,qptr) + orig_val(3,qptr)) * bmva,
     &              (orig_val(2,qptr) + orig_val(4,qptr)) * bmva
            else
              write (outbuf, 920) rec_type(orig_type(1,qptr)),
     &              chg_type(orig_type(2,qptr)),owner(o2i(kt)),
     &              zone(o2i(kt)), intbus(kt), intbas(kt)
            endif
  910       format (t26, a1, 3x, a1, 3x, a3, 2x, a2, 5x, a8, f6.1,
     &              t77, 6f9.1)
  920       format ('  --- ', a1, 3x, a1, 3x, a3, 2x, a2, 5x, a8, f6.1)
            call prtout (1)
          else if (orig_type(1,qptr) .eq. 3 .or. 
     &             orig_type(1,qptr) .eq. 4) then
            kt = orig_type(3,qptr)
            mt = orig_type(4,qptr)
            if (inp2alf(o2i(kt)) .lt. inp2alf(o2i(mt)))
     &        then
              pbr = iabs(brnch_ptr(orig_type(6,qptr)))
              call getchr(3,br_own,kbrnch(3,pbr))
              write (outbuf, fmt) 
     &              rec_type(orig_type(1,qptr)),
     &              chg_type(orig_type(2,qptr)),
     &              br_own, zone(o2i(kt)), zone(o2i(mt)),
     &              intbus(kt), intbas(kt),
     &              intbus(mt), intbas(mt), 
     &              char(orig_type(5,qptr))
  930         format (t26, a1, 3x, a1, 3x, a3, 2x, a2, 1x, a2,
     &                2x, a8, f6.1, 1x, a8, f6.1, 1x, a)
  940         format ('  --- ', a1, 3x, a1, 3x, a3, 2x, a2, 1x, a2,
     &                2x, a8, f6.1, 1x, a8, f6.1, 1x, a)
              call prtout (1)
            endif
          endif
          qptr = orig_nxt(qptr)
        enddo
        ptr = change_nxt(ptr)
      enddo
      return

      end

