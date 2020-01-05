C    @(#)slnhdr.f	20.7 11/11/97
      subroutine slnhdr

      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/ordsta.inc'

      common /itota/ itota

      integer count
C
      lprtsw=1
      if (kspare(16).ge.0) fichsw=1
      call forbtm
      outbuf = '* * * SOLUTION ITERATION SUMMARY * * *'
      call rpnlod
      outbuf=' '
      do 2282 j = 1, 5
         call shdlod(j)
 2282 continue
      call fortop
      call space (2)
      write (outbuf,2300) kbsknt, kbrknt
 2300 format('0', 11x, i5, ' BUSES', 46x, i5, ' BRANCHES' )
      call prtout(1)
      write (outbuf,2310) ntotb, itota
 2310 format('0', 12x, i4, 3x,'-- WITH ADJUSTABLE TYPES', 26x,
     1  i4, 3x, '-- LTC TRANSFORMERS' )
      call prtout(1)
      i=iasw*ntotc
      write (outbuf,2320) i,idckt
 2320 format(t14, i4, 3x,'-- CONTROLLING AREA INTERCHANGE', 19x,
     1       i4, 3x, '-- D C SYSTEMS ')
      call prtout(1)
      i = ntxtie/2
      write (outbuf,2330) npctvr,i
 2330 format(t14, i4, 3x, '-- PERCENTAGE VAR CONTROLLED',22x,
     1       i4,'   -- IDEAL PHASE SHIFTERS ')
      call prtout (1)
c
c     Count actual BX buses
c
      count = 0
      do i = 1, kxtot
        kt = xdata(1,i)
        if (ordtbx .eq. 1) kt = inp2opt(kt)
        if (ntypu(kt) .eq. 11) count = count + 1
      enddo

      write (outbuf,2340) count
 2340 format(t14, i4, 3x,'-- WITH SWITCHABLE REACTANCE ')
      call prtout(1)
      write (outbuf,2342) numagc
 2342 format(t14, i4, '   -- AGC UNITS')
      call prtout(1)
      write (outbuf,2350) nbslck
 2350 format(t14, i4, '   -- SLACK BUSES')
      call prtout(1)
      call space (2)
      write (outbuf,2360)
 2360 format('0  ITERATION  --------------------ABSOLUTE ERROR ',
     1       'SUMMATION--------------------  ',
     2       '-------UNSOLVED--------  -ADJUSTMENTS-  JACOBIAN' )
      call prtout(1)
      write (outbuf,2370)
 2370 format(19x, 'P', 12x, 'Q', 11x, 
     1  'XFMRS     AREA EXPORT  BUS VOLTAGE', 9x, 'AUTO', 8x,
     2  'D C    SLN     BUS    MATRIX' )
      call prtout(1)
      write (outbuf,2380)
 2380 format(9x,'NO    (P.U. MW)    (P.U.MVAR)    (P.U.MVA)    ',
     1       '(P.U. MW)    (P.U.KV)    BUSES XFRMS AREAS LINES  ',
     2       'TRUNC   TYPE   STORAGE' )
      call prtout(1)
      write (outbuf,2390)
 2390 format(9x,'--   -----------  ------------  -----------  -',
     1       '----------  -----------  ----- ----- ----- -----  ',
     2       '-----   ----   -------' )
      call prtout(1)
      outbuf = ' '
      call prtout(1)
      return
      end
