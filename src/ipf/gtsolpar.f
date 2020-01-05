C    @(#)gtsolpar.f	20.9 7/18/96
C****************************************************************
C
C       File: gtsolpar.f
C
C       Purpose: Subroutine to obtain solution parameters
C
C       Author: Walt Powell  Date: 1 May 1993
C       Called by: p_gtdata.f
C
C****************************************************************
C
        subroutine gtsolpar (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)
c
c       This subroutine returns WSCC-formated input data records.
c       Output parameter:
c
c       in_buffer - a character string specifying desired data
c       out_buffer - a character string for storing data
c       error      - warning switch (0 means ignore errors,
c                                    1 means observe errors)
c
C       This routine obtains and applies a filter for entire input 
C       network data.
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/agc.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/ikk.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/slnopt.inc'

        character  text * 120, null * 1, linefeed * 1, 
     &             ai_control(0:2)*4, ltc_control(0:4)*10, 
     &             phase_shift(0:1)*5, xbus_control(0:2)*5,
     &             on_off_switch(0:1)*4,
     &             base_solution(0:1)*4
c     &             base_solution(0:3)*30
        integer o2, apdoutbuf

        save

        data base_solution / 'OFF', 'ON' /
c       data base_solution / 'NORMAL', 'SOLUTION BYPASS', 
c     &                      'LOAD ALTERNATE SOLUTION',
c     &                      'LOAD ALTERNATE STARTING' /
        data ai_control / 'OFF', 'CON', 'MON' /
        data ltc_control / 'OFF', 'ON_NV', 'ON', 'ON_NPS', 'ON_DCONLY' /
        data xbus_control / 'BPA', 'WSCC', 'VMAX' /
        data phase_shift / 'BPA', 'WSCC' /
        data on_off_switch / 'OFF', 'ON' /

        null = char(0)
        linefeed = char(10)
        last = index (in_buffer, null)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
        lasto2 = o2
c
c       Write solution parameters
c
        if ( iopton(20) .gt. 1 ) iopton(20) = 1
        write (text, 90) base_solution(iopton(20))
   90   format ('>BASE_SOLUTION = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        if (iopton(17) .eq. 3) then                                      
           if (ntotc .gt. 0) then                                          
              iopton(17) = 1                                               
           else                                                          
              iopton(17) = 0                                               
           endif                                                         
        endif                                                            
        write (text, 100) ai_control(iopton(17))
  100   format ('>AI_CONTROL = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 110) on_off_switch(iopton(12))
  110   format ('>DEBUG_TX = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 120) on_off_switch(iopton(13))
  120   format ('>DEBUG_BUS = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 130) on_off_switch(iopton(14))
  130   format ('>DEBUG_AI = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 140) on_off_switch(iopton(15))
  140   format ('>DEBUG_DC = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 150) option(7)
  150   format ('>LIMITS_QRES = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 160) option(9)
  160   format ('>LIMITS_PHA = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 170) option(10)
  170   format ('>LIMITS_DA = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 180) option(11)
  180   format ('>LIMITS_DV = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 190) ltc_control(iopton(16))
  190   format ('>LTC = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 200) xbus_control(kspare(24))
  200   format ('>MISC_XBUS = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 210) on_off_switch(iopton(38))
  210   format ('>MISC_DCLP = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 220) on_off_switch(iopton(18))
  220   format ('>MISC_VFLAT = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 230) option(40)
  230   format ('>MISC_TSTART = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 240) on_off_switch(iopton(22))
  240   format ('>MISC_ITER_SUM = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 250) phase_shift(iopton(21))
  250   format ('>MISC_PHASE_SHIFT_BIAS = ', a)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 260) iopton(1)
  260   format ('>SOL_ITER_DECOUPLED = ', i3)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 270) iopton(3)
  270   format ('>SOL_ITER_NEWTON = ', i3)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 280) option(7)
  280   format ('>TOL_BUSV = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 290) option(5)
  290   format ('>TOL_AIPOWER = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 300) option(6)
  300   format ('>TOL_TX = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        write (text, 310) option(6)
  310   format ('>TOL_Q = ', f8.4)
        last = lastch (text)
        length = apdoutbuf(o2, text(1:last), out_buffer(o2:))
        o2 = o2 + length

        return
        end
