C    @(#)bus_edt.f	20.5 11/11/97
C****************************************************************
C
C   	File: bus_edt.f
C
C   	Purpose: Modify encoded bus record "xbuf" to conform to 
C                specific WSCC and PTI dialects.
C                data onto saved NETWORK_DATA file savfil.
C                                                                      *
C       Input and output parameters:
C
C             xbuf     - the encoded bus record
C             dialect  - the WSCC dialect sought
c
C   	Author: Hanford Van Ness             Date: circa 1980
C   	Called by: ext_bus.f
C
C****************************************************************
C
      subroutine bus_edt ( xbuf, dialect, totsuscp )
      character*(*) xbuf, dialect

      include 'ipfinc/lfiles.inc'
 
      character code * 10
      real pgen,  qgen, vhold, vmin, qmax, qmin, shunt
      external code
 
C      
C     All bus types except BV, BX, BD, and BM: Set Vmin to Blank
 
      if ( index( 'BV$BX$BD$BM', xbuf(1:2)) .eq. 0 ) then
         read (xbuf(62:65),fmt='(bz, f4.3)') vmin
         if (vmin .ne. 0) then
            write (dbug,2200)
 2200       format (' VMIN set to blank on next rec')
            write (dbug,2300) xbuf(1:80)
 2300       format (' [', a, ']')
            xbuf(62:65) = ' '
            write (dbug,2300) xbuf(1:80)
            write (dbug,2100)
 2100       format (' ')  
         endif
      endif
 
C     Bus types "B ", "BC", "BV", "BT": non-zero QMIN set to 0.0
C
C     Note: BPA's convention will observe Qmin for these bus types.
C     If that field is non-zero, Qmax is assumed to be "sloppy" data
C     and is set to zero, an altogether different result. The rationale
C     is that the bus was probably hastily changed from type BQ and
C     that these fields should have been changed as well, but weren't.
 
      if ( index( 'B $BC$BV$BT', xbuf(1:2)) .ne. 0 ) then
         read (xbuf(53:57),fmt='(bz, f5.0)') qmin
         if (qmin .ne. 0.0) then
            write (dbug,2500)
 2500       format (' QMIN set to zero in B , BC, BV, or BT bus')
            write (dbug,2300) xbuf(1:80)
            xbuf(53:57) = ' '
            write (dbug,2300) xbuf(1:80)
            write (dbug,2100)
         endif
      endif
 
C     For type BE buses: 
C       1. Non-zero QLIMITS changed to type "BQ"..
C       2. Zero QLIMITS changed to type "B " and the voltage hold 
C          is blanked out.
 
      if (xbuf(1:2) .eq. 'BE') then
         read (xbuf(48:52),fmt='(bz, f5.0)') qmax
         read (xbuf(53:57),fmt='(bz, f5.0)') qmin
         if (qmax .ne. 0.0 .or. qmin .ne. 0.0) then
            write (dbug,2600)
 2600       format (' Following BE bus changed to BQ')
            write (dbug,2300) xbuf(1:80)
            xbuf(1:2) = 'BQ'
            write (dbug,2300) xbuf(1:80)
            write (dbug,2100)
         else if (qmax .eq. 0.0 .and. qmin .eq. 0.0) then
            write (dbug,2700)
 2700       format ('FOLLOWING BE BUS CHANGE TO B ')
            write (dbug,2300) xbuf(1:80)
            xbuf(1:2) = 'B '
            xbuf(58:61) = ' '
            xbuf(62:65) = ' '
            write (dbug,2300) xbuf(1:80)
            write (dbug,2100)
         endif
      endif
 
C     Type "BQ": 
C        1. When PGEN=0 or QGEN=0 flag and advise
C        2. When QMAX<=QMIN, change QMAX = QMIN + 2'
 
      if (xbuf(1:2) .eq. 'BQ') then
         read (xbuf(43:47),fmt='(bz, f5.0)') pgen
         read (xbuf(48:52),fmt='(bz, f5.0)') qgen
         if (dialect(1:4) .eq. 'WSCC') then
            if (pgen .eq. 0 .or. qgen .eq. 0) then
               write (dbug,2900)
 2900          format (' Following BQ bus has zero PGEN/QGEN')
               write (dbug,2300) xbuf(1:80)
               write (dbug,2100)
            endif
 
C        2. When holding voltage with shunt device, change to 
C           hold voltage with and equivalent but ficticious 
c           synchronous condensor
 
            read (xbuf(35:38),fmt='(bz, f4.0)') shunt
            shunt = shunt - totsuscp            
            if (abs(shunt) .lt. 0.1) shunt = 0.0
            read (xbuf(48:52),fmt='(bz, f5.0)') qmax
            read (xbuf(53:57),fmt='(bz, f5.0)') qmin
            read (xbuf(58:61),fmt='(bz, f4.3)') vhold
            if (shunt .ne. 0.0) then
               write (dbug,3000)
 3000          format (' Following BQ bus will hold voltage',
     1                 ' with Synchronous Condenser')
               write (dbug,2300) xbuf(1:80)
   
               if (shunt .gt. 0.0) then
                  qmax = qmax + shunt * vhold * vhold
               else
                  qmin = qmin + shunt * vhold * vhold
               endif
 
               xbuf(48:52) = code(qmax,5,0)
               xbuf(53:57) = code(qmin,5,0)
               write (dbug,2300) xbuf(1:80)
               write (dbug,2100)
            endif
            xbuf(35:38) = code (totsuscp, 4, 0)
            if (qmax .le. qmin) then
               xbuf(48:52) = code(qmax+2.0,5,0)
               write (dbug,3002)
 3002          format (' Following BQ bus has QMAX changed to QMIN + 2 M
     &VAR')
               write (dbug,2300) xbuf(1:80)
            endif

         else if (dialect .eq. 'PTI') then
            if (qgen .ne. 0.0) then

C              When holding voltage with shunt device, change to 
C              hold voltage with and equivalent but ficticious 
c              synchronous condensor
 
               read (xbuf(35:38),fmt='(bz, f4.0)') shunt
               read (xbuf(48:52),fmt='(bz, f5.0)') qmax
               read (xbuf(53:57),fmt='(bz, f5.0)') qmin
               read (xbuf(58:61),fmt='(bz, f4.3)') vhold
               if (shunt .ne. 0.0) then
                  write (dbug,3010)
 3010             format (' Following BQ bus will hold voltage',
     1                 ' with Synchronous Condenser')
                  write (dbug,2300) xbuf(1:80)
   
                  if (shunt .gt. 0.0) then
                     qmax = qmax + shunt * vhold * vhold
                  else
                     qmin = qmin + shunt * vhold * vhold
                  endif
 
                  xbuf(35:38) = ' '
                  xbuf(48:52) = code(qmax,5,0)
                  xbuf(53:57) = code(qmin,5,0)
                  write (dbug,2300) xbuf(1:80)
                  write (dbug,2100)
               endif
            endif
         endif
 
C     Type "BG": 
C        1. When PGEN=0 or QGEN=0 flag and advise
C        2. When QMAX<=QMIN, change QMAX to QMIN + 2 MVAR'
 
      else if (xbuf(1:2) .eq. 'BG') then
         read (xbuf(43:47),fmt='(bz, f5.0)') pgen
         read (xbuf(48:52),fmt='(bz, f5.0)') qgen
         if (dialect(1:4) .eq. 'WSCC') then
            if (pgen .eq. 0 .or. qgen .eq. 0) then
               write (dbug,3020)
 3020          format (' Following BG bus has zero PGEN/QGEN')
               write (dbug,2300) xbuf(1:80)
               write (dbug,2100)
            endif
            read (xbuf(48:52),fmt='(bz, f5.0)') qmax
            read (xbuf(53:57),fmt='(bz, f5.0)') qmin
            if (qmax .le. qmin) then
               xbuf(48:52) = code(qmax+2.0,5,0)
               write (dbug,3030)
 3030          format (' Following BG bus has QMAX changed to QMIN + 2 M
     &VAR')
               write (dbug,2300) xbuf(1:80)
            endif
         endif

C     Type "BX": 
C        1. Blank out remote bus field.
 
      else if (xbuf(1:2) .eq. 'BX') then
         if (dialect(1:4) .eq. 'WSCC') then
            xbuf(66:73) = ' '
            xbuf(74:77) = ' '
            write (dbug,3040)
 3040       format (' Following BX bus has remote bus field blanked out'
     &)
            write (dbug,2300) xbuf(1:80)
            write (dbug,2100)
         endif
      endif
      return
      end
