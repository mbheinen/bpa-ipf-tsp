C    %W% %G%
      subroutine ffread
C     
C     THIS SUBROUTINE DECODES THE FF CARD AND CHECKS FOR ERRORS
C     IN THE PARAMETERS. IT IS CALLED BY INPUT1
C
C     Revs:
C     Aug/18/92 - DEM
C     Corrected misspelling of DTSJ in read statement
 
      include 'tspinc/params.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/param.inc'
      include 'tspinc/ffcard.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/vtchkc.inc'
      include 'tspinc/toler.inc'

C     declarations
      equivalence (sater, dtc), (isg2, istp)

      character dmpmlc*3

C     -  functions
      logical dbghere

C     -  local variables
      logical debug

C     -      begin     begin     begin     begin     begin     begin
      debug = dbghere('FFREAD  ')
C     
C      READ PROGRAM PARAMETERS FROM FF CARD
C     
      read (buffer, 10000) tsim, dt, endt, dtc, istp, toli, ilim, 
     & delang, dcitol, dpmlt, dtsc, dtsj, frqbsf, lovtex, imblok, 
     & mrelay, mfdep, lsolqt, limang, infbus, nodq, nogv, noex, nosc, 
     & noload
10000 format (bz, 4x, f3.0, 1x, f3.1, 1x, f5.1, 1x, f3.1, 1x, i3, 1x,
     & f5.5, 1x, i3, 1x, f4.4, 1x, f2.0, 1x, f3.3, 3(1x, f2.0), 12(1x,
     & i1))

C     READ (BUFFER,1000)T,DT,ENDT,DTC,ISTP,TOLI,ILIM,DELANG,DCITOL
C     1 ,DPMLT,DTSC,DTSG,FRQBSE,LOVTEX,IMBLOK,MRELAY,MFDEP,LSOLQT
C     2  ,LIMANG,INFBUS, NODQ, NOGV, NOEX, NOSC, NOLOAD
C     1000 FORMAT(4X,F3.0,1X,F3.1,1X,F5.1,1X,F3.1,1X,I3,1X,F5.5,1X,I3,1X
C     1 ,F4.4,1X,F2.0,1X,F3.3,3(1X,F2.0),12(1X,I1))

C     -  Use FRQBSE from /SIMOPTS sectn if using new format
      if (ctlfmt .eq. 'OLD') frqbse = frqbsf
C     
C      WRITE FF CARD IMAGE TO THE .SOL FILE
C     
      irectp = 51
      idesc = 256 + 80
      irecln = 20
      if (debug) then
        call dbgeko('FFREAD  -  writing FF card to history file')
        call dbgwri('  IRECTP /record type/ = ', irectp)
        call dbgwri('  IDESC /rec descrip/  = ', idesc)
        call dbgwri('  IRECLN /rec length/  = ', irecln)
      endif
      call puthisrc(irectp, idesc, irecln, buffer)
C     WRITE (L8) 2,10,BUFFER
      if (frqbse .le. 0.0) frqbse = 60.
      if (dt .eq. 0.0) dt = 2.0
      if (dtc .le. 0.0) dtc = dt
      if (endt .le. 0.0) endt = 300.
      if (istp .le. 0) istp = 15
      write (outbuf, 10010) buffer
10010 format ('0', a)
      call prtout(1)
C     
C     INITIALIZE LOW VOLTAGE SWITCH
C     
      lovlt = 0
      if (ilim .eq. 0) ilim = 25
      if (toli .eq. 0.0) toli = .01
      if (delang .eq. 0.0) delang = 0.1
C     
C        CONVERT DELANG TO RADIANS
C     
      delang = delang*0.01745
      if (dcitol .eq. 0.0) dcitol = 8.0
      if (dmpmlt .eq. 0.0) then
        read (buffer, 10020) dmpmlc
10020   format (44x, a3)
        if (dmpmlc .eq. '   ') dmpmlt = 1.0
      endif
      if (dtsc .eq. 0.0) dtsc =  - 1.0
      if (dtsj .eq. 0.0) dtsj = 6.0
      mfdep = mfdep + 1
      if (mfdep .gt. 5) then
        iabort = 1
        write (errbuf(1), 10030) mfdep
        call prterr('E', 1)
10030   format ('0', 'FFREAD, ILLEGAL FREQ DEPENDENCE. MFDEP CODE ',
     &   'MUST BE LESS THAN 5.', i5)
      endif
      if (dt .lt. 0.0) then
        write (errbuf(1), 10040)
        call prterr('E', 1)
10040   format ('0', 'FFREAD, DELTA T IN COLUMNS 23-25 OF THE FF ',
     &   'CARD MUST BE GREATER THAN ZERO.')
        iabort = 1
      endif
      write (outbuf, 10050) ilim
      call prtout(1)
      write (outbuf, 10060) toli
      call prtout(1)
      write (outbuf, 10070) tsim
      call prtout(1)
      write (outbuf, 10080) dt
      call prtout(1)
      write (outbuf, 10090) endt
      call prtout(1)
      write (outbuf, 10100) delang
      call prtout(1)
      write (outbuf, 10110) dcitol
      call prtout(1)
      write (outbuf, 10120) dmpmlt
      call prtout(1)
      write (outbuf, 10130) dtc
      call prtout(1)
      write (outbuf, 10140) istp
      call prtout(1)
      write (outbuf, 10150) frqbse
      call prtout(1)
10050 format (1x, 'ILIM       =', i11)
10060 format (1x, 'TOLI       =', f11.7)
10070 format (1x, 'START TIME =', f11.2)
10080 format (1x, 'DELTA TIME =', f11.2)
10090 format (1x, 'END   TIME =', f11.2)
10100 format (1x, 'DELANG     =', f11.7)
10110 format (1x, 'DCITOL     =', f11.7)
10120 format (1x, 'DMPMLT     =', f11.7)
10130 format (1x, 'DTC        =', f11.7)
10140 format (1x, 'ISTP       =', i11)
10150 format (1x, 'BASE HZ    =', f11.2)
      return
      end
