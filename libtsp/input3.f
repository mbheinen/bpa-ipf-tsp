C    %W% %G%
      subroutine input3
 
C     This subroutine will process machine input data as follows--
C       1. Check user input errors both fatal and others.
C       2. If no fatal errors, prepare modified data tables in mass
C          storage-to be revived later-and list m/c details.
C       3. if fatal errors then abort job after INITIL1.
C
C     It calls subroutines WRITE, SVSINP, SVSSS1, SVSSS2, TSLINP,
C     and subroutines INP3FA through INP3FL.
 
C     Revs:
C     Sep/16/92 - DEM
C     Changed PSS transducer gain data check logic to allow negative
C     voltage gain, and issue warning on negative frequency gain.
C     Mar/11/92 - DEM
C     Added program stop at end of routine if IABORT flag set
 
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/tzro.inc'
      include 'tspinc/reread.inc'
      include 'tspinc/param.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/inp3.inc'
      include 'tspinc/inp3a.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/lnk33n.inc'
      include 'tspinc/packtn.inc'
      include 'tspinc/ffcard.inc'
      include 'tspinc/amorts.inc'
 
      dimension bkvtp(200), bkvpp(200)
      character*8 nametp(200), namepp(200)
      character*1 nidtp(200), nidpp(200), subtyp2
      character*10 work(8, 100)
      equivalence (work, work80)
      dimension cgen(150), cgov(32), creg(35), csupp(42), tem(6), icgen
     &          (150), msub(8), temp(16), buff(8), ttemp(2)
      equivalence (cgen(1), icgen(1))
C     -  Local variables
      character*10 typeb
      character*10 typ
      character*1 id, type
      character*8 nhibus, nfqbus, nvfbus
      character*8 name1, name2
      character*1 id1, id2
      character temp2c*3, temp3c*3, temp4c*4
      character ch20*20
      logical debug/.false./
C     -     Begin     Begin     Begin     Begin     Begin     Begin
      call mpost('INPUT3')
      write (outbuf, 10000)
      call prtout(1)
      call skipln(1)
10000 format ('0 SUBROUTINE INPUT3')
      imchnt = 0
      imchnp = 0
      if (nodq .gt. 0) then
	write (errbuf(1), 10010)
10010   format ('0 ALL GENERATORS ARE CLASSICAL ')
	call prterr('W', 1)
      endif
      if (nogv .gt. 0) then
	write (errbuf(1), 10020)
10020   format ('0 ALL GOVERNORS ARE DISABLED ')
	call prterr('W', 1)
      endif
      if (noex .gt. 0) then
	write (errbuf(1), 10030)
10030   format ('0 ALL EXCITERS ARE DISABLED ')
	call prterr('W', 1)
      endif
      if (nosc .gt. 0) then
	write (errbuf(1), 10040)
10040   format ('0 ALL SUPPLEMENTARY CONTROLS ARE DISABLED ')
	call prterr('W', 1)
      endif
      if (infbus .gt. 0) then
	write (errbuf(1), 10050)
10050   format (
     &   '0 ALL MACHINES ARE INFINITE BUSES. CHECK COLUMN 79 ON the "ff"
     & card'
     &   )
	call prterr('W', 1)
	nodq = 1
	nogv = 1
	noex = 1
	nosc = 1
      endif
 
C     TLIM IS SMALLEST VALUE OF TIME CONSTANT PERMITTED IN THE DATA.
C     ANYTHING LESS THAN THAT WILL BE SET TO ZERO.
 
      tlim = dt/(10.0*frqbse)
      isgg = 0
      ksv = 0
      ksvtot = 0
C     -  Fetch starting data record for gens in file 1
      call whrec1('MAC ', ilo, irmac, isz)
      irmac = irmac + 1
C     IMAC=101
      ibloc = 100
      ilast = mac/100 + irmac
C     ILAST=MAC/100+102
      lmod = mod(mac, 100)
      if (lmod .eq. 0) ilast = ilast - 1
      ikount = 0
      kgen = 0
      kmtr = 0
      ijbusm = 0
      imc = 1
      iflag = 1
      itsl = 0
      anf = 0.0
      bnf = 0.0
      cnf = 0.0
      dnf = 0.0
      xdpp = 0.0
      xqpp = 0.0
      tdodp = 0.0
      tqodp = 0.0
      newex = 1
 
      ig = irmac - 1
  100 ig = ig + 1
      if (ig .gt. ilast) goto 330
 
C     do 5520 ig=irmac,ilast
C     DO 5520 IG=102,ILAST
 
      if (ig .eq. ilast .and. lmod .ne. 0) ibloc = lmod
      ibloc8 = ibloc*8
      read (l1, rec = ig) ((work(i, j), i = 1, 8), j = 1, ibloc)
 
      icard = 0
  110 icard = icard + 1
      if (icard .gt. ibloc) goto 100
 
C     do 5500 icard=1,ibloc
 
      read (work80(icard), 10060) type,subtyp,subtyp2,nbname,bkv,nid
10060 format (bz, 3a1, a8, f4.0, a1)
      if (imc .eq. 1) goto 140
      if (nbnamo .ne. nbname .or. bkvo .ne. bkv) then
	xdpp = 0.0
	xqpp = 0.0
	tdodp = 0.0
	tqodp = 0.0
 
C       THIS IS A NEW BUS   ***
 
	newbs = 1
	if (newex .ne. 1) then
	  write (errbuf(1), 10770) nbnamo, bkvo, ido
	  call prterr('E', 1)
	  imchn = 3
	  newex = 1
	  goto 110
	elseif (ijbusm .ne. 0) then
	  ijbusm = 0
	  goto 140
	endif
      elseif (ijbusm .eq. 1) then
	goto 110
      elseif (ido .eq. nid) then
	goto 160
      else
C       THIS IS ANOTHER MACHINE ON THE SAME BUS   ****
	xdpp = 0.0
	xqpp = 0.0
	tdodp = 0.0
	tqodp = 0.0
	newid = 1
      endif
  120 if (igr .ne. 0) then
	lgnth = igr + igv + iex + isup
	igentn(2, isgg) = lgnth
C       *** wlp extend write
	write (l2, rec = isgg) icgenn, (icgen(nnn), nnn = 1, lgnth)
C       *** end wlp
	if (keybrd(23) .ne. 0) then
	  write (outbuf, 10080) nbnamo, bkvo, ido
	  call prtout(1)
10080     format (1x, 25x, ' GENERATOR ', a8, 2x, f5.1, 2x, a1)
	  write (outbuf, 10090) ((icgenn(nnn, kz), nnn = 1, 2), kz = 1,
     &     4)
	  call prtout(1)
10090     format ('0', 1x, 'ICGENN', 1x, 8i5)
	  write (outbuf, 10100)
10100     format ('0', 30x, ' PRINTING GENERATOR TABLE ')
	  call skipln(1)
	  do jjj = 5, igr, 8
	    kkk = min0(jjj+7, igr)
	    write (outbuf, 10110) (cgen(i), i = jjj, kkk)
10110       format (8(2x, e13.5))
	    call prtout(1)
	  enddo
	  if (igv .eq. 0) then
	    write (outbuf, 10120)
10120       format ('0', 30x, ' PRINTING GOVERNOR TABLE ')
	    call prtout(1)
	    call skipln(1)
	    do jjj = 1, igv, 8
	      kkk = min0(jjj+7, igv)
	      write (outbuf, 10130) (cgen(i+igr), i = jjj, kkk)
10130         format (8(2x, e13.5))
	      call prtout(1)
	    enddo
	  endif
	  if (iex .eq. 0) then
	    write (outbuf, 10140)
10140       format ('0', 30x, ' PRINTING EXCITER TABLE ')
	    call prtout(1)
	    call skipln(1)
	    do jjj = 1, iex, 8
	      kkk = min0(jjj+7, iex)
	      write (outbuf, 10150) (cgen(i+igg), i = jjj, kkk)
10150         format (8(2x, e13.5))
	      call prtout(1)
	    enddo
	  endif
	  if (isup .eq. 0) then
	    write (outbuf, 10160)
10160       format ('0', 30x, ' PRINTING SUPP. CONTROL TABLE ')
	    call prtout(1)
	    call skipln(1)
	    do jjj = 1, isup, 8
	      kkk = min0(jjj+7, isup)
	      write (outbuf, 10170) (cgen(i+igge), i = jjj, kkk)
10170         format (8(2x, e13.5))
	      call prtout(1)
	    enddo
	  endif
	endif
	if (imchn .eq. 0) then
	  if (noprnt .ne. 0) call writem(isgg)
	  goto 130
	endif
      endif
      iabort = 1
      if (ibasic .ne. 0) then
	write (errbuf(1), 10180) nbnamo, bkvo, ido
10180   format ('0 MC, MF, or MI card missing for ', a8, 2x, f5.1, 2x,
     &   a1, '.  Other data cards for this machine not processed')
	call prterr('E', 1)
      endif
      write (errbuf(1), 10190) work80(icard)
10190 format (' Error processing ', a)
      call prterr('E', 1)
  130 continue
      if (iflag .ne. 0) then
	if (newbs .eq. 0) then
	  if (newid .eq. 1) goto 150
	  goto 340
	endif
      endif
      if (ikount .ne. 0) then
	iabort = 1
	write (errbuf(1), 10200) nbnamo, bkvo
	call prterr('E', 1)
10200   format ('0 BOTH HP AND LP MACHINES MUST EXIST TOGETHER AT BUS '
     &   , a8, 2x, f5.1)
      endif
      if (abs(pold-1.) .ge. 0.01 .or. abs(qold-1.) .ge. 0.01) then
	if (kmtr .ne. 1 .or. kgen .eq. 1) then
	  iabort = 1
	  write (errbuf(1), 10210) nbnamo, bkvo
	  call prterr('E', 1)
10210     format ('0 OUTPUTS OF ALL MACHINES (EXCEPT IND.MTR.) ON ',
     &     a8, 2x, f5.1, ' MUST ADD UP TO 100 PER CENT.')
	endif
      endif
      if (iflag .ne. 1) goto 340
  140 newbs = 0
      imc = 0
      nbnamo = nbname
      bkvo = bkv
      pold = 0.
      qold = 0.
      kgen = 0
      kmtr = 0
      kb = nambas(bkv)
      buffer = work80(icard)
      jbusm = inam(nbname, kb)
      if (jbusm .le. 0) then
	ijbusm = 1
	goto 110
      endif
  150 newid = 0
      ijbusm = 0
      ido = nid
      ibasic = 1
      igr = 0
  160 if (newex .ne. 1 .and. subtyp .ne. 'X') then
	write (errbuf(1), 10770) nbname, bkv, nid
	call prterr('E', 1)
	imchn = 3
	newex = 1
      else
	if (type .eq. 'M') then
	  if (subtyp .eq. ' ') then
	    ijk = 1
	  elseif (subtyp .ne. 'Z') then
	    ijk = 2
	    ibasic = 0
	    if (nid .eq. 'H') ikount = 1
	    if (nid .eq. 'L') ikount = ikount - 1
	  elseif (ibasic .eq. 0) then
	    ijk = 3
	    goto 170
	  else
	    goto 110
	  endif
	elseif (type .eq. 'V') then
	  ijk = 8

csw       trap to avoid initializing counters for VN2 or VW2 card
          if (subtyp2 .eq. '2') then
            if (ibasic .ne. 0) goto 110
            goto 170
          endif
csw end
	  ibasic = 0
	elseif (type .eq. 'G') then
	  if (ibasic .ne. 0) goto 110
	  ijk = 4
	  goto 170
	elseif (type .eq. 'T') then
	  if (ibasic .ne. 0) goto 110
	  ijk = 5
	  goto 170
	elseif (type .ne. 'E') then
	  if (type .ne. 'S') then
	    if (type .eq. 'W') then
	      if (ibasic .ne. 0) goto 110
	      ijk = 9
	      goto 170
	    elseif (type .eq. 'X') then
	      if (ibasic .ne. 0) goto 110
	      ijk = 10
	      goto 170
	    endif
	  endif
	  if (ibasic .ne. 0) goto 110
	  ijk = 7
	  goto 170
	elseif (ibasic .eq. 0) then
	  ijk = 6
	  goto 170
	else
	  goto 110
	endif
	mgen = 0
	mex = 0
	msupp = 0
	igt = 0
	imfs = 0
	ldamp = 0
	iutrn = 0
	ifdam = 0
	mdl = 0
	ipd = 0
	jnd = 0
	idc = 0
	igr = 0
	igv = 0
	iex = 0
	isup = 0
	isupp = 0
	igov = 0
	imchn = 0
	imchns = 0
	iabbrt = 0
	indmm = 0
	notbl = 0
	jbushi = 0
	jbusfq = 0
	jbusvq = 0
	omegaf = 0.
	encnst = 0.
	rconst = 0.
  170   if (ijk .eq. 2) then
	  if (subtyp .eq. 'I') then
 
C           ERROR CHECK ON MSCF CARD ENDS. BEGIN PROCESSING M(I) CARD.
 
C           DECODE INDUCTION MOTOR DATA CARD
	    read (work80(icard), 10220) (temp(i), i = 1, 13)
10220       format (bz, 16x, f6.0, 2f3.2, f4.0, 5f5.4, f3.2, f4.2,
     &       2f5.4)
	    if (temp(3) .le. 0.0) temp(3) = 1.0
	    read (work80(icard), 10230) temp2c, temp3c, temp4c
10230       format (22x, 2a3, a4)
	    kmtr = 1
	    if (temp(1) .le. 0.0) then
	      imchn = 1
	      write (outbuf, 10330) nbname, bkv, nid
	      call prtout(1)
	    endif
	    if (temp(8) .le. 0.0) then
	      imchn = 1
	      write (errbuf(1), 10240) nbname, bkv, nid
	      call prterr('E', 1)
10240         format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &         ' EITHER SLIP OR ROTOR RESISTAN    ce must be positive '
     &         )
	    endif
	    if (temp(5) .ge. 0.0 .and. temp(6) .ge. 0.0 .and.
     &          temp(9) .ge. 0.0) goto 180
	    imchn = 1
	    write (errbuf(1), 10250) nbname, bkv, nid
	    call prterr('E', 1)
10250       format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &       ' CHECK RS,XS,XR FOR -VE SIGNS')
  180       mgen = 2
	    igt = 3
	    if (temp(2) .lt. 0.0) indmm = 1
	    if (temp(3) .lt. 0.0) indmm = 1
	    if (temp(2) .eq. 0.0) then
	      if (temp2c .eq. ' ') indmm = 1
	    endif
	    if (temp(3) .eq. 0.0) then
	      if (temp3c .eq. ' ') indmm = 1
	    endif
	    if (indmm .ne. 0) then
	      imchn = 1
	      write (errbuf(1), 10260) nbname, bkv, nid
	      call prterr('E', 1)
10260         format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &         ' P AND Q MUST HAVE A POSITIVE     punched entry ')
	    endif
	    bmvamc = temp(4)
	    if (bmvamc .gt. 0.0) then
	      bmvac = bmva/bmvamc
	      do j = 5, 9
		temp(j) = temp(j)*bmvac
	      enddo
	    else
	      if (bmvamc .eq. 0.0) then
		if (temp4c .eq. ' ') goto 220
	      endif
	      imchn = 1
	      write (outbuf, 10410) nbname, bkv, nid
	      call prterr('E', 1)
	    endif
	  else
C           DECODE MC,MF,MS TYPE MACHINE INPUT DATA CARD
	    read (work80(icard), 10270) typ
10270       format (70x, a10)
	    read (work80(icard), 10280) (temp(i), i = 1, 15)
10280       format (bz, 16x, f6.0, 2f3.2, f4.0, f4.4, 4f5.4, f4.2,
     &       f3.2, 2f5.4, f4.3, f3.2)
	    read (work80(icard), 10290) temp2c, temp3c, temp4c
10290       format (22x, 2a3, a4)
	    if (dmpall .ne. 0.0) temp(15) = dmpall
	    kgen = 1
 
C           CHECK FOR DATA ERRORS ON MSCF CARDS--BEGIN
 
C           CHECK FOR PROPER VALUE OF XD'
	    if (temp(6) .le. 0.0) then
	      imchn = 1
	      write (errbuf(1), 10300) nbname, bkv, nid
	      call prterr('E', 1)
10300         format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &         ' XDP IS -VE OR ZERO ')
	    endif
 
C           EMWS SHOULD BE 0 FOR SUB TYPE S
 
	    if (subtyp .eq. 'S') then
	      if (temp(1) .ne. 0.0) then
		imchn = 1
		write (errbuf(1), 10310) nbname, bkv, nid
		call prterr('E', 1)
10310           format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &           ' INERTIA CONST. NOT ZERO ')
	      endif
C             INERTIALESS MACHINE REPRESENTATION
	      igt = 2
	      mgen = 1
	    else
 
C             EMWS = 999999. INDICATES INFINITE INERTIA REPRESENTATION
 
	      if (temp(1) .eq. 999999.) then
		write (errbuf(1), 10320) nbname, bkv, nid
		call prterr('W', 1)
10320           format ('0 ********* GENERATOR ', a8, 2x, f5.1, 2x, a1,
     &           ' IS AN ', 'INFINITE BUS **********')
	      elseif (temp(1) .le. 0.0) then
		imchn = 1
		write (errbuf(1), 10330) nbname, bkv, nid
		call prterr('E', 1)
10330           format (3x, a8, 2x, f5.1, 2x, a1, 5x, ' INERTIA CONST.'
     &           , '-VE OR ZERO ')
	      endif
 
C             D,Q REPRESENTATION BEING DISABLED
 
	      if (nodq .le. 0) then
		if (subtyp .eq. 'F') then
C                 D Q  ( TWO AXES ) MACHINE REPRESENTATION
		  if (temp(8) .le. 0.0 .or. temp(8) .le. temp(6)) then
		    imchn = 1
		    write (errbuf(1), 10340) nbname, bkv, nid
		    call prterr('E', 1)
10340               format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &               '  XD MUST BE +VE AND .GT.XDP ')
		  endif
C                 IF TQO' = 0, SET XQ' = XQ   I.E.,NO 'G' COIL
		  if (temp(11) .eq. 0.0) temp(7) = temp(9)
		  if (temp(7) .le. 0.0 .or. temp(9) .le. 0.0 .or.
     &                temp(9) .lt. temp(7)) then
		    imchn = 1
		    write (errbuf(1), 10350) nbname, bkv, nid
		    call prterr('E', 1)
10350               format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &               ' BOTH XQ,XQP MUST BE +VE AND X    Q.GE.XQP  ')
		  endif
C                 IF XQ' = XQ, SET TQO' = 0   I.E., NO 'G' COIL
		  if (temp(7) .eq. temp(9)) temp(11) = 0.
C                 TDO' = 999. MEANS CONSTANT FLUX LINKAGE REPRESENTATION
 
C                 IF IAMRTS .NE. 0.0 (READ FROM THE F1 CARD), ALL TWO AX
C                 MACHINES WILL USE DEFAULT DATA FOR DAMPER WINDING REPR
C                 EVEN IF DAMPER WINDING DATA WAS ENTERED ON THEIR DATA
 
		  if (xfact .gt. 0.0 .and. iamrts .ne. 0) then
 
C                   XDPP AND XQPP ARE A FRACTION OF XD PRIME TEMP(6)
 
		    xdpp = xfact*temp(6)
		    xqpp = xfact*temp(6)
 
C                   IF TQ PRIME IS ZERO USED HYDRO DATA, ELSE USE STEAM
 
		    if (temp(11) .eq. 0.0) then
		      tdodp = tdodph
		      tqodp = tqodph
		    else
		      tdodp = tdodps
		      tqodp = tqodps
		    endif
		  endif
 
C                 ENTER DEFAULT DAMPER WINDING DATA IF NONE IS ENTERED F
C                 THIS MACHINE.
 
		  if (tdodp .le. 0.0) then
		    if (xfact .gt. 0.0) then
		      xdpp = xfact*temp(6)
		      xqpp = xfact*temp(6)
		      if (temp(11) .eq. 0.0) then
			tdodp = tdodph
			tqodp = tqodph
		      else
			tdodp = tdodps
			tqodp = tqodps
		      endif
		    endif
		  endif
		  if (temp(10) .eq. 999.0) then
		    mgen = 3
		    igt = 4
C                   TDODP = 0 MEANS NO DAMPER WINDING DATA
		    if (tdodp .eq. 0.0) goto 210
		    mgen = 6
		    igt = 5
		    mdl = 0
		  else
		    if (temp(10) .le. 0.0) then
		      imchn = 1
		      write (errbuf(1), 10360) nbname, bkv, nid
		      call prterr('E', 1)
10360                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 ' TDOP MUST BE +VE FOR D,Q M/C')
		    endif
C                   CONST. FIELD VOLTAGE REPRESENTATION
		    mgen = 4
		    mdl = 11
		    nhibus = nbname
		    hbkv = bkv
		    igt = 4
C                   TDODP = 0 MEANS NO DAMPER WINDING DATA
		    if (tdodp .ne. 0.0) then
		      mgen = 7
		      igt = 5
		    endif
		    if (temp(12) .ge. 0.0) then
		      if (temp(13) .gt. 0.0 .and. 
     &                    temp(14) .gt. temp(13)) goto 190
		      if (temp(13) .eq. 0.0 .and. temp(14) .eq. 0.0)
     &                  then
C                       NO SATURATION DATA --- ASSUME NO SATURATION
			imfs = 0
			goto 200
		      endif
		    endif
		    imchn = 1
		    write (errbuf(1), 10370) nbname, bkv, nid
		    call prterr('E', 1)
10370               format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x,
     &               ' IMPROPER XL/XP OR SAT. DATA')
  190               imfs = 1
		  endif
C                 TDODP = 0 MEANS NO DAMPER WINDING DATA
  200             if (tdodp .ne. 0.0) then
C                   VALIDITY CHECK ON DAMPER WINDING DATA
		    if (tqodp .le. 0.0) then
		      imchn = 1
		      write (errbuf(1), 10380) nbname, bkv, nid
		      call prterr('E', 1)
10380                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 '  TQODP.LE. 0.01 ')
		    endif
		    if (tdodp .lt. tqodp) then
		      imchnt = imchnt + 1
		      nametp(imchnt) = nbname
		      bkvtp(imchnt) = bkv
		      nidtp(imchnt) = nid
		    endif
		    if (xqpp .le. 0. .or. xdpp .le. 0.0) then
		      imchn = 1
		      write (errbuf(1), 10390) nbname, bkv, nid
		      call prterr('E', 1)
10390                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 '  XQPP AND/OR XDPP .LE. 0.')
		    endif
		    if (xqpp .lt. xdpp) then
		      imchnp = imchnp + 1
		      namepp(imchnp) = nbname
		      bkvpp(imchnp) = bkv
		      nidpp(imchnp) = nid
		    endif
		    if (xdpp .ge. temp(6)) then
		      imchn = 1
		      write (errbuf(1), 10400) nbname, bkv, nid
		      call prterr('E', 1)
10400                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 '  XDP.LE.XDPP')
		    endif
C                   IF LEAKAGE REACTANCE GIVEN 0, SET IT EQUAL TO XD''
		    if (temp(12) .gt. xdpp) temp(12) = xdpp
		    if (temp(12) .eq. 0.0) temp(12) = xdpp
		  endif
		  goto 210
		endif
	      endif
C             CLASSICAL GENERATOR REPRESENTATION
	      igt = 1
	      mgen = 1
	    endif
 
C           UNIT CONVERSION FOR GENERATOR DAMPING FACTOR
C           DMPMLT IS THE DAMPING MULTIPLIER FROM THE FF CARD
C           DMPALL IS THE DEFAULT DAMPING FROM THE F1 CARD
 
  210       if (temp(15) .gt. 0.0) then
	      temp(15) = (temp(15)/6.2831853)
	      idc = 1
	    endif
	    if (temp(15) .gt. 0.0 .and. dmpall .eq. 0.0) 
     &        temp(15) = temp(15)*dmpmlt
	    bmvamc = temp(4)
	    if (bmvamc .gt. 0.0) then
	      bmvac = bmva/bmvamc
C             CONVERT MACHINE DATA TO NEW MVA BASE IF NECESSARY
	      do j = 5, 9
		temp(j) = temp(j)*(bmvac)
	      enddo
	      temp(12) = temp(12)*bmvac
	      if (tdodp .ne. 0.0) then
		xdpp = xdpp*bmvac
		xqpp = xqpp*bmvac
	      endif
	    elseif (temp4c .ne. ' ') then
	      write (errbuf(1), 10410) nbname, bkv, nid
	      call prterr('E', 1)
10410         format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &         'MVA BASE ENTRY ON MACHINE CARD     must be either blank
     &or positive'
     &         )
	      imchn = 1
	    endif
	    if (temp(2) .ge. 0.0) then
	      if (temp(3) .ge. 0.0) then
		if (temp(2) .eq. 0.0) then
		  if (temp2c .eq. ' ') temp(2) = 1.
		endif
		pold = pold + temp(2)
		frctnh = temp(2)
		if (temp(3) .eq. 0.0) then
		  if (temp3c .eq. ' ') temp(3) = 1.
		endif
		qold = qold + temp(3)
		goto 220
	      endif
	    endif
	    write (errbuf(1), 10420) nbname, bkv, nid
	    call prterr('E', 1)
10420       format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &       ' P AND Q MUST HAVE +VE SIGNS ')
	    imchn = 1
	  endif
 
C         ERROR CHECK AND READING OFF M/C CARDS IS COMPLETE. START MAKIN
C         BLES IF NO ERRORS
 
C         IMCHN=1 DENOTES ERROR FLAG FOR GENERATOR DATA ERRORS
  220     if (imchn .eq. 1) then
	    notbl = 1
	  else
 
C           WRITE M/C TABLE
 
C           ISGG IS A RUNNING COUNT OF DATA ERROR FREE GENERATORS.  IT A
C           BECOMES THE SERIAL NUMBER OF ALPHANUMERICALLY STACKED GENERA
C           IN IGENT TABLE.  JBUSM IS THE BUS NUMBER TO WHICH THE GENERA
C           IS CONNECTED.  IGENKT IS THE TOTAL NUMBER OF GENERATORS CONN
C           TO BUS NUMBER JBUSM
	    isgg = isgg + 1
	    igentn(1, isgg) = jbusm
	    igentn(2, isgg) = 0
	    igentc(isgg) = nid
	    if (ipactn(2, jbusm) .eq. 0) ipactn(2, jbusm) = isgg
	    igenkt = ipactn(1, jbusm)
	    ipactn(1, jbusm) = igenkt + 1
	    do i = 1, 150
	      cgen(i) = 0.
	    enddo
	    do j = 1, 4
	      do i = 1, 2
		icgenn(i, j) = 0
	      enddo
	    enddo
	    if (igt .ne. 2) then
	      if (igt .eq. 6) then
		do i = 1, 10
		  cgen(i+4) = temp(i)
		enddo
C               WRITE * * *
		igr = 27
		igg = 27
		goto 230
	      else
		cgen(5) = cvcon/temp(1)
		if (infbus .gt. 0) temp(1) = 999999.
		if (temp(1) .eq. 999999.) cgen(5) = temp(1)
	      endif
	    endif
	    if (mgen .eq. 2) then
	      do i = 6, 10
		ip = i - 1
		cgen(i) = temp(ip)
	      enddo
	      cgen(15) = temp(2)
	      cgen(16) = temp(3)
	      cgen(23) = 0.0
	      cgen(24) = 0.0
	      cgen(25) = temp(10)
	      cgen(26) = temp(11)*frqbse
	      cgen(12) = temp(12)
	      cgen(13) = temp(13)
	      igr = 26
	    else
	      cgen(6) = temp(6)
	      cgen(7) = temp(15)
	      if (mgen .eq. 1) then
		cgen(10) = temp(2)
		cgen(11) = temp(3)
		igr = 11
	      else
		cgen(8) = temp(5)
		cgen(9) = temp(8)
		cgen(10) = temp(9)
		cgen(11) = temp(7)
		cgen(12) = temp(12)
	        temp(10) = check_tc(temp(10), tlim, mgen, 'Td0', nbname,
     &               bkv, nid)
                if (temp(10) .ne. 0.0) temp(10) = 1.0/(frqbse*temp(10))
	        temp(11) = check_tc(temp(11), tlim, mgen, 'Tq0', nbname,
     &               bkv, nid)
                if (temp(11) .ne. 0.0) temp(11) = 1.0/(frqbse*temp(11))
		cgen(15) = temp(11)
		cgen(16) = temp(10)
		if (imfs .ne. 0) then
		  s10 = temp(13)
		  s12 = temp(14)
		  esat = (2.4*(s12-s10)-sqrt(.192*s12*s10))/(2.*(1.2
     &             *s12-s10))
		  csat = s10/((1.-esat)**2)
		  cgen(13) = esat
		  cgen(14) = csat
		endif
		cgen(26) = temp(2)
		cgen(27) = temp(3)
		igr = 27
		if (tdodp .ne. 0.0) then
		  cgen(28) = xdpp
		  cgen(29) = xqpp
		  cgen(30) = tdodp
		  cgen(31) = tqodp
		  igr = 39
		endif
	      endif
	    endif
  230       icgenn(1, 1) = mgen
	    icgenn(2, 1) = 0
	  endif
	elseif (ijk .eq. 3) then
 
C         DECODE MZ CARD --- SPECIAL FOR BPA PROGRAM.   THIS CARD CONTAI
C         INFORMATION ABOUT EXTERNAL TRANSFORMER TO BE INSERTED BETWEEN
C         A MACHINE AND IT'S BUS, BUS NAMES FOR REMOTE SIGNAL SOURCES FO
C         VOLTAGE REGULATION, PSS AND NOTCH FILTER DATA.  BLANK BUS NAME
C         IMPLIES MACHINE BUS USED FOR THE PARTICULAR APPLICATION
 
	  read (work80(icard), 10430) nhibus, hbkv, ttemp(1), ttemp(2),
     &     nfqbus, fbkv, nvfbus, vbkv, anf, bnf, cnf, dnf
10430     format (bz, 16x, a8, f4.0, f5.4, f4.4, 1x, 2(1x, a8, f4.0),
     &     4f4.2)
	  ipfbus = 0
	  if (ttemp(1) .le. 0.0) xt = 0.
	  if (ttemp(2) .le. 0.0) rt = 0.
	  if (ttemp(1) .ne. 0.0 .or. ttemp(2) .ne. 0.0) then
	    iutrn = 1
	    xt = ttemp(1)
	    rt = ttemp(2)
	  endif
	  if (imchn .ne. 1) then
	    if (nhibus .eq. '        ') then
	      jbushi = jbusm
	    else
	      kb = nambas(hbkv)
	      jbushi = inam(nhibus, kb)
	      if (jbushi .le. 0) ipfbus = 1
	      icgenn(2, 1) = jbushi
	    endif
	    if (nfqbus .eq. ' ') then
	      jbusfq = 0
	    else
	      kb = nambas(fbkv)
	      jbusfq = inam(nfqbus, kb)
	      if (jbusfq .le. 0) ipfbus = 1
	    endif
	    if (nvfbus .eq. ' ') then
	      jbusvq = 0
	    else
	      kb = nambas(vbkv)
	      jbusvq = inam(nvfbus, kb)
	      if (jbusvq .le. 0) ipfbus = 1
	    endif
	    if (ipfbus .ne. 0) then
	      write (errbuf(1), 10440) nbname, bkv, nid
	      call prterr('E', 1)
10440         format ('0 BUS NAME ON MZ OR SF CARD FOR (', a8, 2x,
     &         f5.1, 2x, a1, ')    is not in power flow')
	      imchn = 5
	    elseif (mgen .eq. 2) then
	      cgen(17) = xt
	      cgen(18) = rt
	    elseif (mgen .ne. 9) then
 
C             ADD TRANSFORMER DATA TO THE M/C TABLE
 
	      cgen(6) = cgen(6) + xt
	      if (mgen .eq. 1) then
		cgen(8) = xt
		cgen(9) = rt
	      else
		cgen(24) = xt
		cgen(25) = rt
		cgen(8) = cgen(8) + rt
		do i = 9, 12
		  cgen(i) = cgen(i) + xt
		enddo
	      endif
	    endif
	  endif
	else
	  if (ijk .eq. 4) then
	    if (mgen .eq. 2) goto 110
	    if (igt .eq. 2) goto 110
	    if (nogv .gt. 0) goto 110

c           process governor input
            call govinp(isgg, work80(icard), bmva)

	  elseif (ijk .ne. 5) then
	    if (ijk .eq. 6) then
	      if (mgen .lt. 4) then
		imfs = 0
	      elseif (noex .le. 0) then
 
C               DECODING EXCITATION SYSTEM DATA CARD
 
		if (newex .eq. 1) then
		  if (subtyp .eq. 'M') then
		    mex = 11
		    mdl = 22
		    newex = 2
		    call inp3fa()
		  elseif (subtyp .eq. 'N') then
		    mex = 12
		    mdl = 12
		    newex = 3
		    call inp3fb()
		  elseif (subtyp .eq. 'O') then
		    mex = 13
		    mdl = 13
		    newex = 4
		    call inp3fc()
		  elseif (subtyp .eq. 'P') then
		    mex = 14
		    mdl = 14
		    newex = 5
		    call inp3fd()
		  elseif (subtyp .eq. 'Q') then
		    mex = 15
		    mdl = 15
		    newex = 6
		    call inp3fe()
		  elseif (subtyp .eq. 'R') then
		    mex = 16
		    mdl = 16
		    newex = 7
		    call inp3ff()
		  elseif (subtyp .eq. 'S') then
		    mex = 17
		    mdl = 17
		    newex = 8
		    call inp3fg()
		  elseif (subtyp .eq. 'T') then
		    mex = 18
		    mdl = 18
		    newex = 9
		    call inp3fh()
		  elseif (subtyp .eq. 'U') then
		    mex = 19
		    mdl = 19
		    newex = 10
		    call inp3fj()
		  elseif (subtyp .eq. 'V') then
		    mex = 20
		    mdl = 20
		    newex = 11
		    call inp3fk()
		  elseif (subtyp .eq. 'W') then
		    mex = 21
		    mdl = 21
		    newex = 12
		    call inp3fl()
		  elseif (subtyp .eq. 'X') then
		    write (errbuf(1), 10600) work80(icard)
10600               format (1x, a8)
		    write (errbuf(2), 10610)
10610               format (' This card is ignored because there is no'
     &               , ' corresponding exciter card')
		    call prterr('E', 2)
		    imchn = 3
		    goto 110
		  else
		    read (work80(icard), 10620) subtyp, (temp(i), i =
     &               1, 15)
10620               format (bz, 1x, a1, 14x, f4.3, f5.2, f4.2, f4.3,
     &               f4.2, 4f4.3, f5.3, 3f4.3, 2f5.4)
 
C                   IF TR .LT. TZERO, SET TO DEFAULT OF TZERO
 
		    if (temp(1) .lt. tzero) then
		      temp(1) = tzero
		      write (errbuf(1), 10630) nbname, bkv, nid, temp
     &                 (1)
10630                 format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 'TR IS BEING SET', ' EQUAL TO: ', f7.5)
		      call prterr('W', 1)
		    endif
		    temp(3) = check_tc(temp(3), tlim, 3, 'TR', nbname,
     &               bkv, nid)
		    if (temp(2) .le. 0.0) then
		      imchn = 3
		      write (errbuf(1), 10640) nbname, bkv, nid
10640                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 ' GAIN KA IS -VE OR ZERO ')
		      call prterr('E', 1)
		    endif
		    if (subtyp .ne. 'E') then
		      temp(13) = check_tc(temp(13), tlim, 13, 'TR',
     &                 nbname, bkv, nid)
		      if (temp(12) .gt. 0.0 .and. temp(13) .eq. 0.0)
     &                 then
			imchn = 3
			write (errbuf(1), 10650) nbname, bkv, nid
			call prterr('E', 1)
10650                   format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                   ' WHEN TF IS 0. KF MUST BE 0. ')
		      endif
		      if (subtyp .eq. 'J') goto 270
		      if (subtyp .eq. 'D') goto 260
		    elseif (temp(12) .le. 0.0) then
		      write (errbuf(1), 10660) nbname, bkv, nid
		      call prterr('E', 1)
10660                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 'K"A FOR TYPE E EXCITER MUST BE     greater than
     &zero.'
     &                 )
		    elseif (temp(3) .ne. 0.0 .or. temp(12) .ge. 10.0) 
     &                then
		      if (temp(3) .le. 0.0 .or. temp(12) .le. 10.0) then
			imchn = 3
			write (errbuf(1), 10670) nbname, bkv, nid
			call prterr('E', 1)
10670                   format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                   ' COMBINATION OF TRH AND K"A FO    r type e exc
     &iter is improper'
     &                   )
		      endif
		    endif
		    if (temp(11) .le. temp(10) .or. temp(11) .le. 0.0) 
     &                then
		      imchn = 3
		      write (errbuf(1), 10680) nbname, bkv, nid
		      call prterr('E', 1)
10680                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 ' IMPROPER FIELD VOLT LIMITS ')
		    endif
		    if (subtyp .eq. 'G' .or. subtyp .eq. 'H') then
		      if (subtyp .eq. 'G') then
			mex = 7
			mdl = 7
		      else
			if (temp(5) .ge. 0.0) then
			  imchn = 3
			  write (outbuf, 10730)
			  call prtout(1)
			endif
			if (temp(6) .le. 0.0) then
			  imchn = 3
			  write (errbuf(1), 10690) nbname, bkv, nid
			  call prterr('E', 1)
10690                     format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                     ' GAIN KE1 -VE OR ZERO ')
			endif
			mex = 8
			mdl = 8
		      endif
		      goto 290
		    else
		      temp(7) = check_tc(temp(7), tlim, 7, 'GH',
     &                 nbname, bkv, nid)
		      if (temp(7) .le. 0.0) then
			imchn = 3
			write (errbuf(1), 10700) nbname, bkv, nid
			call prterr('E', 1)
10700                   format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                   ' TIME CONST. TE -VE OR ZERO ')
		      endif
		      if (temp(8) .le. 0.0 .or. temp(9) .le. temp(8)) 
     &                  then
			imchn = 3
			write (errbuf(1), 10710) nbname, bkv, nid
			call prterr('E', 1)
10710                   format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                   'SE75MAX SHOULD BE GREATER THAN     zero but le
     &ss than semax '
     &                   )
		      endif
		      if (subtyp .eq. 'A' .or. subtyp .eq. 'E' .or.
     &                    subtyp .eq. 'K') goto 280
		    endif
  260               if (temp(6) .ne. 1) then
		      imchn = 3
		      write (errbuf(1), 10720) nbname, bkv, nid
		      call prterr('E', 1)
10720                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 ' GAIN KE SHOULD BE ONE ')
		    endif
		    if (subtyp .eq. 'B') then
		      mex = 2
		      mdl = 2
		      goto 290
		    elseif (subtyp .eq. 'C') then
		      mex = 3
		      mdl = 3
		      goto 290
		    elseif (subtyp .eq. 'F') then
		      mex = 6
		      mdl = 6
		      goto 290
		    elseif (subtyp .ne. 'D') then
		      goto 280
		    endif
C                   TEST NEW IEEE EXCITER MODEL SUBTYPES
  270               if (temp(5) .ge. 0.0) then
		      imchn = 3
		      write (errbuf(1), 10730) nbname, bkv, nid
		      call prterr('E', 1)
10730                 format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                 ' VRMIN NOT NEGATIVE ')
		    endif
		    if (subtyp .eq. 'D') then
		      if (temp(8) .eq. 0.0 .and. temp(9) .eq. 0.0) then
			imchn = 3
			write (errbuf(1), 10740) nbname, bkv, nid
			call prterr('E', 1)
10740                   format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                   ' BOTH KP AND KI ARE ZERO ')
		      endif
		      if (temp(11) .le. 0.0) then
			imchn = 3
			write (errbuf(1), 10750) nbname, bkv, nid
			call prterr('E', 1)
10750                   format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                   ' VBMAX NOT POSITIVE ')
		      endif
		      temp(7) = check_tc(temp(7), tlim, 7, 'TE',
     &                 nbname, bkv, nid)
		      if (temp(7) .le. 0.0) then
			imchn = 3
			write (outbuf, 10700) nbname, bkv, nid
			call prtout(1)
		      endif
		      if (temp(14) .le. 0.0) then
			write (errbuf(1), 10760) nbname, bkv, nid
			call prterr('E', 1)
10760                   format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                   ' COMMUTATING REACTANCE - XL MU    st be positi
     &ve for type d exciter '
     &                   )
			imchn = 3
		      endif
		      mex = 4
		      mdl = 4
		    else
		      if (temp(11) .le. temp(10)) then
			imchn = 3
			write (outbuf, 10680) nbname, bkv, nid
			call prtout(1)
		      endif
		      mex = 9
		      mdl = 9
		    endif
		    goto 290
  280               if (subtyp .eq. 'A') then
C                     ASSIGN NUMERICAL VALUES TO DIFFERENT EXCITER TYPES
		      mex = 1
		      mdl = 1
		    elseif (subtyp .eq. 'K') then
		      mex = 10
		      mdl = 10
		    else
		      mex = 5
		      mdl = 5
		    endif
		  endif
  290             mgen = 5
C                 IGR .GT. 27 IS A MACHINE WITH DAMPER WINDINGS REPRESEN
		  if (igr .gt. 27) mgen = 8
		  icgenn(1, 1) = mgen
		  if (imchn .ne. 3) then
		    if (notbl .eq. 0) then
		      iex = 5
		      if (mex .lt. 11) iex = 35
		      do i = 1, iex
			creg(i) = 0.0
		      enddo
		      if (mex .lt. 11) then
			tem(1) = temp(1)
			tem(2) = temp(3)
			tem(3) = temp(4)
			tem(4) = temp(7)
			tem(5) = temp(13)
			tem(6) = temp(15)
			do i = 1, 6
			  tem(i) = check_tc(tem(i), tlim, i, 'Exciter',
     &                     nbname, bkv, nid)
 
C                         Modify exciter time constants for trapezoidal
 
			  if (tem(i) .gt. 0.0) tem(i) = tem(i)*frqbse
			enddo
			creg(8) = temp(2)
			if (mex .eq. 6) creg(8) = creg(8)/frqbse
			creg(1) = temp(11)
			vfmax = temp(11)
			creg(2) = temp(10)
			vfmin = temp(10)
			if (mex .eq. 5) then
			  creg(8) = temp(12)
			  if (creg(8) .lt. 10.0) 
     &                      creg(8) = creg(8) / frqbse
			  creg(10) = temp(2)
			else
			  creg(10) = tem(5)
			  creg(6) = temp(12)*frqbse
			  if (mex .eq. 6) creg(14) = creg(8)*tem(2)
			endif
			creg(12) = tem(2)
			if (mex .ne. 7) then
			  if (mex .ne. 9) then
			    creg(7) = temp(6)
			    if (mex .eq. 8) then
			      creg(9) = temp(14)
			    else
			      creg(9) = tem(4)
			      if (mex .ne. 2) then
				if (mex .eq. 4) then
				  creg(14) = temp(11)
				  creg(4) = temp(9)
				  creg(5) = temp(8)
				  if (bmvamc .gt. 0.0) 
     &                              creg(5) = creg(5) * bmvac
				  creg(2) = temp(5)
				  creg(1) =  - temp(5)
				  creg(20) = temp(14)
				endif
				goto 300
			      endif
			    endif
			    creg(14) = tem(6)
			  endif
			endif
  300                   creg(11) = tem(3)
			creg(13) = tem(1)
			if (mex .ne. 4 .and. mex .ne. 7 .and. 
     &                      mex .ne. 8 .and. mex .ne. 9) then
			  sd75 = temp(8)
			  sd100 = temp(9)
 
C                         CALCULATE SATURATION CONSTANTS FROM MAXMIMUM A
 
			  vamax = amax1(vfmax, -vfmin)
 
			  esat = (alog(sd100/sd75))/(.25*vamax)
			  csat = sd100/(exp(esat*vamax))
			endif
 
C                       VOLTAGE REGULATOR LIMITS COULD BE A FUNCTION OF
C                       WILL BE COMPUTED LATER TEMPORARILY FIELD VOLTAGE
 
			if (mex .ne. 4 .and. mex .ne. 7) then
			  if (mex .eq. 8) then
			    creg(2) = temp(5)
			    creg(1) =  - temp(5)
			    creg(4) = temp(11)
			    creg(5) = temp(10)
			  elseif (mex .eq. 9) then
			    creg(2) = temp(5)
			    creg(1) =  - temp(5)
			    creg(4) = temp(11)
			    creg(5) = temp(10)
			    creg(7) = temp(8)
			  else
			    creg(4) = esat
			    creg(5) = csat
			    creg(27) = temp(5)
			  endif
			endif
			goto 310
		      else
			nmex = mex - 10
			goto 110
		      endif
		    endif
		  endif
		  notbl = 2
		  goto 110
		elseif (subtyp .ne. 'X') then
		  write (errbuf(1), 10770) nbname, bkv, nid
10770             format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x,
     &             'FZ card missing')
		  call prterr('E', 1)
		  newex = 1
		  imchn = 3
		  goto 110
		elseif (newex .eq. 2) then
		  newex = 1
		  call inp3fa()
		elseif (newex .eq. 3) then
		  newex = 1
		  call inp3fb()
		elseif (newex .eq. 4) then
		  newex = 1
		  call inp3fc()
		elseif (newex .eq. 5) then
		  newex = 1
		  call inp3fd()
		elseif (newex .eq. 6) then
		  newex = 1
		  call inp3fe()
		elseif (newex .eq. 7) then
		  newex = 1
		  call inp3ff()
		elseif (newex .eq. 8) then
		  newex = 1
		  call inp3fg()
		elseif (newex .eq. 9) then
		  newex = 1
		  call inp3fh()
		elseif (newex .eq. 10) then
		  newex = 1
		  call inp3fj()
		elseif (newex .eq. 11) then
		  newex = 1
		  call inp3fk()
		elseif (newex .eq. 12) then
		  newex = 1
		  call inp3fl()
		else
		  write (errbuf(1), 10780) nbname, bkv, nid
10780             format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x,
     &             'FIRST EXCITER CARD MISSING')
		  call prterr('E', 1)
		  imchn = 3
		  goto 110
		endif
  310           icgenn(1, 3) = mex
		icgenn(2, 3) = 0
		igg = igr + igv
		do i = 1, iex
		  cgen(i+igg) = creg(i)
		enddo
	      endif
	    elseif (ijk .eq. 7) then
	      if (nosc .le. 0) then
		if (mgen .ne. 5 .and. mgen .ne. 8) then
		  if (mgen .ne. 9) goto 320
		endif
		if (mex .ne. 5) then
		  if (mex .ne. 8) then
 
C                   DECODING POWER SYSTEM STABILIZER ( PSS ) DATA CARD
 
		    read (work80(icard), 10790) subtyp
 
C                   SUBTYP = T IS A TRANSIENT STABILIZER
 
		    if (subtyp .eq. 'T') then
		      itsl = itsl + 1
		      call tslinp(itsl)
		      icgenn(1, 4) = msupp
		    else
C                     -  Note: Next read does not pick up V_s_lo or rmt_
		      read (work80(icard), 10790) subtyp, (temp(i), i =
     &                 1, 13)
10790                 format (bz, 1x, a1, 14x, f4.3, f3.3, f4.3, f3.3,
     &                 f4.2, 8f4.3)
C                     -  Put data into temp new PSS (inactive)
C                     call ssinp (temp)
		      if (temp(5) .le. 0.0 .or. temp(12) .le. 0.0) then
			imchn = 4
			if (temp(5) .le. 0.0 .or. temp(12) .le. 0.0) 
     &                     then
			  imchn = 4
			  write (errbuf(1), 10800) nbname, bkv, nid
			  call prterr('E', 1)
10800                     format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                     ' TQ AND VSLIM MUST BE +VE ')
			endif
		      endif
 
C                     PSS TRANSDUCER GAIN DATA CHECK.  KQV MUST NOT BE N
C                     BE NEGATIVE FOR SUB TYPE "P".  KQV & KQS BOTH CAN'
C                     TIME.
		      call target()
C                     -  OK for K_qv to be negative, since dV_s / dV_t w
C                     be positive and produce positive feedback.
C                     TEMP(1) = ABS(TEMP(1))
C                     -  Issue warning if K_qs is negative for types 'S'
		      if (index('SF', subtyp) .gt. 0 .and. 
     &                    temp(3) .lt. 0.0) then
			write (errbuf(1), 10880) nbname, bkv, nid
			write (errbuf(1)(19:120), '(a)')
     &                   ' has PSS with negative frequency or speed gain
     & (K_qs).'
			call prterr('W', 1)
		      endif
C                     TEMP(3) = ABS(TEMP(3))
		      if (temp(1) .eq. 0.0) then
			if (temp(3) .eq. 0.0) then
			  imchn = 4
			  write (errbuf(1), 10880) nbname, bkv, nid
			  write (errbuf(1)(19:120), '(a)')
     &                     ' has PSS with both K_qs & K_qv equal to zero
     &.'
			  call prterr('E', 1)
			endif
		      endif
		      do jj = 1, 5, 2
			if (temp(jj+5) .lt. tlim) then
			  if (temp(jj+6) .ge. tlim) then
			    imchn = 4
			    write (errbuf(1), 10810) nbname, bkv, nid,
     &                       jj
			    call prterr('E', 1)
10810                       format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &                       ' LEAD TIME CONST. MUST BE ZERO     when la
     &g time const. is zero in no. '
     &                       , i1)
			  endif
			endif
		      enddo
		      if (imchn .ne. 4) then
			if (notbl .eq. 0) then
 
C                         IF TQV LT TZERO, SET TO DEFAULT OF TZERO
 
			  if (temp(2) .lt. tzero .and. 
     &                        temp(1) .gt. 0.0) then
			    temp(2) = tzero
			    write (errbuf(1), 10820) nbname, bkv, nid,
     &                       temp(2)
10820                       format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x,
     &                       'TQV IS BEING SET', ' EQUAL TO: ', f7.5)
			    call prterr('W', 1)
			  endif
 
C                         IF TQS LT TZERO, SET TO DEFAULT OF TZERO
 
			  if (temp(4) .lt. tzero .and. 
     &                        temp(3) .gt. 0.0) then
			    temp(4) = tzero
			    write (errbuf(1), 10830) nbname, bkv, nid,
     &                       temp(4)
10830                       format ('0', 2x, a8, 2x, f5.1, 2x, a1, 5x,
     &                       'TQS IS BEING SET', ' EQUAL TO: ', f7.5)
			    call prterr('W', 1)
			  endif
			  if (subtyp .eq. 'S') then
			    msupp = 1
			    isupp = 1
			  elseif (subtyp .eq. 'F') then
			    msupp = 2
			    isupp = 2
C                           DECODE BUS NAME FOR REMOTE SIGNAL SOURCE IN
			    read (work80(icard), 10840) nfqbus, fbkv
10840                       format (bz, 68x, a8, f4.0)
			    if (nfqbus .ne. '        ') then
			      kb = nambas(fbkv)
			      jbusfq = inam(nfqbus, kb)
			      if (jbusfq .le. 0) then
				write (outbuf, 10440) nbname, bkv, nid
				call prtout(1)
				imchn = 4
				goto 110
			      endif
			    endif
			  else
			    msupp = 3
			    isupp = 3
			  endif
 
C                         FORM SUPPLEMENTARY CONTROL TABLE.
 
			  isup = 42
			  do i = 1, isup
			    csupp(i) = 0.0
			  enddo
			  tem(1) = temp(2)
			  tem(2) = temp(4)
			  tem(3) = temp(5)
			  tem(4) = temp(6)
			  tem(5) = temp(8)
			  tem(6) = temp(10)
			  do i = 1, 6
			    tem(i) = check_tc(tem(i), tlim, i, 'PSS',
     &                       nbname, bkv, nid)
			    if (tem(i) .gt. 0.0) tem(i) = tem(i)*frqbse
			  enddo
			  csupp(3) = temp(1)
			  csupp(4) = tem(1)
			  csupp(5) = temp(3)
			  if (msupp .eq. 3) then
			    if (bmvamc .gt. 0.0) 
     &                        csupp(5) = csupp(5) * bmvac
			  endif
			  csupp(6) = tem(2)
			  csupp(7) = tem(3)
			  csupp(14) = temp(12)
			  csupp(15) = temp(13)
			  do i = 4, 6
			    i2 = 2*i
			    csupp(i2) = temp(i2-1)*frqbse
			    csupp(i2+1) = tem(i)
			  enddo
			  icgenn(1, 4) = msupp
			  icgenn(2, 4) = 0
			  igge = igg + iex
			  if (msupp .eq. 2 .or. msupp .eq. 5) 
     &                      icgen(igge+1) = jbusfq
			  if (csupp(3) .gt. 0.0) icgen(igge+2) = jbusvq
C                         PROCESSING NOTCH FILTER DATA
C                         *   IF SUFFICIENT DATA GIVEN FOR PSS NOTCH FIL
C                         *   CSUPP TABLE
			  if (dnf .gt. 0.0) then
			    csupp(33) = dnf/(frqbse*frqbse)
			    csupp(34) = bnf/(frqbse*frqbse)
			    csupp(35) = cnf/frqbse
			    csupp(36) = anf/frqbse
			    anf = 0.0
			    bnf = 0.0
			    cnf = 0.0
			    dnf = 0.0
			  endif
			  do i = 3, isup
			    cgen(i+igge) = csupp(i)
C                           if (debug) then
C                           call dbgeko ('INPUT3 - After data checking P
C                           WRITE (ch20,585) NBNAME,BKV,NID
C                           call dbgwrc ('  Gen name: ',ch20)
C                           call dbgwrf ('  CSUPP(3) /PSS K_qv/ = ',CSUP
C                           endif
			  enddo
			endif
		      endif
		    endif
		    goto 110
		  endif
		endif
  320           write (errbuf(1), 10850) nbname, bkv, nid
		call prterr('W', 1)
10850           format (3x, a8, 2x, f5.1, 2x, a1, 5x,
     &           ' SUPP. CNTRL. NOT TO BE USED F    or this excitation s
     &ystem -- ignored '
     &           )
	      endif
	    elseif (ijk .eq. 8) then
 
C             CALL SVSINP TO DECODE AND ERROR CHECK TYPE V STATIC VAR SO
 
	      call svsinp(subtyp2)
	    elseif (ijk .eq. 9) then
 
C             CALL SVSSS1 TO DECODE AND ERROR CHECK TYPE W SUPPLEMENTARY
C             SIGNAL FOR SVS
 
	      call svsss1()
	    elseif (ijk .eq. 10) then
 
C             CALL SVSSS2 TODECODE AND ERROR CHECK TYPE X SUPPLEMENTARY
C             SIGNAL FOR SVS
 
	      call svsss2()
	    else
C             DECODE M-BLANK MACHINE DATA CARD
	      read (work80(icard), 10860) nbname, bkv, nid, gmva,
     &         pwrfct, (temp(i), i = 1, 4)
10860         format (bz, 3x, a8, f4.0, a1, f5.1, 1x, f3.2, 12x, 2f5.4,
     &         2f4.4)
	      if ((gmva .le. 0.0) .or. (abs(pwrfct) .gt. 1.)) then
		write (errbuf(1), 10870)
10870           format (' MVA and/or Power Factor error for ')
		write (errbuf(1)(36:120), 10880) nbname, bkv, nid
10880           format (a8, 2x, f5.1, 2x, a1)
		call prterr('E', 1)
	      endif
C             DAMPER WINDING DATA HANDLING
	      xdpp = temp(1)
	      xqpp = temp(2)
	      tdodp = temp(3)
	      tqodp = temp(4)
	      if (tdodp .lt. 0.01) tdodp = 0.0
	      if (tqodp .lt. 0.01) tqodp = 0.0
	      if (tqodp .ne. 0.0) tqodp = 1.0/(frqbse*tqodp)
	      if (tdodp .ne. 0.0) tdodp = 1.0/(frqbse*tdodp)
	    endif
	    goto 110
	  elseif (nogv .gt. 0) then
	    goto 110
	  else
c           process turbine
            call turbinp(isgg, work80(icard))
	  endif

c         csw removed as part of mgov removal
c	  icgenn(1, 2) = mgov
	  icgenn(2, 2) = 0
	  do i = 1, igv
	    cgen(i+igr) = cgov(i)
	  enddo
	endif
      endif
      goto 110
  330 continue
      iflag = 0
      goto 120
  340 if (isgg .eq. 0) then
	write (errbuf(1), 10950)
	call prterr('E', 1)
10950   format (
     &   '0 NO GOOD MACHINE ENCOUNTERED---JOB WILL BE ABORTED I    n lin
     &k 3  '
     &   )
	call erexit()
      endif
      nnn = isgg + 1
      if (keybrd(29) .ne. 0) then
	do j = 1, ntotd
	  write (outbuf, 10960) (ipactn(i, j), i = 1, 3)
	  call prtout(1)
	enddo
10960   format (1x, 3i10)
      endif
      if (imchnp .ne. 0) then
	write (errbuf(1), 10970)
10970   format ('0 THE FOLLOWING MACHINES HAVE XQPP LESS THEN XDPP')
	call prterr('W', 1)
	do itrr = 1, imchnp
	  write (outbuf, 10980) namepp(itrr), bkvpp(itrr), nidpp(itrr)
10980     format (5x, a8, 1x, f5.1, 1x, a1)
	  call prtout(1)
	enddo
      endif
      if (imchnt .ne. 0) then
	write (errbuf(1), 10990)
10990   format ('0 THE FOLLOWING MACHINES HAVE TDODP LESS THEN TQODP')
	call prterr('W', 1)
	do itrr = 1, imchnt
	  write (outbuf, 11000) nametp(itrr), bkvtp(itrr), nidtp(itrr)
11000     format (5x, a8, 1x, f5.1, 1x, a1)
	  call prtout(1)
	enddo
      endif
      if (istop3 .eq. 0 .and. iabort .eq. 0) then
	write (outbuf, 11010)
	call prtout(1)
11010   format ('0 SUBROUTINE INPUT3 HAS BEEN PROCESSED.')
	link = 4
	return
      else
	write (errbuf(1), 11020)
11020   format ('0 SEVERE DATA ERRORS IN INPUT --JOB WILL BE ABORTED')
	call prterr('E', 1)
	call erexit()
      endif
      end
 
