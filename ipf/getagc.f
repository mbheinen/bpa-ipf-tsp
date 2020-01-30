C    @(#)getagc.f	20.4 2/13/96
      subroutine getagc (error)
 
C     process /AGC commands.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
c	Global variables used:
c		agc, kagc, numagc
      include 'ipfinc/alpha.inc'
c	Global variables used:
c		pnetu(r*8), ploadu(r*8), ntypu
      include 'ipfinc/arcntl.inc'
c	Global variables used:
c		arcnam
      include 'ipfinc/area.inc'
c	Global variables used:
c		karea, jarzn
      include 'ipfinc/blank.inc'
c	Global variables used:
c		buf, ntotc, bmva, card
      include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, opt2inp, base, inp2opt, busdta
      include 'ipfinc/com007.inc'
c	Global variables used:
c		 bustyp
      include 'ipfinc/ecvar.inc'
c	Global variables used:
c		None
      include 'ipfinc/jobctl.inc'
c	Global variables used:
c		None
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		inp
      include 'ipfinc/ordsta.inc'
c	Global variables used:
c		ordvlt, ordtie 
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf, outbuf
      include 'ipfinc/qsdup.inc'
c	Global variables used:
c		dupsw 

      common /is_batch / is_batch

      integer find_bus, error

      external kmpagc, swpagc, find_bus

      character bus1 * 8, word(100) * 30, capital * 30,
     1          commnt * 54, type * 2, cfmt * 7
 
      numagc = 0
C                  ERROR  = flag: 0/1 = no errors/errors encountered.   
      error = 0 
 
      if (index(buf,'AGC') .eq. 0) go to 900
C                                     /AGC  
  100 read (inp,110, end=200) buf   
  110 format (a)
      card = buf(1:1)   
      call space (1)
      write (outbuf,120 ) buf(1:80) 
  120 format (' AGC text (', a, ')')
      call prtout (1)   
      if (card .eq. '.') then   
         go to 100  
      else if (card .eq. 'B') then  
         read (buf,130,err=192) bus1, base1 
  130    format (bz, t7, a8, f4.0)  
         nb = find_bus (bus1, base1)  
         if (nb .le. 0) then
            write (errbuf(1),140) bus1, base1   
  140       format ('/ AGC bus ', a8, f6.1, ' is not in system.')   
            call prterx ('W', 1)
            error = 1   
         else   
            if (numagc .ge. MAXAGC) then
               write (errbuf(1),150) MAXAGC 
  150          format ('More than ', i3, ' / AGC buses.')   
               if (is_batch .eq. 0) then
                  call prterx ('E',1)
               else
                  call prterx ('F',1)
               endif
               error = 1
            else
               numagc = numagc + 1  
               call uscan(buf(20:),word,nwrd,'=',' ,()<>/\\')   
               pmax = busdta(7,nb)  
               pmin = 0.0   
               pcttot = 0.0 
               if (ordvlt .eq. 2) then
                  kt = inp2opt(nb)
               else
                  kt = nb
               endif   
               pgen = (pnetu(kt) +ploadu(kt)) * bmva  
               if (pgen .le. 0.0) then  
                  write (errbuf(1),160 ) bus1, base1, pgen * bmva   
  160             format ('/ AGC bus ', a8, f6.1,   
     1               ' is not a generator: P_Gen = ', f8.1) 
                  call prterx ('W', 1)  
               endif
               kagc(1,numagc) = nb  
               kagc(2,numagc) = 0   
               kagc(3,numagc) = 0   
               agc(5,numagc) = pmax / bmva  
               agc(6,numagc) = 0.0  
               agc(7,numagc) = pgen / bmva  
               agc(8,numagc) = (pnetu(kt) + ploadu(kt)) * bmva
               agc(8,numagc) = sngl(ploadu(kt))
               kagc(9,numagc) = 0   
               kagc(10,numagc) = 0  
               kagc(11,numagc) = 0  
               kagc(12,numagc) = ntypu(kt)         
               pmin = 0.0   
               do 180 i = 1, nwrd, 3
                  word(i) = capital (word(i))
                  last = lastch(word(i+2))
                  write (cfmt, 102) last
  102             format ('(f', i2, '.0)')
                  if (word(i)(1:1) .eq. '%') then
                     read (word(i+2), fmt=cfmt, err=192) pct
C                    READ (WORD(I+2), *, ERR=192) PCT
                     agc(4,numagc) = 0.01 * pct
                  else if (word(i) .eq. 'PMAX') then
                     read (word(i+2), fmt=cfmt, err=192) pmax
C                    READ (WORD(I+2), *, ERR=192) PMAX
                     agc(5,numagc) = pmax / bmva
                  else if (word(i) .eq. 'PMIN') then
                     read (word(i+2), fmt=cfmt, err=192) pmin
C                    READ (WORD(I+2), *, ERR=192) PMIN
                     agc(6,numagc) = pmin / bmva
                  else if (word(i) .eq. 'PGEN') then
                     read (word(i+2), fmt=cfmt, err=192) pgenew
C                    READ (WORD(I+2), *, ERR=192) PGENEW
                     pgen = pgenew  
                     pnetu(kt) = dble( pgen / bmva) - ploadu(kt) 
                     agc(7,numagc) = pgen / bmva
                  else  
                     last = lastch (word(i))
                     write (errbuf(1),170 ) word(i)(1:last) 
  170                format ('Indecipherable keyword (',a,') ignored.')   
                     call prterx ('W',1)
                  endif 
  180          continue 
               if (pmin .gt. pgen .or. pgen .gt. pmax) then 
                  write (errbuf(1),190 ) bus1, base1, pmin, pgen, pmax  
  190             format ('/AGC bus ', a8, f6.1,
     1               ' initial generation violates limits: P_min =',
     2               f7.1, ' P_gen =', f7.1, ' P_max =', f7.1)  
                  call prterx ('W', 1)  
               endif
            endif   
         endif  
         go to 100  
        
  192    write (errbuf(1), 194) buf(1:60)   
  194    format ('Illegal data in field (', a, ')') 
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         error = 1  
         go to 100  
        
      endif 
      go to 210 
        
  200 buf = '( END ) GETAGC'
      card = buf(1:1)   
        
  210 if (numagc .eq. 0) then   
         write (errbuf(1),220 ) 
  220    format ('No generators specified with / AGC')   
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         error = 1  
         go to 900  
      else if (numagc .gt. 1) then  
         dupsw = .false.
         call qiksrt (1, numagc, kmpagc, swpagc)
         if (dupsw) then
  222       last = numagc   
            do 228 i = 1, last-1
               if (kagc(1,i) .eq. kagc(1,i+1)) then 
                  nb = kagc(1,i)
                  write (errbuf(1),224) bus(nb), base(nb)   
  224             format ('Duplicate AGC generators: ', a8, f7.1,
     1                    '. Second entity deleted.')
                  call prterx ('W', 1)  
                  do 226 j = i+1, last-1
                     do 225 k = 1, 14   
  225                kagc(k,j) = kagc(k,j+1)
  226             continue  
                  numagc = numagc - 1   
                  go to 222 
               endif
  228       continue
         endif  
      endif 
      numslk = 0
      numare = 0
      pcttot = 0.0  
      ptot = 0.0
      do 260 i = 1, numagc
         nb = kagc(1,i)
         pcttot = pcttot + agc(4,i)
         ptot = ptot + agc(5,i)
C
C        Flag generator if it is a system or area slack bus.
C
         do 230 j = 1, ntotc
            k = karea(1,j)  
            if (ordtie .eq. 2) k = opt2inp(k) 
            if (k .eq. nb) then 
               kagc(9,i) = j
               numslk = numslk + 1  
               go to 240
            endif   
  230    continue   
  240    continue   
         if (numare .eq. 0) then
            numare = jarzn(nb)  
         else if (numare .ne. jarzn(nb)) then   
            write (errbuf(1),250 ) bus(nb), base(nb), arcnam(jarzn(nb)) 
  250       format ('/AGC bus ', a8, f6.1,  
     1              ' is in wrong area (', a, ')')   
            call prterx ('W', 1)
         endif
  260 continue
C
C     Check for viable percentage scheme: PTOT > 0.
C
      if (ptot .le. 0.1) then
         write (errbuf(1),270 ) ptot
  270    format ('/ AGC scheme does not include any generators: ',
     1           'P_total = ', f8.1)
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         error = 1
         go to 900
      endif
      if (numslk .eq. 0) then
         write (errbuf(1), 272)
  272    format ('No area or system slack bus included in ',
     &           '/ AGC control scheme.')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         error = 1
      endif
 
      call forbtm
      outbuf = ' Initialization of AGC Generators '
      call rpnlod
      write (outbuf, 280)
  280 format(t2, 'Type',
     1       t8, 'Generators',  
     2      t26, '- Percentage -',  
     3      t42, '----- Power (MW) ------', 
     4      t78, 'Comments')
      call shdlod(1)
      write (outbuf, 290)   
  290 format(t26,'Scheduled Used',  
     1       t42,'  P_min  P_base  P_max ') 
        
      call shdlod(2)
      outbuf= ' '   
      call shdlod(3)
      call shdlod(4)
      call shdlod(5)
      call fortop   
        
      do 310 i = 1,numagc   
         k = kagc(1,i)  
        
         percnt = agc(4,i)  
         commnt = ' '   
         pctgen = agc(5,i) / ptot   
         if (abs (pcttot - 1.0) .lt. 0.01) then 
            pctuse = percnt 
            if (abs(pctuse - pctgen) .gt. 0.10) then
               commnt = '% used is not proportional to P_max'   
            endif   
         else if (abs(pcttot) .lt. 0.01) then   
            pctuse = agc(5,i) / ptot
            agc(4,i) = pctuse   
            commnt = '% allocated in proportion to P_max'   
         else   
            pctuse = agc(4,i) / pcttot  
            agc(4,i) = pctuse   
            write (commnt, 298) agc(4,i) * 100.0
  298       format ('% scaled from ', f8.2) 
         endif  
        
         type = 'B' // bustyp(kagc(12,i))   
         write (outbuf, 300) type, bus(k), base(k), 100.0 * percnt, 
     1      100.0 * pctuse, agc(6,i) * bmva, agc(7,i) * bmva,   
     2      agc(5,i) * bmva, commnt 
  300    format(t2, a2, t8, a8, f7.1, t24, f6.1, f8.1, t39, 3f8.1,  
     2      t78, a) 
           call prtout(1)   
  310 continue  
        
  900 continue  
        
      outbuf = ' '  
      call rpnlod   
      call shdlod (1)   
      call shdlod (2)   
      call shdlod (3)   
      call shdlod (4)   
      call shdlod (5)   
      outbuf = '0 End of / AGC '
      call prtout(1)
      call forbtm   
        
      return
      end   
