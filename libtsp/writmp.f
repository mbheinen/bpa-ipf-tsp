C    %W% %G%
      subroutine writmp(iux, cma, nw, idu)
C     
C     THIS SUBROUTINE SIMULATES A CALL TO A MASS STORAGE DEVICE.
C     IT WRITES NW*4 WORDS FROM CMA(1) TO ARRAY SIMMS(IS,IU) THRU
C     SIMMS(IS+4*NW,IU).  WHERE IU IS A SIMULATED RECORD NUMBER
C     DETERMINED BY IU = IFUNIT(IUX).  THE STARTING ADDRESS, IS,
C     IS STORED IN ARRAY MSUB(IUX*2).  ARGUMENT IDU IS NEVER USED.
C     WRITMP CALLS COPY AND IFUNIT.
C     
      dimension cma(nw)
      include 'tspinc/params.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/space0.inc'
 
      dimension ifunit(14)
      logical debug
C     -       Begin       Begin       Begin       Begin       Begin

      data maxu/7*1/
      data ifunit/1, 2, 0, 3, 0, 0, 4, 0, 0, 5, 0, 6, 7, 0/
      debug = .false.
 
C     GET UNIT NUMBER 1-7
 
      iu = ifunit(iux)
 
C     FIND WHERE TO PUT DATA (EOI)
 
      is = maxu(iu)
 
C     Update start of block (IS) & end of channel (MAXU)
 
      msub(iux*2) = is
      maxu(iu) = maxu(iu) + nw
      if (debug) then
        call dbgeko('WRITMP - about to write to SIMMS array')
        call dbgwri('  IUX (unit #)       = ', iux)
        call dbgwri('  IU (channel #)     = ', iu)
        call dbgwri('  NW (size of block) = ', nw)
        call dbgwri('  IS (start index)   = ', is)
        call dbgwri('  MAXU (end index)   = ', maxu(iu))
      endif
C     
C     IF TABLE OVERFLOW OCCURS, ABORT THE JOB
C     
      if (maxu(iu) .gt. 200000) then
        write (errbuf(1), 10000) maxu(iu), iu
10000   format ('0', 5x, 'FATAL ERROR IN WRITMP--TABLE OVERFLOW',
     &   'MAXU = ', i8, 5x, 'IU =  ', i2)
        call prterr('E', 1)
        call erexit()
      endif
 
C     WRITE TO SIMULATE ARRAY
 
C     Replace COPY call with inline code
C     CALL COPY(NW*4,CMA,SIMMS(IS,IU))                              !
      ls = is
      do la = 1, nw
        simms(ls, iu) = cma(la)
        ls = ls + 1
 
      enddo
      goto 100

C     **********************************************************
      entry readmp(iux, cma, nw, idu)
C     
C     THIS IS AN ENTRY POINT IN WRITMP.  IT READS NW*4 WORDS FROM
C     ARRAY SIMMS INTO ARRAY CMA.  ARGUMENT IDU IS NEVER USED.
C     READMP CALLS COPY AND IFUNIT.
C     
C     GET UNIT NUMBER 1-7
 
      iu = ifunit(iux)
      ls = msub(iux*2)
      ke = ls + nw
      if (debug) then
        call dbgeko('READMP - about to read from SIMMS array')
        call dbgwri('  IUX (unit #)       = ', iux)
        call dbgwri('  IU (channel #)     = ', iu)
        call dbgwri('  NW (size of block) = ', nw)
        call dbgwri('  LS (start index)   = ', ls)
        call dbgwri('  KE (end index)     = ', ke)
      endif
C     
      if (ke .gt. 200000) then
        iabort = 1
        write (errbuf(1), '(a)')
     &   ' READMP - overflow of SIMMS array while reading'
        call prterr('E', 1)
        call erexit()
      endif
 
C     MOVE DATA FROM SIMULATE ARRAY
 
C     Replace COPY call with inline code
C     CALL COPY(NW*4,SIMMS(MSUB(IUX*2),IU),CMA)

      do la = 1, nw
        cma(la) = simms(ls, iu)
        ls = ls + 1
      enddo
  100 return
      end
