C    @(#)getx2.f	20.7 5/27/99
        subroutine getx2 (jout)

C       COMPUTES VECTOR X2 AND SCALAR C2 FOR COMPENSATION

        include 'ipfinc/parametr.inc'

        include 'ipfinc/apcom.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/time1.inc'

        common /cmpsen/ ik,im,xmik,xmim,jk,jm,xmjk,xmjm

c       Note: This variable can be changed with a symbolic debugger
        common /term_debug/ iterm

C       THE FOLLOWING CONSTANTS SAVE THE NON-ZERO ELEMENTS OF M

        xmjk = 0.
        xmjm = 0.

C       ZERO OUT M2

        call fvfill(0.,x2,1,iq)
C
C       JK AND JM ARE POINTERS TO THE NON-ZERO ELEMENTS IN M2
C
        jk = min0( klnc(1,jout), klnc(2,jout) ) -1
        jm = max0( klnc(1,jout), klnc(2,jout) ) -1
        if (ipqv(jk+1).eq.0) jk = 0
        if (ipqv(jm+1).eq.0) jm = 0
C
        if (idebug .gt. 1) write(dbug,1000) jout,jk,jm
 1000   format(' FOR OUTAGE ',i4,' JK,JM ',2i5)
C
C       FIND VECTOR M2
C
        if (clnc(5,jout) .eq. 0.0) then
           xm = 1.0
        else
           xm = clnc(6,jout) / clnc(5,jout)
        endif
        if (jk .gt. 0) then
           xmjk = xm
           x2(jk) = xmjk
        endif
        if (jm .gt. 0) then
           xmjm = -1.0/xm
           x2(jm) = xmjm
        endif
C
C       COMPUTE X2 VECTOR
C
        call srsfb (iq,x2,ipfbpp,fbpp,jfbpp,dgbpp)
        if (idebug .gt. 1) write(dbug,1008) (x2(i),i=1,iq)
 1008   format(' SOLUTION X2',/,(1x,10e12.4))
C
C       FIND SCALAR MULTIPLIER C2
C
        c2inv  = -1.0 / clnc(2,jout)
        c2 = c2inv
C
        if (jk .ne. 0) c2 = c2 - xmjk * x2(jk)
        if (jm .ne. 0) c2 = c2 - xmjm * x2(jm)

        if (iterm .ne. 0) then
          write (*,42) jk, jm, xmjk, xmjm, c2, c2inv
   42     format ('  GETX2 factors ',2i5, ' factors ',4f10.4)
        endif
C
C       CHECK FOR SYSTEM SEPARATION
C
        if (abs(c2inv) .gt. 1.0e-3) then
           if (abs(c2) .lt. 1.0e-4*abs (c2inv)) go to 80
        else
           if (abs(c2) .lt. 1.0e-6*abs (c2inv)) go to 80
        endif
        c2 = 1.0 / c2
C
C       IF BOTH ENDS OF THE OUTAGED LINE ARE PV BUSES, NO
C       COMPENSATION IS NECESSARY. SET C2=0 TO FLAG THIS CONDITION.
C
        if (jk.eq.0.and.jm.eq.0) c2 = 0.
C
        if (idebug .gt. 1) write(dbug,1020) c2,xmjk,xmjm
 1020   format(' C2,XMJK,XMJM ',3e11.4)
        return
C
C       RECORD SYSTEM SEPARATION
C
   80   continue
        if (iib .lt. MXIBAD) then
           ibad(iib) = 0
           bad(iib) = 0.0
           iib = iib + 1
        else if (iib .eq. MXIBAD) then
           ibad(iib) = 0
           bad(iib) = 0.0
           iib = iib + 1
           write (errbuf(1),82) jout,nout,iib
   82      format (' Overflow of "IBAD" array: JOUT,NOUT,IIB =',3i8)
           call prterx ('E',1)
        else
        endif
C
C       SET C1 AND C2 TO A HARD 0
C
        c1 = 0.
        c2 = 0.
        if (idebug.gt.1) write (dbug,1040) jout
 1040   format(' **** SYSTEM SEPARATION AS A RESULT OF THE',i4,
     1       'TH OUTAGE ****')
        return
        end
