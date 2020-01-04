C    @(#)getx1.f	20.7 5/27/99
        subroutine getx1 (jout)

C       COMPUTES VECTOR X1 AND SCALAR C1 FOR COMPENSATION

        include 'ipfinc/parametr.inc'

        include 'ipfinc/apcom.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/time1.inc'

        common /cmpsen/ ik,im,xmik,xmim,jk,jm,xmjk,xmjm

c       Note: This variable can be changed with a symbolic debugger
        common /term_debug/ iterm

C       THE FOLLOWING CONSTANTS SAVE THE NON-ZERO ELEMENTS OF M

        xmik = 0.
        xmim = 0.

C       ZERO OUT M1

        call fvfill(0.,x1,1,iq)
C
        ik = min0( klnc(1,jout), klnc(2,jout) ) -1
        im = max0( klnc(1,jout), klnc(2,jout) ) -1
C
        if (idebug .gt. 1) write(dbug,1000) jout,ik,im
 1000   format(' FOR OUTAGE ',i4,' IK,IM ',2i5)
C
C       FIND VECTOR M1
C
        if (ik.eq.0) go to 10
        xmik = 1.0
        x1(ik) = xmik
   10   xmim = -1.0
        x1(im) = xmim
C
C       FIND TERM X1
C
        call srsfb (iq,x1,ipfbp,fbp,jfbp,dgbp)
C       CALL FSRSFB (IQ,X1,IPFBP,FBP,0,DGBP)
        if (idebug .gt. 1) write (dbug,1006) (x1(i),i=1,iq)
 1006   format(' SOLUTION X1',/,(1x,10e12.4))
C
C       FIND SCALAR MULTIPLIER C1
C
        c1inv = -1.0/clnc(2,jout)
        c1 = c1inv - xmim * x1(im)
        if (ik .ne. 0) c1 = c1 - xmik * x1(ik)

        if (iterm .ne. 0) then
           write (*,42) ik, im, xmik, xmim, c1, c1inv
   42      format ('0 GETX1 factors ',2i5, ' factors ',4f10.4)
        endif
C
C       CHECK FOR SYSTEM SEPARATION
C
        if (abs(c1inv) .gt. 1.0e-3) then
           if (abs(c1) .lt. 1.0e-4*abs (c1inv)) go to 80
        else
           if (abs(c1) .lt. 1.0e-6*abs (c1inv)) go to 80
        endif
        c1 = 1.0 / c1
        return
C
   80   continue
C
C       RECORD SYSTEM SEPARATION
C
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
C       SET C1 TO A HARD 0
C
        c1 = 0.
        if (idebug.gt.1) write (dbug,1040) jout
 1040   format(' **** SYSTEM SEPARATION AS A RESULT OF THE',i4,
     1       'TH OUTAGE ****')
        return
        end
