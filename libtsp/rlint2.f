C    %W% %G%
      subroutine rlint2  
C       
C     THIS SUBROUTINE FORMS AND INITIALIZES TABLES FOR THE DEFAULT  
C     DISTANCE RELAY.  IT IS CALLED BY INITL4.  
C       
      include 'tspinc/params.inc'  
      include 'tspinc/namec.inc'  
      include 'tspinc/pointr.inc'  
      include 'tspinc/bypass.inc'  
      include 'tspinc/matrow.inc'  
      include 'tspinc/relays.inc'  
      include 'tspinc/param.inc'  
      include 'tspinc/prt.inc'  
  
      dimension ib(MAXBUS), jb(MAXBUS)  
      dimension rltab(206), irltab(206)  
      equivalence (rltab, irltab)  
      character*8 name, name1, name2  
  
      ndfltd = 0  
      index = 0  
      indbp = 0  
      indi = 0  
      ij = 0  
      mspce = 0  
      lastbr = 0  
      lastbs = 0  
      indbpm = 0  
      if (keybrd(16) .ne. 0) then  
        do i = 1, nbypas  
          write (outbuf, 10000) i, kbpass(1, i), kbpass(2, i)  
          call prtout(1)  
        enddo  
10000   format (1x, '  RELAY BYPASS       ', 1x, i10, 5x, i10, 5x, i10)  
      endif  
      do i = 1, nbypas  
        ib(i) = kbpass(1, i)  
        jb(i) = kbpass(2, i)  
C         
C       FIND CORRESPONDING BRANCH IN YMATRIX AND SET JBUS NUMBER  
C       TO NEGATIVE TO INDICATE THIS BRANCH IS TO BYPASSED  
C       NEW BUS TEST  
C         
        if (ib(i) .ne. lastbs) then  
C           
C         WRITE OUT LAST ROW  
C           
          if (lastbs .ne. 0) call putmat(lastbs, ii)  
C           
C         SET UP FOR NEW BUS  
C           
          lastbs = ib(i)  
          call getmat(ib(i), ii)  
C           
C         OBTAIN LOW LIMIT TO RIGHTHAND TERMS OF YMATRIX  
C           
          indxp = ii - 3*matrow(3)  
C           
C         OBTAIN STARTING INDEX (EXCLUDES THE DIAGONAL TERM)  
C         FOR RIGHTHAND TERMS. NOTE RIGHTHAND TERMS ARE STORED  
C         IN REVERSE ORDER, FROM HIGH TO LOW.  
C           
          indx = ii - 5  
C           
C         LAST BRANCH CODE  
C           
          lastbr = 0  
C         
C       CHECK IF BRANCHES EXIST FOR SEARCH  
C         
        elseif (lastbr .ne. 0) then  
          goto 100  
        endif  
C         
C       SEARCH FOR BRANCH TO BE BYPASSED  
C         
        do while (jb(i) .ne. matrow(indx))  
          indx = indx - 3  
C           
C         TEST IF END OF SEARCH  
C           
          if (indx .le. indxp) goto 100  
        enddo  
        matrow(indx) =  - matrow(indx)  
C         
C       SET SEARCH INDEX TO NEXT MATRIX ELEMENT  
C         
        indx = indx - 3  
C         
C       TEST IF PAST LEFTMOST ELEMENT OF RIGHTHAND SIDE.  
C       THEN END OF SEARCH.  
C         
        if (indx .lt. indxp) lastbr = 1  
        goto 110  
C         
C       SEARCH FAILED. . . SET UP FOR ERROR MESSAGE  
C         
  100   iabort = 1  
        indpo = ib(i)  
        jndpo = jb(i)  
        name1 = exnamc(indpo)  
        ibase1 = ixnamn(indpo)  
        name2 = exnamc(jndpo)  
        ibase2 = ixnamn(jndpo)  
        base1 = basekv(ibase1)  
        base2 = basekv(ibase2)  
        write (errbuf(1), 10010) name1, base1, name2, base2  
10010   format ('0', 5x,   
     &   'ERROR  POWER FLOW DOES NOT HAVE A BRANCH ',
     &   'BETWEEN' ,a8,1x,f5.1,' AND ' ,a8,1x,f5.1)      
        call prterr('E', 1)  
  110   continue  
      enddo  
C       
C     WRITE OUT FINAL YMATRIX ROW  
C       
      call putmat(lastbs, ii)  
      if (ij .eq. 1) then  
        do while (.true.)  
          indp = ib(indbp)  
          if (indp .eq. indi) then  
            jndp = jb(indbp)  
            do while (jndp .ne. matrow(indx))  
              if (jndp .le. matrow(indx)) goto 140  
              irltab(i+1) = matrow(indx)  
              irltab(i+2) = 1  
              rltab(i+4) =  - 1.0  
              rltab(i+3) = rltab(i+4)  
              if (keybrd(16) .ne. 0) then  
                write (outbuf, 10030) irltab(i+1), irltab(i+2), rltab(i  
     &           +3), rltab(i+4)  
                call prtout(1)  
              endif  
              i = i + 4  
              indx = indx - 3  
              if (indx .lt. indxp) goto 130  
            enddo  
            nline = nline - 1  
            if (nline .le. 0) goto 120  
            indx = indx - 3  
            if (indx .lt. indxp) goto 180  
          elseif (indp .gt. indi) then  
            goto 160  
          endif  

          indbp = indbp + 1  
          if (indbp .gt. nbypas) goto 160  
          indbpm = indbpm + 1  
          ij = 1  
        enddo  
C         
C       CHECK IF ANY MORE ENTRIES IN BYPASS TABLE FOR THE SAME ROW.  
C       IF YES , THEN IT IS AN ERROR CONDITION  
C         
  120   m = 1  
        do while ((indbp+m) .le. nbypas)  
          if (indi .lt. ib(indbp+m)) goto 150  
          indpo = ib(indbp+m)  
          jndpo = jb(indbp+m)  
  
C         Branch could not be found  
  
          iabort = 1  
          indpo = ib(i)  
          jndpo = jb(i)  
          name1 = exnamc(indpo)  
          ibase1 = ixnamn(indpo)  
          name2 = exnamc(jndpo)  
          ibase2 = ixnamn(jndpo)  
          base1 = basekv(ibase1)  
          base2 = basekv(ibase2)  
          write (errbuf(1), 10010) name1, base1, name2, base2  
          call prterr('E', 1)  
          m = m + 1  
        enddo  
        goto 150  
  130   indpo = indp  
        jndpo = jndp  
  
C       Branch could not be found  
  
        iabort = 1  
        indpo = ib(i)  
        jndpo = jb(i)  
        name1 = exnamc(indpo)  
        ibase1 = ixnamn(indpo)  
        name2 = exnamc(jndpo)  
        ibase2 = ixnamn(jndpo)  
        base1 = basekv(ibase1)  
        base2 = basekv(ibase2)  
        write (errbuf(1), 10010) name1, base1, name2, base2  
        call prterr('E', 1)  
        goto 180  
  
C       Branch could not be found  
  
  140   iabort = 1  
        indpo = ib(i)  
        jndpo = jb(i)  
        name1 = exnamc(indpo)  
        ibase1 = ixnamn(indpo)  
        name2 = exnamc(jndpo)  
        ibase2 = ixnamn(jndpo)  
        base1 = basekv(ibase1)  
        base2 = basekv(ibase2)  
        write (errbuf(1), 10010) name1, base1, name2, base2  
        call prterr('E', 1)  
        goto 160  
      else  
C         
C       YMATRIX IS NOW MARKED WITH BYPASS FLAGS  
C       (NEGATIVES ON THE JBUSES)  
C       NOW BUILD A DEFAULT RELAY TABLE FROM THE  
C       REMAINING BUSES  
C         
C       LOOP FOR EACH IBUS  
C         
        icbyp = 0  
      endif  
  150 indi = indi + 1  
      neg = 1  
      if (indi .gt. nmx) goto 190  
      call getmat(indi, ii)  
      loclen = matrow(3)  
      nline = loclen - 1  
C       
C     TEST FOR EXISTENCE OF AT LEAST ONE BRANCH  
C       
      if (nline .le. 0) goto 150  
      ibase = ixnamn(indi)  
      ckx = rovx(ibase)  
      irltab(1) = indi  
      rltab(2) = ckx  
      if (keybrd(16) .ne. 0) then  
        write (outbuf, 10020) irltab(1), rltab(2)  
10020   format ('0 IRLTAB(1), RLTAB(2)', 5x, i5, 5x, f8.3)  
        call prtout(1)  
      endif  
      i = 3  
      indxp = ii - 3*loclen  
      indx = ii - 5  
  160 do while (.true.)  
        if (matrow(indx) .gt. 0) then  
C           
C         BRANCH NOT TO BYPASS, MOVE INTO TABLE  
C           
          irltab(i+1) = matrow(indx)  
          irltab(i+2) = 1  
          rltab(i+4) =  - 1.0  
          rltab(i+3) = rltab(i+4)  
          if (keybrd(16) .ne. 0) then  
            write (outbuf, 10030) irltab(i+1), irltab(i+2), rltab(i+3),   
     &       rltab(i+4)  
10030       format ('0 IRLTAB(I+1) TO IRLTAB(I+4) ', 5x, 2(i5, 2x),   
     &       f8.4, 2x, f8.4)  
            call prtout(1)  
          endif  
          i = i + 4  
        else  
C           
C         IF FOUND NEGATIVE RESTORE TO POSITIVE  
C           
          matrow(indx) =  - matrow(indx)  
          neg =  - 1  
          icbyp = icbyp + 1  
          nline = nline - 1  
        endif  
        indx = indx - 3  
        if (indx .lt. indxp) goto 170  
      enddo  
C       
C     RESTORE YMATRIX INDICES TO POSITIVE NUMBERS  
C     NEG = -1 MEANS AT LEAST ONE INDEX WAS NEGATIVE  
C       
  170 if (neg .eq. -1) call putmat(indi, ii)  
  180 irltab(3) = nline  
C       
C     TEST IF ANY BRANCHES STORED  
C       
      if (i .eq. 3) goto 150  
      if (index+i .gt. 30000) goto 200  
      do j = 1, i  
        store(index+j) = rltab(j)  
      enddo  
      index = index + i  
C       
C     GO FIND NEW BUS  
C       
      goto 150  
  190 ndfltd = ndfltd + index  
      if (nbypas .gt. icbyp) then  
        iabort = 1  
        indbpd = nbypas - indbp  
        write (errbuf(1), 10040) indbpd  
        call prterr('E', 1)  
10040   format ('0', 5x, 'PROBABLE RENUMBERING ERROR  ', i5,   
     &   'UNMATCHED BRANCHES LEFT OVER IN BYPASS TABLE')  
      endif  
      itripl = idfltd + ndfltd  
      goto 210  
  200 write (errbuf(1), 10050)  
10050 format (' ERROR, ARRAY STORE IN SUBROUTINE RLINT2 IS ',   
     & 'OVERFLOWING, CONSULT A PROGRAMMER. ')  
      call prterr('E', 1)  
      iabort = 1  
  210 return  
      end  
