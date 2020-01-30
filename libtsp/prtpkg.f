C    %W% %G%
        subroutine prtpkg
 
C       STANDARDIZED PRINT PACKAGE FOR TSP.
C       ROUTINE WILL PRINT ON THE CRT, LINEPRINTER AND/OR FICHE
C       UNITS AS NEEDED, USING THE SAME OUTPUT BUFFER.
 
 
C       THIS PACKAGE CONTAINS THE FOLLOWING ENTRIES:
 
C       1.      PRTOUT(NUM)     PRINT THE DETAIL LINE ON ALL
C                               SELECTED DEVICES
 
C       1a.     PFOMF(NUM)      PRINT THE DETAIL LINE ON THE
C                               FICHE UNIT ONLY FOR CONTROL OF FICHE.
C                               LINE NOT COUNTED!
 
C       2.      HEDLOD          LOAD HEADER MESSAGE BUFFER
 
C       3.      RPNLOD          LOAD REPORT NAME BUFFER
 
C       4.      SHDLOD(NUM)     LOAD SUB_HEADING(NUM), 1.LE.NUM.LE.5
 
C       5.      COMLOD(NUM)     LOAD COMMENT(NUM), 1.LE. NUM .LE.2
 
C       6.      SHDPRT(NUM)     PRINT SUB_HEADING
 
C       7.      FORBTM          FORCE BOTTOM FOOTER BOTH LPRT & FICHE
 
C       8.      FORTOP          FORCE TOP HEADER BOTH LPRT & FICHE ANY
C                               COMENTS AND SUBHEADINGS WILL PRINT TOO
 
C       9.      NEWPG           JUMP TO NEW PAGE.  PRINT FOOTER AND
C                               HEADER ON LPRT AND FICHE
 
C       10.     SKIPLN(NUM)     SKIPLN "NUM" BLANK LINES ON BOTH LPRT
C                               AND FICHE
 
C      11.      DBGPRT(NUM)     TURNS ON/OFF PRINTING OF ALL RECORDS
C                               TO THE DEBUG FILE.
C                               0 = OFF!  >0 = ON
 
C      12.      CHKBTM(NUM)     FORCE A NEW PAGE IF LESS THAN NUM LINES
C                               REMAIN
 
 
C               HEADER IS THE HEADER AND FOOTER LINE AND WILL APPEAR
C                      ON ALL PAGES OF THE REPORT.
 
C               COMENT IS TWO LINES OF USER-SUPPLIED COMMENTORY, AND
C                      WILL APPEAR BELOW THE HEADER ON ALL PAGES OF
C                       THE REPORT.
 
C               REPNAM IS THE NAME OF THE SECTION OF THE MAIN REPORT,
C                      AND WILL BE PRINTED ON THE SAME LINE WITH HEADER
C                      AT THE HEAD AND FOOT OF EACH PAGE.
 
C               SUBHED IS 5 LINES OF SUBHEADER INFORMATION PROVIDED BY
C                      THE CALLING MODULE.  IT WILL BE PRINTED WHEN
C                      REQUESTED BY THE CALLING MODULE AND BELOW THE
C                      HEADER OF EACH NEW PAGE.
 
        include 'tspinc/prt.inc'
        include 'tspinc/pageno.inc'
        include 'tspinc/topbot.inc'
 
        integer first,dbsw
        character cch*1,cc*1,ccl*1,ccf*1
        
        save dbsw  

        data dbsw /0/

C               ****************************************
 
        entry dbgprt(num)
           dbsw=num
        return
 
C               ****************************************
 
        entry prtout(num)
 
 
        cch=outbuf(1:1)
        ccl=cch
        ccf=cch
 
C               WRITE ON DEBUG FILE IF SWITCH IS ON
        if( dbsw.gt.0)write( dbug,100) outbuf
 
 
        if (lstdnl .eq. 0) then
           ccl='1'
        endif
 
        if (lstdnf .eq. 0) then
           ccf='1'
        endif
 
        if( crtsw.gt.0 ) then
           outbuf(1:1)=' '
           print 100, outbuf
           outbuf(1:1)=cch
        endif
 
           if( ccl .eq. '0' ) then
              if( lprtsw.gt.0 ) lineno = lineno + 1
 
           else if( ccl .eq. '1' ) then
C                                       PRT AT TOP OF NEXT PG
              if(lprtsw.gt.0) then
                 if (lstdnl .ne. 0) then
                    call prtbtm(lprt,pageno,lineno,maxlin)
                 endif
 
                 call prttop(lprt,pageno,lineno,maxlin)
                 ccl = ' '
                 lstdnl=1
              endif
 
           else if ( ccl .eq. '+' ) then
              if( lprtsw.gt.0 ) lineno = lineno - 1
           endif
 
           if( ccf .eq. '0' ) then
              if( fichsw.gt.0 ) fichln = fichln + 1
 
           else if( ccf .eq. '1' ) then
              if(fichsw.gt.0) then
                 if (lstdnf .ne. 0) then
                    call prtbtm(mfich,fichpg,fichln,fichmx)
                 endif
 
                 call prttop(mfich,fichpg,fichln,fichmx)
                 lstdnf = 1
                 ccf = ' '
              endif
 
           else if ( ccf .eq. '+' ) then
              if( fichsw.gt.0 ) fichln = fichln - 1
           endif
 
C                  PRINT ONLY IF NOT TOO CLOSE TO PAGE FOOT
        if (lstdnl .ne. 0) then
           if( lprtsw.gt.0 ) then
              if(lineno.ge.(maxlin-2)) then
                 call prtbtm(lprt,pageno,lineno,maxlin)
                 call prttop(lprt,pageno,lineno,maxlin)
                 ccl = ' '
              endif
 
              outbuf(1:1) = ccl
              write(lprt,100) outbuf
100           format(a)
              lineno=lineno+1
           endif
 
        endif
 
        if (lstdnf .ne. 0) then
           if( fichsw.gt.0 ) then
 
              if(fichln.ge.(fichmx-2)) then
                 call prtbtm(mfich,fichpg,fichln,fichmx)
                 call prttop(mfich,fichpg,fichln,fichmx)
                 ccf = ' '
              endif
 
              outbuf(1:1) = ccf
              write(mfich,100) outbuf
              fichln = fichln + 1
           endif
        endif
 
        return
 
C               ***************************************
 
C       PRINT SPECIAL MICROFICHE CONTROL ON FICHE FILE ONLY
 
        entry pfomf(num)
           write(mfich,100) outbuf
        return
 
C               ****************************************
 
        entry hedlod
C                       LOAD HEADER MESSAGE WITH NEW TEXT....
                header(1:50) = outbuf(1:50)
        return
 
 
C               ****************************************
 
        entry rpnlod
C                       LOAD REPORT NAME WITH NEW TEXT...
                repnam(1:60)=outbuf(1:60)
        return
 
 
 
C               ****************************************
 
        entry shdlod(num)
 
C                       LOAD SUBHEAD(I) WITH NEW TEXT....
           if(num.gt.0.and.num.lt.6) then
              if(index('0 #',outbuf(1:1)) .eq. 0 )  outbuf(1:1) = ' '
              subhed(num)=outbuf
C
           else
              print 102
102           format(1x,'CALLING ERROR TO PRTOUT',
     1           /' ATTEMPT TO CHANGE SUBHED WITH INVALID SUB')
           endif
 
        return
 
 
 
 
C               ****************************************
 
        entry comlod(num)
C
C                       LOAD COMENT(NUM) WITH NEW TEXT....
C
           if (num .eq. 1) then
              coment(1) = ' '
              coment(2) = ' '
C
           else if (coment(1) .eq. ' ') then
              coment(1) = outbuf
C
           else
              coment(2) = outbuf
           endif
C
        return
C
C               ****************************************
C
        entry forbtm
C
C          FORCE BOTTOM ON BOTH LPT & FICH UNITS.
 
           if (lstdnl .ne. 0) then
              if (lprtsw.gt.0) call prtbtm(lprt,pageno,lineno,maxlin)
              lstdnl=0
           endif
 
           if (lstdnf .ne. 0) then
              if (fichsw.gt.0) call prtbtm(mfich,fichpg,fichln,fichmx)
              lstdnf=0
           endif
C
        return
C
C               ****************************************
C
        entry fortop
 
C          FORCE TOP ON BOTH LPT 8 FICH UNITS.
 
             if (lstdnl .eq. 0) then
                if (lprtsw.gt.0) call prttop(lprt,pageno,lineno,maxlin)
                lstdnl=1
             else
                 if (lprtsw.gt.0) call prtbtm(lprt,pageno,lineno,maxlin)
             endif
 
             if (lstdnf .eq. 0) then
                if (fichsw.gt.0) call prttop(mfich,fichpg,fichln,fichmx)
                lstdnf=1
             else
                if (fichsw.gt.0) call prtbtm(mfich,fichpg,fichln,fichmx)
             endif
 
        return
C
C               ****************************************
C
        entry skipln(num)
C
C               SKIPLN "NUM" BLANK LINES OR TO BOTTOM OF PAGE
 
        outbuf = ' '
 
        if( crtsw .gt. 0 ) then
           do 120 i = 1,num
              print 110
 110          format(1x)
 120       continue
        endif
C
        if( lprtsw .gt. 0 ) then
           first = lineno
           jlast = maxlin - 3
           last = first + num
           if( last .gt. jlast ) last = jlast
           do 130 i = first+1,last
              write(lprt,110)
              lineno = lineno + 1
 130       continue
        endif
C
        if( fichsw .gt. 0 ) then
           first = fichln
           jlast = fichmx - 3
           last = first + num
           if( last .gt. jlast ) last = jlast
           do 140 i = first+1,last
              write(mfich,110)
              fichln = fichln + 1
 140       continue
        endif
C
        return
C
C               ****************************************
C
        entry shdprt
C
C               SUBHEAD PRINT AT THE CURRENT LINE ON THE PAGE.
C               IF YOU'RE TOO CLOSE TO THE FOOT,START A NEW PAGE ANYWAY
C
        if (crtsw.gt.0) then
           do 190 i=1,5
           if (subhed(i).ne.' ') print 100,subhed(i)
  190      continue
        endif
C
        if (lstdnl .eq. 0) then
           call prttop(lprt,pageno,lineno,maxlin)
           lstdnl = 1
        endif
 
        if (lstdnf .eq. 0) then
           call prttop(mfich,fichpg,fichln,fichmx)
           lstdnf = 1
        endif
C
        if( lprtsw.gt.0 ) then
           j=maxlin-9
           if(lineno.ge.j) then
                call prtbtm(lprt,pageno,lineno,maxlin)
                call prttop(lprt,pageno,lineno,maxlin)
C               OTHERWISE WRITE THE SUBHEADER
           endif
C
           do 200 i = 1,5
              if (subhed(i).ne.' ')then
                 if (subhed(i)(1:1).eq.'0') then
                    lineno=lineno + 1
                 else
                    subhed(i)(1:1)= ' '
                 endif
 
                 write(lprt,100) subhed(i)
                 lineno = lineno + 1
              endif
 
 200       continue
        endif
C
        if( fichsw.gt.0 ) then
           j = fichmx - 9
 
           if( fichln.ge.j) then
              call prtbtm(mfich,fichpg,fichln,fichmx)
              call prttop(mfich,fichpg,fichln,fichmx)
C               OTHERWISE WRITE THE SUBHEADER
           endif
 
           do 300 i = 1,5
              if (subhed(i).ne.' ') then
                 if (subhed(i)(1:1).eq. '0') then
                    fichln = fichln + 1
                 else
                    subhed(i)(1:1)= ' '
                 endif
                 write(mfich,100) subhed(i)
                 fichln = fichln + 1
              endif
 300       continue
C
        endif
C
        return
 
C               ****************************************
 
        entry newpg
 
C               FINISH OLD PAGE AND START A NEW PAGE
 
           if(lprtsw.gt.0) then
              if (lstdnl .ne. 0) then
                 call prtbtm(lprt,pageno,lineno,maxlin)
              endif
              call prttop(lprt,pageno,lineno,maxlin)
              lstdnl=1
           endif
C
           if(fichsw.gt.0) then
              if (lstdnf .ne. 0) then
                 call prtbtm(mfich,fichpg,fichln,fichmx)
              endif
              call prttop(mfich,fichpg,fichln,fichmx)
              lstdnf=1
           endif
C
        return
C               ****************************************
 
C               ****************************************
 
        entry chkbtm(num)
 
C               FORCE NEW PAGE/FRAME IF NOT MORE THAN NUM LINES REMAIN
 
          if( lineno .ge. (maxlin - num) ) then
            if(lprtsw.gt.0) then
              if (lstdnl .ne. 0) then
                 call prtbtm(lprt,pageno,lineno,maxlin)
              endif
              call prttop(lprt,pageno,lineno,maxlin)
              lstdnl=1
            endif
          endif
 
          if( fichln .ge. (fichmx - num) ) then
            if(fichsw.gt.0) then
              if (lstdnf .ne. 0) then
                 call prtbtm(mfich,fichpg,fichln,fichmx)
              endif
              call prttop(mfich,fichpg,fichln,fichmx)
              lstdnf=1
            endif
          endif
 
        return
C               ****************************************
        end
