C    %W% %G%
      subroutine ritecp(cma,iec,nw)
C * * *
C * * * THIS SUBROUTINE WRITES NW WORDS FROM ARRAY CMA INTO
C * * * ARRAY ECSP STARTING AT ECSP(IEC+1).
C * * *
      include 'tspinc/space1.inc'
      dimension cma(*)
C     CALL COPYR (CMA,ECSP(IEC),NW)
      lb = 0
      do 123 la = 1,nw
        ecsp(iec+lb) = cma(la)
        lb = lb + 1
 123  continue
      return

      entry redecp(cma,iec,nw)
C * * *
C * * * THIS IS AN ENTRY POINT IN RITECP.  IT READS NW WORDS
C * * * FROM ARRAY ECSP(IEC+1) INTO ARRAY CMA.
C * * *
C     CALL COPYR (ECSP(IEC),CMA,NW)
      lb = 0
      do 234 la = 1,nw
        cma(la) = ecsp(iec+lb)
        lb = lb + 1
 234  continue
      return
      end
