C    @(#)chkmzr.f	20.3 2/13/96
        subroutine chkmzr(rdat,adat,n)
C$
C$      YMMDD   NAME    CHANGE DESCRIPTION
C$      90827   AL/     NEW SUBROUTINE USED FOR CHECKING BLANKS
C$                      ON CHANGE CARDS
C
C       THIS SUBROUTINE CHECKS EACH ITEM IN ARAY RDAT(I) AND IF ZERO
C       CHECKS PARALLEL ARAY ADAT(I) FOR BLANK  AND IF IT IS A BLANK
C       A VERY LARGE NEGATIVE NUMBER IS INSERTED INTO RDAT(I)
C
        dimension rdat(n)
        character*(*) adat(n)
C
        do 20 i=1,n
           if(rdat(i).eq.0.0.and.adat(i).eq.' ')   then
              rdat(i)=-1.e30
           endif
20      continue
        return
        end
