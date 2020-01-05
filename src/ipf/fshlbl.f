C    @(#)fshlbl.f	20.3 2/13/96
        subroutine fshlbl( mfich, copies )
        integer mfich, copies
 
C       ROUTINES WRITES THE STANDARD FICHE LABEL RECORD.
 
C       The label record specifies username, account number,
C       number of copies and a job code.
 
C       MFICH is the unit number of the file to write the
C             message on.
 
C       COPIES is the number of copies wanted.
 
C       THIS MESSAGE IS TO INFORM THE FICHE OPERATOR WHEN
C       YOUR MICRO-FICHE IS BEING PROCESSED.
 
        include 'ipfinc/prt.inc'
 
        character usernm*10, jobcod*6
 
C ***   GET THE JOB_CODE
        call jobseq(jobcod)

C ***   GET THE USER INFORMATION
        call get_user( usernm ) 

C                               WRITE THE LABEL ON THE FICHE UNIT
 
        write(outbuf,100)jobcod, copies
  100   format(' .        MICROFICHE JOBCODE IS ',a,
     1          ' With',i3,' copy(ies)' )
        call prtout(1)
        call space(2)
 
        write(mfich,160) jobcod, usernm(1:8), copies
  160   format('$MFLB     ',a6,2x,a8,i5,' COPIES ' )
 
        return
 
        end
