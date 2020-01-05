C    @(#)prohed.f	20.4 10/10/96
        subroutine prohed
 
C               SET UP THE CASE PROCESS HEADING
C               AND THE FICHE TITLE
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/prt.inc'
 
        write (outbuf,10) chase1(1),chase1(34),chase1(35)
   10   format('PWRFLO case: ',a10,' proj: ',2a10 )
        call hedlod
 
        write (outbuf,20) 
   20   format('$MFFB')
        call pfomf(1)

        write (outbuf,30) chase1(1),chase1(34),chase1(35)(1:9),rdate
   30   format('$MFHD     POWERFLOW case ',a10,1x,a10,a9,a9)
        call pfomf(1)
 
        return
 
        end
