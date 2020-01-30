C    @(#)updatetb.f	20.3 2/13/96
        subroutine updatetb (nb, oldjtb, newjtb)
c                                                                       
c       This routine changes the status of a TBX entity to reflect
c       a "hot" start.
c                                                                       
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
c	Global variables used:
c		qnetu(r*8)	
        include 'ipfinc/blank.inc'
c	Global variables used:
c		None
        include 'ipfinc/bus.inc'
c	Global variables used:
c		None	
        include 'ipfinc/cbus.inc'
c	Global variables used:
c		None	
        include 'ipfinc/com008.inc'
c	Global variables used:
c		None	
        include 'ipfinc/lfiles.inc'
c	Global variables used:
c		None	
        include 'ipfinc/oldtbx.inc'
c	Global variables used:
c		oldtbx	
        include 'ipfinc/prt.inc'
c	Global variables used:
c		None	
        include 'ipfinc/tbx.inc'
c	Global variables used:
c		tbx(r*8)
        include 'ipfinc/xdata.inc'
c	Global variables used:
c		None
                                                                        
        integer oldjtb, newjtb

        common /dbkk/ dgkk, dbkk, bkkadj
        double precision dgkk,dbkk, bkkadj                                        
                                                                        
        ltype = oldtbx(1,oldjtb)
        itype = oldtbx(7,oldjtb)                                    

        if (ltype .eq. 2 .and. itype .eq. 3) then              
c                                                                 
c          Type BQ in state Q_min                              
c                                                                       
           qnetu(nb) = tbx(4,newjtb)                                   
           vk = tbx(5,newjtb)                                     
           suscp = tbx(6,newjtb)                                  
           if (suscp .gt. 0.0) then                            
              dbkk = dbkk - suscp                              
              qsuscp = suscp * vk ** 2                         
              qnetu(nb) = qnetu(nb) + qsuscp                             
           endif                                               
        else if (ltype .eq. 2 .and. itype .eq. 4) then         
c                                                                      
c          Type BQ in state Q_max                              
c                                                                       
           qnetu(nb) = tbx(3,newjtb)                                   
           vk = tbx(5,newjtb)                                     
           suscp = tbx(6,newjtb)                                  
           if (suscp .lt. 0.0) then                            
              dbkk = dbkk - suscp                              
              qsuscp = suscp * vk ** 2                         
              qnetu(nb) = qnetu(nb) + qsuscp                             
           endif                                               
        else if (ltype .eq. 3 .and. itype .eq. 2) then         
c                                                                       
C          Type BG in state Q_min                              
c                                                                       
           qnetu(nb) = tbx(4,newjtb)                                   
        else if (ltype .eq. 3 .and. itype .eq. 3) then         
c                                                                       
c          Type BG in state Q_max                              
c                                                                       
           qnetu(nb) = tbx(3,newjtb)                                   
        else if (ltype .eq. 5 .and. itype .eq. 2) then         
c                                                                       
c          Type BX in state Q_min                              
c                                                                       
           qnetu(nb) = tbx(4,newjtb)                                   
        else if (ltype .eq. 5 .and. itype .eq. 3) then         
c                                                                       
c          Type BX in state Q_max                              
c                                                                       
           qnetu(nb) = tbx(3,newjtb)                                   
        endif
        return
        end        
