C    @(#)cutting_sub.f	20.8 10/10/96
        subroutine cutting_sub

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/pageno.inc'
        include 'ipfinc/cut.inc'

        character filnam*60,case*10,comfil*60                             
        logical baseok, eof, control, chkbch
        integer status, open_file
c                                                                       
c       Cutting initialization                                          
c                                                                       
        call pfinit
        call initlz(status)
        call set_batch
                                                                        
        case = ' '                                                      
        call am_date(rdate)                                                
        batch = chkbch(batch)                                           
                                                                        
c       Set up the page header                                          

        write (outbuf, 10) prgvsn                                       
   10   format('BPA Cutting Program Version:',a)                        
        call hedlod                                                     
                                                                        
c       Print program version on crt or .log file                       

        write (*, 15) outbuf(1:60)                                          
   15   format(1x,a60)                                                  
                                                                        
                                                                        
c       Blank the report name, subheader and comments                   
        outbuf = ' '                                                    
        call rpnlod                                                     
        call shdlod(1)                                                  
        call shdlod(2)                                                  
        call shdlod(3)                                                  
        call shdlod(4)                                                  
        call shdlod(5)                                                  
        call comlod(1)                                                  
        call comlod(2)                                                  
                                                                        
c       ****************************************************************
c       Initialize program variables (use Powerflow values)             

        inpnm = ' '
        call pfopen                                                     
        call prtime('start CUTTING')                                            

        inquire (unit=inp,name=comfil)                                  
        write (outbuf,235) comfil                                       
  235   format(' * command FILE IS : (', a, ')' )                           
        call prtout (1)                                                 
        if( batch ) then                                                
           outbuf='0[ Batch Run ]'                                      
        else                                                            
           outbuf='0[ Interactive run ]'                                
        endif                                                           
                                                                        
        call prtout(1)                                                  
        call space(1)                                                   
                                                                        
        write (*, 232)
  232   format (' Enter NAME for BUS/BRANCH output file > ',$)          
        read (*, 130) filnam                                              
c
c       Logical busbrn contains complete cut system

        status = open_file (busbrn, filnam, 'F', 'W', iostat)

c       print the first page heading                                    
                                                                        
        inquire (unit=busbrn,name=filnam)                                   
        last = lastch(filnam)
        write (outbuf,234) filnam(1:last)
  234   format(' * BUS/BRANCH file is : (', a, ')' )                        
        call prtout (1)                                                 
                                                                        
  100   write (*, 110)                                                          
  110   format(' ')                                                       
        write (*, 120)                                                          
  120   format(' Enter file name for OLD_BASE > ',$)                      
        read (*, 130) filnam                                                
  130   format(a)                                                         
                                                                        
c       ****************************************************************  
c       open oldbase and load case                                        
                                                                        
        call gtbase(filnam,case,baseok)                                   
                                                                        
c       ****************************************************************  
c       see if we need to go further with this case                       
                                                                        
        if (.not.baseok) then                                           
           write (*, 140) 
  140      format (' Requested case file is not OK')
           goto 100                                                     
        endif                                                           
                                                                        
        last = lastch(filnam)
        write (outbuf,141) filnam(1:last)
  141   format(' * OLD_BASE file is : (', a, ')' )                          
        call prtout (1)                                                 
        case = oldcse                                                   
c
c       Logical busfil contains cut bus data, brndta contains cut
c       branch data.
c                                                                        
        status = open_file (busfil, 'scratch.1', 'F', 'W', iostat)
        status = open_file (brndta, 'scratch.2', 'F', 'W', iostat)
                                                                        
        rewind inp
        control = .true.
        do while (control)
           read (inp,142,end=143) inrcd                                            
  142      format (a)                                                      
           call cutcom                                                     
           control = (index ('[(/',inrcd(1:1)) .ne. 0) 
        enddo
  143   buf = inrcd                                                     
                                                                        
        write (outbuf,144) chase1(1),chase1(34),chase1(35)              
  144   format('CUTTING case: ',a10,' proj: ',2a10 )                    
        call hedlod                                                     
                                                                        
        write (outbuf,10145)
10145   format('$MFFB')
        call pfomf(1)                                                   

        write (outbuf,145) chase1(1),chase1(34),chase1(35)(1:9),rdate   
  145   format('$MFHD     CUTTING case ',a10,1x,a10,a9,a9)              
        call pfomf(1)                                                   
                                                                        
c       **************************************************              
c       read input commands, define cut and retained system             
        call cutdat (nsyst)                                             
        crtsw = 0                                                       
                                                                        
c       **************************************************              
c       perform cutting, merge pi-back branches                         
        call cutbr                                                      
                                                                        
c       **************************************************              
c       list cut systems, add bcd data to file                          

        rewind busbrn                                                       
        do i = 1,nsyst                                              
           write (busbrn,242) i                                                
  242      format('. Network Data - Cut System No. ',i3)                   
           rewind busfil                                                       
           rewind brndta                                                       
           call cutsys (i, busfil, brndta)                                                 
           rewind busfil                                                       
           rewind brndta                                                       
           eof = .false.
           do while (.not. eof)
              read (busfil,260,end=270) buf                                       
  260         format (a)                                                      
              write (busbrn,260) buf                                              
           enddo
  270      continue
           do while (.not. eof)
              read (brndta,260,end=280) buf                                       
              write (busbrn,260) buf                                              
           enddo
  280      continue
        enddo
        call close_file (busfil)                                                 
        call close_file (brndta)                                                 
        call close_file (busbrn)                                                 

        return
        end                                                             
