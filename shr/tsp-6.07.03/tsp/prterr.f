C    %W% %G%
        subroutine prterr (errtyp, nbl)                                 
                                                                        
C       PRTERR prints the error message in a uniform format and         
C       maintains a count in errcnt of  the numbers of errors at        
C       each severity level. It reformats the error text and calls      
C       the print routine for actual printing of the messages.          
                                                                        
C       The error message is also saved in common /ERRMSG/ERRM(LIMERR)  
C       for summarizing at the end of the swing output.                 
                                                                        
C       ERRTYP - a character designating the severity level:            
                                                                        
C              I - informative                                          
C              W - warning                                              
C              E - error                                                
C              A - abort                                                
                                                                        
C       NBL - the number of lines encoded in ERRBUF                     
C                                                                       
                                                                        
      include 'tspinc/prt.inc' 
      include 'tspinc/pageno.inc' 
      include 'tspinc/topbot.inc' 
      include 'tspinc/errmsg.inc' 
                                                                        
        character errtyp*1                                              
        character *12 errflg(5)                                         
        character crgctl*1                                              
        integer type, savcrt, svlprt                                    
        integer errno                                                   
        data errflg /'*INFORMATIVE','*** WARNING ','*** ERROR   ',      
     1     '*** FATAL   ','*** ABORT   '/                               
C * * *                                                                 
C* *  *   SAVE LPRT & CRT SWITCHS AND TURN THEM ON..                    
C * * *                                                                 
        svlprt  = lprtsw                                                
        savcrt  = crtsw                                                 
        lprtsw  = 1                                                     
        crtsw = 1                                                       
        type = 0                                                        
                                                                        
                                                                        
        type=index('IWEFA',errtyp)                                      
                                                                        
                                                                        
                                                                        
        if(type.ne.0) then                                              
                                                                        
           errcnt(type)=errcnt(type)+1                                  
                                                                        
C          DON'T PRINT ERROR LEVEL 1 ON CRT OR LPT OR FICHE             
C               (defer to end of pf reports)                            
C                                                                       
           if(type.eq.1) then                                           
              lprtsw=0                                                  
              crtsw=0                                                   
              fichsw=0                                                  
           endif                                                        
                                                                        
        else                                                            
           write (outbuf,140) errtyp                                    
  140      format ('0 ILLEGAL ERROR TYPE ',a,' SET TO "I"')             
           type = 1                                                     
           outbuf(122:) = errflg(type)                                  
           call prtout (1)                                              
        endif                                                           
                                                                        
C***   PRINT LINES OF ERROR MESSAGE                                     
                                                                        
        do 60 i = 1,nbl                                                 
        if (i .eq. 1) then                                              
           if(errbuf(1)(1:1).eq.'0') then                               
              crgctl = '0'                                              
              errbuf(1)(1:1) = ' '                                      
           else                                                         
              crgctl = ' '                                              
           endif                                                        
        else                                                            
           crgctl = ' '                                                 
        endif                                                           
        write (outbuf,150) crgctl,errflg(type),errbuf(i)(1:106),        
     1     errflg(type)                                                 
  150   format(a1,a12,1x,a,1x,a12)                                      
        call prtout (1)                                                 
        if (numerr .lt. LIMERR) then                                    
            numerr=numerr+1                                             
            errm(numerr)=outbuf                                         
        endif                                                           
  60    continue                                                        
                                                                        
        if (errcnt(2)+errcnt(3)+errcnt(4) .eq. LIMERR) then             
           write (lprt,165) LIMERR                                      
165        format(1x,'PRTERR MORE THAN ',i4,' ERRORS. ERROR SUMMARY',   
     1     ' IS ABORTED.---WARNING')                                    
        endif                                                           
                                                                        
C***                    RESTORE LPRT & CRT SWITCHES TO ORIG. VALUE      
                                                                        
        crtsw  = savcrt                                                 
        lprtsw = svlprt                                                 
                                                                        
        return                                                          
        end                                                             
