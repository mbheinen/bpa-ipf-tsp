C FORTELT FLAG                                                          
C---------------------------------------------------------------------  
C                                                                       
C        RELEASE    3        VERSION    1         JULY 15, 1985         
C                                                                       
C---------------------------------------------------------------------  
C                                                                       
C        DEFINITION OF PRINT FLAGS:                                     
C                  MSGFLG(1)   controls printing of intermediate        
C                              HVDC arrays. (DCREAD)                    
C                  MSGFLG(2)   controls printing of the complete        
C                              HVDC arrays prior to compression,        
C                              and after compression. (DCREAD)          
C                              controls printing of card type,          
C                  MSGFLG(3)   controls printing of the complete        
C                              HVDC arrays after compression. (DCREAD)  
C                  MSGFLG(4)   prints the mismatch values for each      
C                              iteration.  (CONMIS & NETMIS)            
C                  MSGFLG(5)   Controls printing of dc line tables      
C                              in routine CMPRSS                        
C                  MSGFLG(6)   Controls printing of injections in       
C                              routine PQTOAC                           
C                  MSGFLG(7)   Controls printing of intermediate        
C                              dc solutions.                            
C                  MSGFLG(8)   Controls the application of central      
C                              control using CTRFLO.                    
C                  MSGFLG(9)   Controls the application of local        
C                              converter control limits in LIMFLO.      
C                  MSGFLG(10)  Controls the application of discrete     
C                              tap steps using FINFLO.                  
C                                                                       
C                                                                       
      INTEGER           INTERV                                          
C                                                                       
      LOGICAL           MSGFLG(10)                                      
C                                                                       
      COMMON/INDI/      INTERV,       MSGFLG                            
C                                                                       
C---------------------------------------------------------------------  
C     END FLAG                                                          
