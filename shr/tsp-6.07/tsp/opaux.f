C    %W% %G%
        function  opaux(ind)                                            
C * * *                                                                 
C * * * THIS FUNCTION CONTAINS TWO ENTRY POINTS:                        
C * * * OPPRNT WHICH READS GENERATOR OUTPUT TABLES AND RETURNS          
C * * * A LOGICAL 1 IF THE DATA IS TO BE PRINTED.                       
C * * * OPPLOT WHICH READ GENERATOR OUTPUT TABLES AND RETURNS           
C * * * A LOGICAL 1 IF THE DATA IS THE BE PLOTTED.                      
C * * * CALLED BY GENOUT.                                               
C * * *                                                                 
        common /outop/ igensw(15)                                       
        logical opaux, opprnt, opplot                                   
        opaux=igensw(ind).ge.4                                          
        return                                                          
                                                                        
        entry opprnt (ind)                                              !dem
C       ENTRY OPPRNT                                                    
        opprnt=igensw(ind).eq.1.or.igensw(ind).eq.3.or.                 
     1        igensw(ind).eq.5.or.igensw(ind).eq.7                      
        return                                                          
                                                                        
        entry opplot (ind)                                              !dem
C       ENTRY OPPLOT                                                    
        opplot=igensw(ind).eq.2.or.igensw(ind).eq.3.or.                 
     1        igensw(ind).eq.6.or.igensw(ind).eq.7                      
        return                                                          
                                                                        
        end                                                             
