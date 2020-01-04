C    %W% %G%
      block data stabdat                                                !dem
c
c     Initializes fixed data in COMMON blocks
c     First writing: Apr/09/92 by DEM
c 
      include 'tspinc/params.inc' 
      include 'tspinc/titles.inc' 
      include 'tspinc/param.inc'
      include 'tspinc/amorts.inc'
      include 'tspinc/fltopt.inc'
      include 'tspinc/newton.inc'

c     Initialize /TITLE/ vars 

      data relay/'RELAY     '/                                        
      data remote/'REMOTE    '/                                       
      data series/'SERIES    '/                                       
      data capac/'CAPACITOR '/                                        
      data load/'LOAD      '/                                         
      data repr/'REPRESENT '/                                         
      data shed/'SHEDDING  '/                                         
      data net/'NETTING   '/                                          
      data machn/'MACHINE   '/                                        
      data datx/'DATA      '/                                         
      data blnk10/'          '/                                       
      data swdata/'SWING DATA'/                                       
      data xend/'END'/                                                  
      data direct/'DIRECT    '/                                       
      data currnt/'CURRENT   '/                                       
c   

c     Initialize /PARAM/ vars

      data frqbse /60.0/
c
      data xfact,tdodps,tqodps,tdodph,tqodph / 5 * 0.0 /
      data itskp / 0 /
c 
      data lcomp,ifltsw / 2 * 0 /
c
      data inewts / 0 /

      end
