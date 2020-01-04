C    @(#)typnam.f	20.4 1/7/99
      subroutine typnam (type,numtyp) 
  
      include 'ipfinc/com007.inc' 
      include 'ipfinc/prt.inc' 
C 
      character type*1, bscode*16 
  
      equivalence (bscode,bustyp) 
      save 
C 
C     ENTRY TYPNAM: GIVEN "TYPE", DETERMINE "NUMTYP" 
C 
      numtyp = index (bscode,type) 
      if (numtyp .eq. 0) then 
         write (errbuf(1),90) type,bustyp(1) 
   90    format ('0 ILLEGAL BUS SUBTYPE "',a1,'" CHANGED TO "',a1,'".') 
         call prterx ('W',1) 
         numtyp = 1 
      endif 
      return 
      end 
