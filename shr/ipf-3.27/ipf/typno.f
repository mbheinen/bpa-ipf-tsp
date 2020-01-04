C    @(#)typno.f	20.1 1/7/99
      subroutine typno (type,numtyp) 
  
      include 'ipfinc/com007.inc' 
      include 'ipfinc/prt.inc' 
C 
      character type*1, bscode*16 
  
      equivalence (bscode,bustyp) 
      save 
  
C     ENTRY TYPNO: GIVEN "NUMTYP", DETERMINE "TYPE" 
C 
      if (numtyp .ge. 1 .and. numtyp .le. 16) then 
         type = bustyp(numtyp) 
      else 
         type = '*' 
      endif 
      return 
      end 
