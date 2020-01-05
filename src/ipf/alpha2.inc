
        common /alpha2/ volt(MAXBUS), jndex(2,MAXLTC+MAXBUS),  
     +                  kvolt(MAXBUS), iflag(MAXBUS),                          
     +        jflag(2,2*MAXLTC+2*MAXTIE+2*MAXTBX+2*MAXPVC+MAXCAR+600) 
c
       double precision volt
c
       integer jndex, kvolt, iflag, jflag
c
