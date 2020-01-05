*BEGIN JPILST
C
C       The data structure arguments used with the $GETJPI
C       system service consists of one-4 word item for
C       each JOB/PROCESS as follows :
 
C                   ----------
C                  |
C                  |    BUFFER_LENGTH  ( short word INTEGER*2 )
C                  |
C         ONE      |    ITEM_CODE      ( short word INTEGER*2 )
C         ITEM    <<
C       STRUCTURE  |    BUFFER_ADDRESS ( long word  INTEGER*4 )
C                  |
C                  |    RETURN_LENGTH  ( long word  INTEGER*4 )
C                  |
C                  |
C                   ----------
 
C       where
 
C          BUFFER_LENGTH    is the length of buffer to receive
C                           the specified information.  All buffers
C                           reserved should be VAX Long words, unless
C                           otherwise indicated.
 
C          ITEM_CODE        is the address in the VAX system services
C                           containing the requested information.
C                           They were obtained from the system by
C                           assembling $JPIDEF.
 
C          BUFFER_ADDRESS   indicates where the local variable is to
C                           store the specified information.
 
C          RETURN_LENGTH    is an address of where to store the
C                           length of the returned information.
C                           If equal to 0, no length is returned.
 
        IMPLICIT INTEGER*2  L
        IMPLICIT INTEGER*4  A
 
        INTEGER*4  CPUTIM,PGFLTS,WRKSET,FAULTS,WSSIZE
 
        CHARACTER  PRONAM*15,USERNM*12,ACNAME*8,USER*(*),ACCNT*(*)
 
        LOGICAL BATCH
 
 
 
C       Define the symbolic values to be used in the GETJPI call
C       Values were obtained by assembling the 'JPIDEF' MACRO FILE.
C       These values could change with a subsequent release of the
C       system.
C
        PARAMETER  ( JPI$_ACCOUNT  = '00000203'X ,
     1               JPI$_CPUTIM   = '00000407'X ,
     2               JPI$_PAGEFLTS = '0000040A'X ,
     3               JPI$_PRCNAM   = '0000031C'X ,
     4               JPI$_USERNAME = '00000202'X ,
     5               JPI$_WSSIZE   = '00000411'X )
C
C       Set up the SYS$GETJPI item list data structure as a COMMON
C       (length, location_code, adr_of_buffer, buffer_return_length)
 
        COMMON  /JPILST/
     1          LenACC , LocACC , AdrACC , ArtnAC ,
     2          LenCPU , LocCPU , AdrCPU , ArtnCP ,
     3          LenFLT , LocFLT , AdrFLT , ArtnFL ,
     4          LenPNM , LocPNM , AdrPNM , ArtnPN ,
     5          LenUNM , LocUNM , AdrUNM , ArtnUN ,
     6          LenWSS , LocWSS , AdrWSS , ArtnWS ,
     9          ALLDON
C
C       Set up static values in Item List
C
C       buffer_lengths,     location_codes    , return_buffer_length
 
        DATA    LenACC  /8/, LocACC /JPI$_ACCOUNT/ , ArtnAC /0/,
     1          LenCPU  /4/, LocCPU /JPI$_CPUTIM/  , ArtnCP /0/,
     2          LenFLT  /4/, LocFLT /JPI$_PAGEFLTS/, ArtnFL /0/,
     3          LenPNM /15/, LocPNM /JPI$_PRCNAM/  , ArtnPN /0/,
     4          LenUNM /12/, LocUNM /JPI$_USERNAME/, ArtnUN /0/,
     5          LenWSS  /4/, LocWSS /JPI$_WSSIZE/  , ArtnWS /0/,
     9          ALLDON  /0/
 
C
C       Set up Buffer Addresses at RUN time
C
                AdrACC = %LOC ( ACNAME )
                AdrCPU = %LOC ( CPUTIM )
                AdrFLT = %LOC ( PGFLTS )
                AdrPNM = %LOC ( PRONAM )
                AdrUNM = %LOC ( USERNM )
                AdrWSS = %LOC ( WRKSET )
 
C
C************  JULY 21,1980  ************
C
