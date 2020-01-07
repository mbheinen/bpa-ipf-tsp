
        integer MAXBUS, MAXCBS, MAXTBX, MAXGEN, MAXXDT, MAXPVC, MAXREI
        parameter ( MAXBUS  = 13000 )       ! number of study busses
        parameter ( MAXCBS  = MAXBUS*2/3 )  ! continuation buses
        parameter ( MAXTBX  = MAXBUS/5 )    ! spec types (V,Q,G,O,X,F)
        parameter ( MAXGEN  = MAXBUS/10 )   ! type bg and bx buses
        parameter ( MAXXDT  = MAXBUS/16 )   ! type bx buses
        parameter ( MAXPVC  = MAXBUS/50 )   ! %-var controlled gens.
        parameter ( MAXREI  = 300 )         ! max number of REI buses

        integer MAXBRN, MAXBRN2, MAXLTC, MAXPHS, MAXYE
        parameter ( MAXBRN  = MAXBUS*5/2 )  ! branches w/transposes
        parameter ( MAXBRN2 = MAXBRN*2 )    ! branches w/double entry
        parameter ( MAXLTC  = MAXBRN/10 )   ! ltc transformers
        parameter ( MAXPHS  = MAXBRN/100 )  ! phase shifters
        parameter ( MAXYE   = MAXBRN*5/2 )  ! off-diagonal elements

        integer MAXCAR, MAXCAZ, MAXCAZR, MAXCZN, MAXZON, MAXTIE
        parameter ( MAXCAR  =  100 )        ! interchange control areas
        parameter ( MAXCAZ  =  100 )        ! zones per control areas
        parameter ( MAXCAZR =   10 )        ! zones per area record
        parameter ( MAXCZN  = 1200 )        ! zones in all control areas
        parameter ( MAXZON  = MAXCZN )      ! number of zones
        parameter ( MAXTIE  = MAXBRN/10 )   ! controlled area-i ties

        integer MAX2DC, MAXMDC, MAXDCL
        parameter ( MAX2DC  = 15 )            ! two-terminal dc buses
        parameter ( MAXMDC  = 50 )            ! multi-terminal dc buses
        parameter ( MAXDCL  = MAX2DC+MAXMDC ) ! dc lines (2t + mt)

        integer MAXOWN, MAXVLT 
        parameter ( MAXOWN = 250 )          ! Number of owner codes
        parameter ( MAXVLT = 200 )          ! Number of voltage levels

        integer MAXM, MAXOLT 
        parameter ( MAXM    = 250 )         ! LP arrays in aref.cmn
C                                           ! & smallp
        parameter ( MAXOLT  = 1000 )        !   overloaded transformers
	
	integer MAXBUFFER                                      
	parameter (MAXBUFFER = 4000)        ! IPC buffer size (Change this 
c                                           ! to 4000 for Linux)
