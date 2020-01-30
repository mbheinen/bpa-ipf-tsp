C
        integer MAX_ZONE_DATA
        parameter (MAX_ZONE_DATA = 200)
        integer MAX_CUT_DATA
        parameter (MAX_CUT_DATA = 2000)
        common /cut2/ inptsw, inptls, idat, idat2, 
     &                data(2,MAX_CUT_DATA), basedt(MAX_CUT_DATA)
        common /cut2c/ zdata(MAX_ZONE_DATA), busdt(MAX_CUT_DATA), 
     &                 odata(MAX_CUT_DATA)

        integer idata(2,MAX_ZONE_DATA)
        character zdata*2, busdt*8, odata*3
        equivalence (idata, data)
C
