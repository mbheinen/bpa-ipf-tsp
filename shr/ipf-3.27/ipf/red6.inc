C
        integer MAX_RECORD
        parameter (MAX_RECORD = 1000)
        common /red6/ inptsw, inptls, idat, idat2, data(2,MAX_RECORD),
     &                basedt(MAX_RECORD)
        common /red6c/ zdata(MAX_RECORD), busdt(MAX_RECORD), 
     &                 adata(MAX_RECORD)
        dimension idata(2,MAX_RECORD)
        character zdata*2, busdt*8, adata*10
        equivalence (idata,data)
C
