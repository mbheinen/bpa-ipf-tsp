c
        integer MAXERRMSG 
        parameter (MAXERRMSG = 2000)
        common /errmsg/ errm(MAXERRMSG)
        common /errnum/ numerr
        character*133 errm
c
