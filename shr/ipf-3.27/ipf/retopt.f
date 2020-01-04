C    @(#)retopt.f	20.4 11/11/97
        subroutine retopt

        write (*, 10000)
10000   format (' Error - This program should never be called!', /,
     &          '         Replace to call to RETSLN')
        return
        end
