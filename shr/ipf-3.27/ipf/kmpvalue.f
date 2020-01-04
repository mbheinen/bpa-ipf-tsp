C    @(#)kmpvalue.f	20.3 2/13/96
	integer function kmpvalue (a, b)

        if (a .lt. b) then
           kmpvalue = -1
        else if (a .eq. b) then
           kmpvalue = 0
        else
           kmpvalue = +1
        endif

        return
        end
