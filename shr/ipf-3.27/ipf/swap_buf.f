C    @(#)swap_buf.f	20.3 2/13/96
	subroutine swap_buf (m, n)

	common /shellsort/ buffer(1000)
        integer buffer

        junk = buffer(m)
        buffer(m) = buffer(n)
        buffer(n) = junk

        return
        end
