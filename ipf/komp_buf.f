C    @(#)komp_buf.f	20.3 2/13/96
	integer function komp_buf (m, n)

	common /shellsort/ buffer(1000)
        integer buffer

        komp_buf = buffer(m) - buffer(n)
        return
        end
