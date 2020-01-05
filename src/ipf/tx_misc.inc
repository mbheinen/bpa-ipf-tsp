
        integer MAX_3TERM
        parameter (MAX_3TERM = 100)
        integer MAX_ZDATA
        parameter (MAX_ZDATA = 100)

	common /tx_misc/ num_3term,  tx_3term(6,MAX_3TERM),
     &                   num_zdata, tx_zdata(26,MAX_ZDATA)

	integer num_3term,  tx_3term, num_zdata
        real tx_zdata

