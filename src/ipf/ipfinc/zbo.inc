
        parameter ( MAX_ZBO = MAXBASEKVS * MAXOWNERS / 2 )
        common /zbo/ zn_bs(0:MAXCZN), bs_zn(0:MAXBASEKVS), 
     &               zn_ow(0:MAXCZN), ow_zn(0:MAXOWNERS),
     &               bs_ow(0:MAXBASEKVS), ow_bs(0:MAXOWNERS),
     &               num_zbo, zbo(0:MAX_ZBO), zbo_nxt(0:MAX_ZBO)
        integer zn_bs, bs_zn, zn_ow, ow_zn, bs_ow, ow_bs, zbo, zbo_nxt

