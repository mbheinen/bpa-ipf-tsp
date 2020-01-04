c
        common /alt_base_flags/ alt_base_loaded, tbx_flag
        integer alt_base_loaded
        logical tbx_flag
c
c    alt_base_loaded = status of alternate (ref) base case
c
c       0 : NOT loaded
c       1 : loaded
c
c    tbx_flag = tbx pointer array loaded ?
c
