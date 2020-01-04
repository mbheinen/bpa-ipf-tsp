
        common /tbx/ tbx(8,MAXTBX), ptrtbx(MAXBUS), tbx_loaded
c
	double precision tbx
        integer ptrtbx, tbx_loaded
c
c       tbx_loaded : 0 = ptrtbx not loaded
c                    1 = ptrtbx loaded in input order
c                    2 = ptrtbx loaded in optimal order
c
