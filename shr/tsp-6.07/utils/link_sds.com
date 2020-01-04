$!
$!  Test architecture type: vax = 1, alpha axp = 2
$!
$   if f$getsyi("arch_type") .eq. 1
$   then
$     tsplib = "tspref:tspl_dbg_vax"
$     ipflib = "disk4:[vax_bld.ipf]ipfl_db_vax"
$     mode = "VAX/VMS"
$     exec = "sds.exe"
$     map = "sds.map"
$   else
$     tsplib = "tspref:tspl_dbg"
$     ipflib = "ipfref:ipfl_db"
$     mode = "ALPHA/VMS"
$     exec = "sds.alfexe"
$     map = "sds.alfmap"
$   endif
$ @compile sds
$ @compile tspref:getpfn
$ @compile tspref:getswn
$ @compile gtswdta
$ @compile tspref:openpf
$ inquire type "  Link SDS debug version (y or n)? "
$ if type .eqs. "y" .or. type .eqs. "Y" .or. type .eqs. ""
$   then
$     debug = "/debug"
$   else
$     debug = ""
$   endif
$ write sys$output "  Linking SDS ''debug' mode ''mode'"
$ link/exe='exec'/cross_reference/map='map' 'debug' -
       disk25:[wlp7536.tsp.utilities]sds, getpfn, getswn, gtswdta, openpf -
      +'ipflib'/inc=(rddtai, ldoldbse, bushinit, bus_hash, init_bdpfdt, -
                     getput, opnfil, prterx, rddac, rddat, rddatx, -
                     loaddc, loadcc, get_user, n_date) -
      +'tsplib'/lib -
      +'ipflib'/lib -
      +sys$library:vaxcrtl/lib
$ pur 
