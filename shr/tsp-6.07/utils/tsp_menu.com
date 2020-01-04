$!    GO_TSP.COM  |
$!----------------+
$!
$! ] Batch file to launch the BPA stability program and accompanying
$!   utility programs.   Bypasses PSAP where  it exists.
$! ] At this time, no in-line parameters are read, all options must be picked
$!   by menus
$! ] This procedure generally calls smaller ones to do the dirty work
$!
$! set noverify
$! - Architecture type: vax = 1, alpha axp = 2
$  src_type = f$getsyi("arch_type") 
$! - Architecture code: vax = "V", alpha axp = "A"
$  src_arch = f$extract (0,1,f$getsyi("arch_name"))
$!
$  parm_echo = 0 
$  echo == "write sys$output "
$  esc == "e"
$  esc[0,7] == 27
$  clrtxt == esc + "[H" + esc + "[2J"
$  assign DISK16:[PSAP.SWING] swg_exe_a /nolog
$  assign disk24:[PSAP.SWING] swg_exe_v /nolog
$!
$  if src_arch .eqs. "V"
$     then
$     t_bque == "@ swg_exe_v:batque.com"
$     compdate == "$swg_exe_v:compdate.vaxexe"
$     kmenu == "$swg_exe_v:keymenu.vaxexe"
$     kmenuwd == "$swg_exe_v:keymenuwd.vaxexe"
$     t_pltdev == "@ swg_exe_v:pltdev.com"
$     subtype == "@ swg_exe_v:subtype.com"
$     swginit == "$swg_exe_v:swginitc.vaxexe"
$     t_versn == "@ swg_exe_v:version.com"
$     browse == "$swg_exe_v:view.vaxexe"
$  else
$     t_bque == "@ swg_exe_v:batque.com"
$     compdate == "$swg_exe_a:compdate.alfexe"
$     kmenu == "$swg_exe_a:keymenu.alfexe"
$     kmenuwd == "$swg_exe_a:keymenuwd.alfexe"
$     t_pltdev == "@ swg_exe_v:pltdev.com"
$     subtype == "@ swg_exe_v:subtype.com"
$     swginit == "$swg_exe_a:swginitc.alfexe"
$     t_versn == "@ swg_exe_v:version.com"
$     browse == "$swg_exe_a:view.alfexe"
$  endif
$! -  Ask for submittal type  --> run_type (sym)
$!   inquire cha "  Just before _subtype_ >"
$  run_type == "?" 
$  subtype
$  if run_type .eqs. "Q" then exit
$  if parm_echo 
$    then
$    echo clrtxt
$    echo "Run type: ",run_type
$    echo " "
$    inquire /nopunc cha "      Hit [rtn] to go on >"
$  endif
$!
$!
$! - If batch, ask for a queue to send to  --> bque (sym), targ_os (sym)
$!   inquire cha "  Just before _t_bque_ >"
$  bque == "N/A" 
$  targ_os == src_arch                 ! 
$  if run_type .eqs. "Batch" 
$    then
$    t_bque
$    if bque .eqs. "QUIT" then exit
$    if parm_echo 
$      then
$      echo clrtxt
$      echo "Run type: ",run_type
$      echo "Batch queue: ",bque
$      echo "Machine type: ",targ_os
$      echo " "
$      inquire /nopunc cha "      Hit [rtn] to go on >"
$    endif
$!
$!   -  Time to begin batch run will always be immediate 
$    battime == "0"
$!   -  Always notify user when batch job done  
$    notice ==  "NOTIFY"
$  endif
$!
$! - Ask for a device to plot to --> plt_dev (sym)
$  plt_dev == "?"
$  type swg_exe_v:blank24.dat 
$  t_pltdev
$  if plt_dev .eqs. "QUIT" then exit
$  if parm_echo 
$    then
$    echo clrtxt
$    echo "Run type: ",run_type
$    if run_type .eqs. "Batch"
$      then
$      echo "Batch queue: ",bque
$      echo "Machine type: ",targ_os
$    endif
$    echo "Plot to: ",plt_dev
$    echo " "
$    inquire /nopunc cha "      Hit [rtn] to go on >"
$  endif
$!
$! - Ask for what version to use  --> sw_program (log)
$  define targ_os_l 'targ_os' /nolog
$  define sw_program  "?" /nolog
$  t_versn
$  swprog == f$trnlnm("sw_program")
$  if swprog .eqs. "QUIT" then exit
$  swprog == f$search (swprog) 
$  if swprog .lts. "0" 
$    then 
$    echo " "
$    echo "* * Error: Non-existent TSP executable selected. * *"
$    echo "    TSP_MENU will stop here."
$    echo " "
$    exit
$  endif
$  if parm_echo 
$    then
$    echo clrtxt
$    echo "Run type: ",run_type
$    if run_type .eqs. "Batch"
$      then
$      echo "Batch queue: ",bque
$      echo "Machine type: ",targ_os
$    endif
$    echo "Plot to: ",plt_dev
$    echo "Stab program: ",swprog
$    echo " "
$    inquire /nopunc cha "      Hit [rtn] to go on >"
$  endif
$!
$! - Now get case ID and make case_id & temp files using SWGINITC 
$!  case_id == "?"
$  if run_type .eqs. "Batch"  
$    then
$    run_spec = "Batch|" + bque + "|" + battime + "|" + notice
$  else
$    run_spec = run_type
$  endif
$! 
$  type swg_exe_v:blank24.dat 
$  swginit 'swprog' 'plt_dev' 'run_spec' 
$  set term /nopasthru 
$  echo esc+"[23;78H"
$  echo " "
$  echo " "
$  if f$search("temp.dat") .les. "!"  
$    then
$    echo "tsp_menu [f] Temporary files missing"
$    exit
$  endif
$  open /read tempfl temp.dat
$  read tempfl case_id 
$!  read tempfl bque  
$  close tempfl
$  purge temp.dat /nolog
$  if case_id .eqs. "QUIT" then exit
$! - setup DCL only
$  case_com == f$search (case_id + ".com")
$  if run_type .eqs. "Setup" 
$    then
$    echo " "
$    echo "Swing set-up completed"
$    echo "  Look for file ",case_com
$    echo " "
$    exit
$  endif 
$! -  batch run
$  if run_type .eqs. "Batch"  
$    then 
$    prity = 100
$    user_dir = f$environment ("DEFAULT")
$    set verify
$    submit /log /name='case_id' /queue='bque' -
       /after='battime' /'notice' /noprint /prior='prity' -
       'user_dir''case_id'.com
$    set noverify
$    exit
$  endif
$! - here if interactive run
$  set term /wid=132
$  @ 'case_com'
$  echo " " 
$  cha = " "
$  inquire /nopunct cha "     Press [RTN] to go on >"
$  set term /wid=80
$  exit
$!
