$!  -    .COM file for stab case TEST_GOV
$ assign disk25:[POW.TSP.UTILITIES]  DFLT 
$ assign disk25:[POW.TSP.UTILITIES]TEST_GOV.SWI  FOR005
$ assign disk25:[POW.TSP.UTILITIES]TESTGOV.BSE  FOR003
$ assign SWG_EXE_V:TSPMASTER.POST  FOR023
$ assign SWG_OUT:TEST_GOV.SOL  FOR008
$ assign SWG_OUT:TEST_GOV.OUT  FOR006
$ assign PLOT_OUT:TEST_GOV.PLT  PLOT$FILE
$ assign PLOT_OUT:TEST_GOV.PLT  FOR022
$ assign SWG_OUT:TEST_GOV.SWX  FOR011
$ run /nodebug  [wlp7536.tsp]tsp.exe
$ set prot=(s:wred,o:wred,g:wred,w:wred)/log SWG_OUT:TEST_GOV.SOL
$ set prot=(s:wred,o:wred,g:wred,w:wred)/log SWG_OUT:TEST_GOV.OUT
$ set prot=(s:wred,o:wred,g:wred,w:wred)/log PLOT_OUT:TEST_GOV.PLT
$ set prot=(s:wred,o:wred,g:wred,w:wred)/log SWG_OUT:TEST_GOV.SWX
$ @disk24:[POW.TSP.UTILITIES]plot_pdf  PLOT_OUT:TEST_GOV.PLT  plot_10
$ purge/keep=2/log  SWG_OUT:TEST_GOV.OUT
$ purge/keep=2/log  PLOT_OUT:TEST_GOV.PLT
$ purge/keep=2/log  SWG_OUT:TEST_GOV.SWX
$ purge/keep=2/log  SWG_OUT:TEST_GOV.SOL
