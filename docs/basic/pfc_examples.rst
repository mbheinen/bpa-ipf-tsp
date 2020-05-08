.. _pfc-examples:

************
PFC Examples
************
This section gives several examples of PFC files that can be used with the ``bpf`` process.

Base Case Example
=================
Here is a basic powerflow run that contains all data in the control file (``ninebus.pfc``).

:: 

  (  POWERFLOW,CASEID=NINEBUS,  PROJECT = EXAMPLES  ) 
  / HEADER 
  H    WSCC Nine Bus Base Case 
  / COMMENT 
  C 
  C  CASEB-1_  NINE-BUS CASE, ON CARDS, THAT IN ADDITION TO TESTING 
  C             THE FEATURES OF CASE A-1, ALSO FEATURES_TRANSFORMERS 
  C            AND SUBTYPE "E" BUSES. 
  C 
  / P_INPUT_LIST,FULL 
  / P_OUTPUT_LIST,FULL 
  / AI_LIST=NONE 
  / P_ANALYSIS_RPT,LEVEL = 4 
  / F_ANALYSIS_RPT,LEVEL = 1 
  / NEW_BASE, FILE = ninebus.bse 
  / NETWORK_DATA 
  B     GEN1 HI    230  2  -0.0  -0.0  0.0 0.0      -0.0 -0.0 -0.0 
  B     GEN2 HI    230  1  -0.0  -0.0  0.0 0.0      -0.0 -0.0 -0.0 
  B     GEN3 HI    230  2  -0.0  -0.0  0.0 0.0      -0.0 -0.0 -0.0 
  B     STA A        230   1125.0  50.0  0.0 0.0    -0.0 -0.0 -0.0 
  B     STA B        230  2  90.     30. 
  B     STA C        230  2100.0   35.0 0.0 0.0     -0.0 -0.0  -0.0 
  BS   GEN1       16.5  2 -0.0    -0.0   0.0 0.0     71.6 -0.0 -0.01040 
  BE   GEN2       18 1   -0.0      -0.0  0.0 0.0    163.0 -0.0 -0.01025 
  BE   GEN3       13.8  2 -0.0    -0.0   0.0 0.0     85.0 -0.0 -0.01025 
  L     GEN1 HI    230 STA B       230  1        1700  9200        7900 
  L     GEN1 HI    230 STA B       230  2        1700  9200        7900 
  L     GEN1 HI    2302STA A      230            1         85               88 
  L     GEN3 HI    230 STA B       230            39       17             179 
  L     STA C        230 GEN3 HI   230            1190  10080   10450 
  L     STA A        230 GEN2 HI   230             32      161           153
  L     GEN2 HI   2302STA C       230              85       72           745 
  T     GEN1 HI   230 GEN1        16.5                 5760  23000 1650 
  T     GEN2 HI   230 GEN2           18                 6250  23000 1800 
  T     GEN3 HI   230 GEN3        13.8                  5860 23000 1380
  (STOP)

A more convenient method to perform the preceding setup is to use a NETWORK_DATA file. The PFC file would look like this::

  (  POWERFLOW,CASEID=NINEBUS,  PROJECT = EXAMPLES  ) 
  / HEADER 
  H      WSCC Nine Bus Base Case 
  / COMMENT 
  C 
  C  CASEB-1_  NINE-BUS CASE, ON CARDS, THAT IN ADDITION TO TESTING 
  C            THE FEATURES OF CASE A-1, ALSO FEATURES TRANSFORMERS 
  C            AND SUBTYPE "E" BUSES. 
  C 
  / NEW_BASE, FILE= ninebus.bse 
  / INCLUDE_CONTROL,FILE = ninebus.ctl 
  / NETWORK_DATA, FILE = ninebus.net 
  (STOP)

Where ``ninebus.ctl`` contains::

  / P_INPUT_LIST,FULL 
  / P_OUTPUT_LIST,FULL 
  / AI_LIST=NONE 
  / P_ANALYSIS_RPT,LEVEL = 4 
  / F_ANALYSIS_RPT,LEVEL = 1

and ``ninebus.net`` contains::

  B     GEN1 HI   230  2  -0.0  -0.0 0.0 0.0    -0.0 -0.0 -0.0 
  B     GEN2 HI   230  1  -0.0 -0.0 0.0 0.0     -0.0 -0.0 -0.0 
  B     GEN3 HI   230  2  -0.0 -0.0 0.0 0.0     -0.0 -0.0 -0.0 
  B     STA A       230 1125.0 50.0 0.0 0.0     -0.0 -0.0 -0.0 
  B     STA B       230 2 90.    30. 
  B     STA C      230 2100.0  35.0 0.0 0.0    -0.0 -0.0 -0.0 
  BS    GEN1    16.5  2  -0.0  -0.0 0.0 0.0     71.6 -0.0 -0.01040 
  BE    GEN2        18 1 -0.0   -0.0 0.0 0.0   163.0 -0.0 -0.01025 
  BE    GEN3     13.8 2 -0.0   -0.0 0.0 0.0     85.0 -0.0 -0.01025 
  L     GEN1 HI  230 STA B     230 1      1700  9200        7900 
  L     GEN1 HI  230 STA B     230 2      1700  9200        7900 
  L     GEN1 HI  2302STA A    230        1     85          88 
  L     GEN3 HI  230 STA B     230        39   17          179 
  L     STA C    230 GEN3 HI   230        1190 10080  10450 
  L     STA A    230 GEN2 HI   230        32   161         153 
  L     GEN2 HI  2302STA C   230         85   72          745 
  T     GEN1 HI  230 GEN1    16.5                5760            23000 1650 
  T     GEN2 HI  230 GEN2      18                 6250            23000 1800 
  T     GEN3 HI  230 GEN3    13.8                5860            23000 1380

Change Case Example
===================
Here is an example of loading a system from a solved old base case, and make data changes, and save a new base.::

  ( POWERFLOW, CASEID = TEST-CHG,  PROJECT = TEST-WSCC-DATA) 
  / NEW_BASE, FILE = 9BUSNEW.BSE 
  / COMMENTS 
  C  CASEB-1_  NINE-BUS CASE, ON CARDS, THAT IN ADDITION TO TESTING THE 
  C             FEATURES OF CASE A-1, ALSO FEATURES  TRANSFORMERS AND 
  C             SUBTYPE "E" BUSES. 
  C                THE BUS_BRANCH FILE AND THE CHANGE FILE ARE REMOTE 
  / INCLUDE_CONTROLS, FILE = TESTCONT.CTL 
  / OLD_BASE, FILE= ninebus.bse 
  / CHANGES, FILE = CHANG.DAT 
  ( STOP - END OF TEST )

.. note::

  PFC language commands are not performed in the order they are encountered in the file, but rather in the order the ``bpf`` program decides is logical.

Merge Case Example 1
====================
Here is an example of merging two systems defined from sepearate solved old base files.::

  ( POWERFLOW, CASEID = TEST-MERGE, PROJECT = TEST-MERGE_OLD_BASE ) 
  /COMMENTS 
  C  CASE 2 - TEST BASE MERGE BY MERGING TWO IDENTICAL BASE SYSTEMS. 
  C           TWO MUTUALLY EXCLUSIVE SUBSYSTEMS ARE INTEGRATED TO 
  C           REGENERATE THE ORIGINAL SYSTEM. 
  C 
  C    EACH SYSTEM IS BUILT FROM DIFFERENT AREAS OF THE SAME OLDBASE 
  . 
  .     control options 
  . 
  / P_INPUTLIST,FULL 
  / F_INPUTLIST,NONE 
  / P_OUTPUTLIST,FULL 
  / F_OUTPUTLIST,NONE 
  / AILIST=FULL 
  . 
  / NEW_BASE, FILE = MERGOLD.BAS 
  . 
  .      DEFINE SUBSYSTEM "AREA 1" 
  . 
  / MERGE_OLD_BASE, SUB_SYSTEM_ID = AREA-1, OLD_BASE = TESTDC.BAS 
  > USE_AIC 
  > SAVE_AREAS 
  A  AREA 1 
  . 
  .      DEFINE SUBSYSTEM "AREA 2"
  / MERGE_OLD_BASE, SUB_SYSTEM_ID = AREA-2,OLD_BASE = TESTDC.BAS 
  > SAVE_ AREAS 
  A  AREA 2 
  . 
  .      SUBSYSTEMS ARE NOW MERGED 
  . 
  .      ( CHANGES ) may now follow 
  . 
  ( STOP )

Merge Case Example 2
====================
Here is an example of merging two topologically overlapping networks into one consolidated network and solvubg the network, creating a new base to be called J86JFY82. Each of the original networks is to be appropriately modified before the merger. The first network is a WSCC base case saved as 86J201.BSE which must be modified by saving areas, excluding buses, renaming buses and excluding certain branches. The second network is the BPA system which will be extracted from the branch file BDFY82W using the extraction date Jan 1986.::

  (POWERFLOW, CASEID = J86FY82, PROJECT = BASEMERGE) 
  /NEWBASE FILE = [APF]J86FY82.BSE 
  . 
  .Note:  composite network will be solved with defaults. 
  . 
  /MERGE_OLD_BASE,SUBSYSID = WSCC_NETWORK,OLD_BASE=86J201.BSE 
  >SAVE_AREAS 
  ..... 
  ..... "A" - records - name fields only 
  ..... 
  >EXCLUDE_BUSES 
  ..... 
  ..... "B" - records - name fields only 
  ..... 
  >RENAME_BUSES 
  ..... 
  ..... rename table 
  ..... 
  >EXCLUDE_BRANCHES 
  ..... 
  ..... "L" - records - name fields only 
  ..... 
  /MERGE_NEW_BASE,SUBSYSID = BPA_NETWORK,BRANCH_DATA=BDFY84,DATE=186 
  ..... 
  ..... "B" - records for BPA system 
  ..... 
  /CHANGES 
  ..... 
  ..... change records 
  ..... 
  (STOP)

Reduction Case Example
======================
Here is an example of reducing an existing network to a desired size and solving the reduced network. Reduction is achieved by retaining only specified zones from the original system. Produce full input/output listings on microfiche. Partial input/output listings (restricted to certain specified zones) will be printed on paper. Give full analysis report on both paper and fiche. In solving the network, regulating transformers will be activated and area-interchange control will be switched to control mode. Provide full listing of area interchange flows.::

  (POWERFLOW, CASEID = A86FY81RED, PROJECT = SAMPLE_PCL) 
  /OLDBASE, FILE = A8601FY81.BA2 
  /REDUCTION 
  >SAVE_ZONES,NA,NB,NC,ND,NE,NF,NG,NH,NI,NJ,NR 
  >SAVE_ZONES 19,17,20,08,PR,27,16 
  /P_INPUT_LIST, ZONES=NA,NB,NC,ND,NE,NF,NG,NH,NI,NJ,NK 
  /P_INPUT_LIST, ZONES = 19,17,20,08,PR 
  /P_OUTPUT_LIST, ZONES= NA,NB,NC,ND,NE,NF,NG,NH,NI,NJ,NK 
  /P_OUTPUT_LIST, ZONES= 19,17,20,08,PR 
  /LTC = ON 
  /AI_CONTROL = CON 
  /AI_LIST = FULL 
  /P_ANALYSIS_RPT, LEVEL = 4 
  (STOP)

