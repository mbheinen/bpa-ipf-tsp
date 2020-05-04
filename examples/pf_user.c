char user_def[1000];
float p_in, kv; 
char user_name[8];
float p; 
pf_rec r;

strcpy(user_def, "> DEFINE_TYPE BRANCH_P\n");
strcat(user_def, " LET A1 = ASTOR#TP 115 SEASIDE 115\n"); 
strcat(user_def, "C ASTOR#TP 115 SEASIDE 115 P_in = $A1");
pf_user_init_def();
pf_user_load_def(user_def);
pf_user_sub_def("base");
printf("%s\n", pf_cflow_outbuf);

pf_rec_branch(&r, "F");
pf_user_init_def();
pf_user_branch("L1", &r, "P");
pf_user_sub_def("base");
pf_user_quantity("L1", "", &p);
printf("BRANCH_P = %6.2f\n", p);

pf_rec_bus(&r, "F"); 
pf_user_init_def(); 
pf_user_bus("V1", &r, ".VK");
pf_user_sub_def("base"); 
pf_user_quantity("V1", ".VK", &v); 
printf("V in kV  = %4.1f\n", v);

pf_user_init_def(); 
pf_user_define("L1", "ASTOR#TP 115 SEASIDE 115", "BRANCH_P"); 
pf_user_define("B1", "CHIEF#JO 500", "BUS_INDEX"); 
pf_user_define("U1", "USER", "OLDBASE"); 
pf_user_comment("L1", "", "/F8.3"); 
pf_user_comment("B1", ".VK", "/F8.3"); 
pf_user_comment("U1", "", "/A7"); 
pf_user_sub_def("base"); 
pf_user_quantity("L1", "", &p_in); 
pf_user_quantity("B1", ".VK", &kv); 
pf_user_string("U1", 7, user_name); 
printf("P_IN = %6.2f, kV = %4.1f, User is %s\n", p_in, kv, user_name);

pf_user_init_def(); 
pf_user_define("L1", "ASTOR#TP 115 SEASIDE 115", "BRANCH_P"); 
pf_user_define("B1", "CHIEF#JO 500", "BUS_INDEX"); 
pf_user_define("U1", "USER", "OLDBASE"); 
pf_user_comment("L1", "", "/F8.3"); 
pf_user_comment("B1", ".VK", "/F8.3"); 
pf_user_comment("U1", "", "/A7"); 
pf_user_sub_def("base"); 
pf_user_quantity("L1", "", &p_in); 
pf_user_quantity("B1", ".VK", &kv); 
pf_user_string("U1", 7, user_name); 
printf("P_IN = %6.2f, kV = %4.1f, User is %s\n", p_in, kv, user_name);

pf_rec_itie(&r, "F"); 
pf_user_init_def(); 
pf_user_itie("I1", &r, "P"); 
pf_user_sub_def("base"); 
pf_user_quantity("I1", "", &p); 
printf("INTERTIE_P = %6.2f\n", p);

strcpy(user_def,"> DEFINE_TYPE BRANCH_P\n");
strcat(user_def," LET A1 = ASTOR#TP 115 SEASIDE 115\n");
strcat(user_def,"C ASTOR#TP 115 SEASIDE 115 P_in = $A1");
pf_user_init_def();
pf_user_load_def(user_def); 
pf_user_sub_def("base");
printf("%s\n", reply_pf);

pf_user_init_def();
pf_user_define("L1", "ASTOR#TP 115 SEASIDE 115", "BRANCH_P");
pf_user_define("B1", "CHIEF#JO 500", "BUS_INDEX");
pf_user_define("U1", "USER", "OLDBASE");
pf_user_comment("L1", "", "/F8.3");
pf_user_comment("B1", ".VK", "/F8.3");
pf_user_comment("U1", "", "/A7");
pf_user_sub_def("base");
pf_user_quantity("L1", "", &p_in);
pf_user_quantity("B1", ".VK", &kv);
pf_user_string("U1", 7, user_name);
printf("P_IN = %6.2f, kV = %4.1f, User is %s\n", p_in, kv, user_name);

pf_cflow_init(argc, argv); 
pf_load_oldbase("fgrove.bse"); 
pf_solution(); 
pf_user_report("sum_define.user", "sum_define.rep");

pf_user_init_def(); 
pf_user_define("L1", "ASTOR#TP 115 SEASIDE 115", "BRANCH_P"); 
pf_user_define("B1", "CHIEF#JO 500", "BUS_INDEX"); 
pf_user_define("U1", "USER", "OLDBASE"); 
pf_user_comment("L1", "", "/F8.3"); 
pf_user_comment("B1", ".VK", "/F8.3"); 
pf_user_comment("U1", "", "/A7"); 
pf_user_sub_def("base"); 
pf_user_quantity("L1", "", &p_in); 
pf_user_quantity("B1", ".VK", &kv); 
pf_user_string("U1", 7, user_name); 
printf("P_IN = %6.2f, kV = %4.1f, User is %s\n", p_in, kv, user_name);
