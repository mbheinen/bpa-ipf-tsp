error = pf_save_changes ("mychanges.chg");
error = pf_save_netdata ("mychanges.net","WSCC", "NOMINAL", 80);
error = pf_save_newbase ("mychanges.bse");
error = pf_save_wscc_stab_data ("mychanges.asif", "ASCII");