/*
*                      Interactive PowerFlow (IPF)
*               Bonneville Power Administration, Portland, OR.
*
*         Authors:  D. L. Clark,     J. G. Coleman,     W. L. Powell,
*                   B. E. Rogers,    K. E. Rowell,      J. L. Rutis,
*                   D. M. Stefonek,  D. B. Szymanski,   T.    Liu
*
*
*                              NOTICE
*
* Interactive Powerflow (IPF) was developed by BPA and its contractors
* with about 20% of the cost supported by the Electric Power Research
* Institute (EPRI).  By mutual agreement, as described in EPRI Agreement
* RP2746-03 entitled Graphical User Interface for Powerflow, March, 1992,
* all results of this project--including the computer program and its
* documentation--are to be in the public domain.  In a separate Memorandum
* of Understanding with the Western Systems Coordinating Council (WSCC),
* BPA agreed in March, 1992, to keep WSCC informed of progress, to make
* its best effort to develop the program according to the Guidelines
* adopted by the WSCC Computer Program Management Subcommittee, and to
* make the final results available for possible further development by
* WSCC. 
*
* This notice must appear in all copies of this software and documentation.
*
* This notice shall not be removed from any module. 
*
* 1. There is no charge for program development costs, however, a fee
* covering costs incurred in responding to requests is charged to the
* organization receiving materials non-electronically.  This fee also 
* covers resources, reproduction, media preparation, and shipping.
*
* 2. The BPA does not provide assistance with the conversion of its
* programs to other computer systems.
*
* 3. The BPA does not provide consulting services to its users.
*
* 4. In consideration and receipt or acceptance of programs and related
* documentation, you and your orgranization agree to advise any
* third-party receipients (in writing) that the program(s) and/or
* documetation are in the public domain and available from the BPA in the
* event thay are sold, assigned, or transferered to other orgainzations.
* This process ensures that BPA-developed programs be identified as such
* to recipients.
* 
* Interactive PowerFlow Program (IPF)
* The Interactive PowerFlow Program models the balanced steady-state
* operation of an electric power network.  It is used by power system
* planners and design engineers to investigate electric power networks,
* determine bus voltage distribution, line real and reactive power flows,
* line overloads, system reactive reaquirements, area controls, effect of
* load shedding, generator, dropping, and line outages.
* 
* Currently, the program efficiently uses computer resources through
* the implementation of advanced techniques of large system analyses,
* including the Newton-Raphson method for solving second order algebraic
* equations and sparse-matrix computation techniques.  Typically, it
* solves a 6000 bus network and requires 20 MB of virtual storage.  The
* state-of-the-art Graphical User Interface (GUI) has been integrated into
* the IPF using the X Window System/Motif.
*
* The IPF runs on various platforms which support the X Window
* System/Motif.  At BPA, IPF is used on VAX VMS workstations,  
* DEC Alpha workstations running either OPEN VMS or OSF UNIX,
* and properly configured PCs, typically running the UNIX operating system.
*
* Prepared by:
* Planning Methods Section
* Systems Engineering Branch
* Bonneville Power Administration
* P.O. Box 3621 -- EOHB
* Portland, OR  97208
* May 1994
*
*/
#include <stdio.h>                      /* For printf and so on. */
#include <stdlib.h>
#include <Xm/Text.h>
#include <Xm/MessageB.h>
#include <Mrm/MrmAppl.h>                /* Motif Toolkit and MRM */
#include <X11/X.h>
#ifdef VMS
#define badXtGetApplicationResources
#else
#include <unistd.h>
#endif

/** include definition of resources **/
#include "ipf_ipc.h"
#include "ipf_rsrc.h"
#include "ipfdebug.h"
unsigned long ipfdebug = DB_NoMask;

extern void autostart		();	/* WR */
extern void startup		();	/* WR */
extern void setup_screen_display();	/* WR */
extern void test_graphdata	();	/* WR */
extern Widget widget_id		( char * );

int getXtResources(AppResData *app_resdata, Widget toplevel_widget);
int getResources(AppResData *app_resdata, int argc, char *argv[]);
/* int execsrv(AppResData *app_resdata, int argc, char *argv[]); */
int ipc_startup(AppResData *app_resdata);

/* VUIT routines for the user to call */
void s_error();
int  HashFunction();
int  HashLookup();
int  HashRegister();
void HashCount();
void HashErrorMsg( char *name );
Widget VUIT_Manage();
void VUIT_Unmanage();
void Syntax();
void fetch_widget();

/* Motif Global variables */
Display         *display;		/* Display variable */
XtAppContext    app_context;		/* application context */
Widget		toplevel_widget;	/* Root widget ID of application */
MrmHierarchy	s_MrmHierarchy;		/* MRM database hierarchy ID */
AppResData app_resdata;			/** appplication resources **/

/* Literals from the UIL code (if any) */


/*
 * Global data
 */
static MrmType class_id;		/* Place to keep class ID*/
static MrmType *dummy_class;            /* and class variable. */
static char *db_filename_vec[] =        /* Mrm.hierachy file list. */
  {
#ifdef VMS
"uidpath:gui.uid"
#else
"gui.uid"
/****VUIT Mrm.hierarchy****/
#endif
  };
static int db_filename_num =
                (sizeof db_filename_vec / sizeof db_filename_vec [0]);
char *vuit_dummy_ident_value = "VUIT dummy identifier value";
int i;
#define hash_table_limit 2000
struct HASH_TABLE_STRUCT
    {
    char	*widget_name;
    Widget	id;
    } hash_table[hash_table_limit + 1];

#ifdef WIN32
/** note: the resources in XGUI may need to be changed to ipfgui **/
char szClientName[] = "ipfgui";
#endif

/*
 * Forward declarations
 */
void unmanage_test_all();
void system_test_pcdr();
void test_abort();
void system_test_c();
void quick_exit();
void toolbox_move_toggle();
void edit_applyz();
void edit_bus_closez();
void edit_resetz();
void output_column_numz();
void edit_busz();
void edit_send_to_pfz();
void pf_descrip_form();
void fill_descrip_form();
void send_descrip_to_pf();
void unmanage_report_hint();
void pf_update_alpha_list();
void pf_reset_filter_lists();
void pf_deselect_alpha_list();
void pfAlphaList_map_cb();
void set_double_paper_size();
void set_standard_paper_size();
void redraw_graph_with_new_dply_opts();
void read_print_options();
void manage_plot_options_dialog();
void unmanage_print_display_opt();
void unmanage_print_page_opt();
void unmanage_user_comment_dialog();
void user_comment_dialog_cb();
void change_bus_name_label();
void line_z_calc();
void line_z_units_change();
void line_z_freq_change();
void set_std_cond_values();
void read_lcd_file();
void close_line_z_filesel();
void line_z_edit_insert();
void line_z_edit_replace();
void line_z_edit_delete();
void line_z_edit_bundle();
void line_z_ok_callback();
void int_check();
void save_lcd_cb();
void tap_send();
void tap_changed();
void manage_branch_tap_dialog();
void disable_unimpl_help();
void manage_file_save_coord_opt();
void manage_stability_form();
void file_save_select_cb();
void file_save_select_cancel_cb();
void file_save_select_ok_cb();
void unmanage_save_coord_options();
void unmanage_file_save_coord_opt();
void unmanage_bus_help();
void disable_bus_help();
void set_title_mode_2namebase();
void set_title_mode_linename();
void name_change_file_status_set();
void adjust_color();
void edit_kv_color();
void foreground_toggle();
void edit_colorcell();
void color_reset();
void unmanage_color_dialog();
void init_color_dialog();
void toggle_to_slider_form();
void next_color_block();
void prev_color_block();
void colorcell_edit();
void fill_16_color_form();
void ask_to_replace_XGUI();
void close_cflow_dialog();
void write_coord_plot_dlg_to_db();
void set_volts_relative_of_name();
void deactivate_coord_update();
void activate_coord_update();
void change_bus_icon_display_status();
void set_flow_data_segment();
void set_flag();
void change_gen_icon_display_status();
void change_reac_icon_display_status();
void modifyBusLocation();
void manage_bus_edit_dialog();
void output_column_num();
void bus_edit_autoload_toggle();
void close_area_box();
void get_area_selection();
void manage_area_interchange_box();
void area_control_cb();
void intertie_cb();
void manage_area_intertie_dialog();
void close_area_type_box();
void set_area_cont_edit();
void define_selection_call();
void define_ok_cb();
void define_help_cb();
void screen_display_setup_cb();
void debug_dmgr_cb();
void set_dbase_source();
void get_db_first();
void unmanage_dbase_dialog_cb();
void get_db_next();
void manage_graphelement_sreach_dlg();
void fill_graphelement_fields();
void get_db_1st_link();
void set_search_flg();
void get_db_nxt_link();
void set_bus_icon();
void manage_bus_icon_form();
void save_xgui();
void setup_bus_style();
void set_kv_colors();
void set_overload_colors();
void create_from_scratch();
void file_save_proc();
void help_annotate_get();
void create_new_ipf_bus();
void help_annotate_save();
void help_annotate_remove();
void activate_reports_dialog();
void activate_quit_dialog();
void activate_open_file_dialog();
void apply_file_selections();
void unmanage_reports_dialog();
void get_bus_selection();
void cancel_area_selection_box();
void manage_network_edit_dialog();
void pfinit_cb();
void ipc_commandString_xtoc();
void file_default_set();
void file_name_set();
void unmanage_ipc_com_dialog();
void manage_not_implemented();
void register_name();
void unmanage_help_dialog();
void unmanage_annotate_dialog();
void manage_help_annotate();
void manage_help_dialog();
void help_dialog_page_up();
void help_dialog_page_down();
void pfsolution_cb();
void manage_bus_branch_dialog();
void manage_command_dialog();
void manage_save_dialog();
void unmanage_file_open_dialog();
void manage_cflow_select_dialog();
void cflow_launch_cb();
void cflow_kill_cb();
void cflow_debug_cb();
void manage_error_dialog();
void unmanage_bus_output_dialog();
void send_mod_data_to_powerflow();
void reset_data();
void unmanage_bus_front_box();
void set_bus_type();
void set_default_files();
void unmanage_line_z_dia();
void manage_line_z_dia();
void manage_bus_sect_dialog();
void sect_init();
void sect_ok();
void unmanage_bus_sect_dia();
void manage_line_z_filesel();
void help_file_name_set();
void help_input_callback();
void help_expose_callback();
void print_view_plot();
void line_z_list_number_cb();
void manage_line_z_save_dialog();
void unmanage_line_z_save_dia();
void unmanage_ipf_rep_list_dia();
void manage_ipf_rep_list_dia();
void error_dia_help_cb();
void manage_ipf_alpha_bus_list();
void unmanage_ipf_alpha_bus_list();
void pfAlphaList();
void ipf_alpha_srch_value_chg();
void alpha_bus_list_select();
void unmanage_line_tap_dialog();
void alphanum_check();
void decimal_check();
void digit_check();
void load_all_edit_widget_id();
void printGraphData();
void tools_set_view_mode_cb();
void change_cursor_to();
void edit_bus();
void edit_init();
void edit_reset();
void edit_bus_close();
void edit_send_to_pf();
void edit_apply();
void overstrike();
void sect_bus();
void sect_tie();
void tap_apply();
void tap_ok();
void tap_init();
void tools_zoom_cb();
void unmanage_save_file_dialog();
void pfGetFilterLists();
void manage_pfreport_dia();
void pfGetReport();
void unmanage_pf_report_dia();
void fill_branch_jacket_cb_sb();
void fill_bus_dialog_cb();
void set_cont_type();
void alpha_check();
void manage_area_selection_dialog();
void line_pq_list_cb();
void line_pq_edit_insert();
void line_pq_edit_replace();
void line_pq_edit_delete();
void special_selection_action_cb();
void create_pq_record();
void create_cont_record();
void reports_file_ok_cb();
void process_pq_radio_buttons();
void print_plot();
void manage_print_opt_page();
void manage_print_opt_display();
void data_check();
void unmanage_solve_dialog();
void manage_solve_dialog();
void solve_reset();
void unmanage_cor_edit_dialog();
void cor_selection_edit();
void manage_cflow_socket_dia();
void cancel_bus_settings();
void process_regxfmr_rb();
void unmanage_save_net_dia();
void manage_save_net_dialog();
void create_reac_rec();
void create_line_rec();
void create_xfmr_rec();
void create_xfmr_shift_rec();
void create_equiv_rec();
void create_regxfmr_rec();
void create_dc_2_term_rec();
void create_dc_multi_term_rec();
void set_dia_flow_deflts();
void change_print_plot_opts();
void apply_files();
void process_prtopt_rb();
void refresh_solution_data();
void creategraphtbl();
void draw_area_input();
void clear_solution_data();
void unmanage_printer_select_dia();
void plot_destination_cb();
void manage_printer_select_dia();
void set_printer_selection();
void get_bus_alpha_select();
void set_regxfmr_jckts_cb();
void alphanum_sp_check();
void file_check_and_save_cb();
void file_save_cb();
void exit_ipf_quick();
void exit_ipf();
void set_graph_unit_and_origin_cb();
void fill_area_selection_box2();
void loadArea2();
void send_add_data_to_powerflow();
void send_del_data_to_powerflow();
void exit_gui();
void pfget_solution_params();
void disable_exit_help();
void unmanage_file_new_message_dia();
void manage_file_new_message_dia();
void manage_display_dialog();
void unmanage_coord_file_msg();

/*
 * Names and addresses of callback routines to register with Mrm
 */
static MrmRegisterArg reglist [] = {
{"unmanage_test_all", (caddr_t)unmanage_test_all},
{"system_test_pcdr", (caddr_t)system_test_pcdr},
{"test_abort", (caddr_t)test_abort},
{"system_test_c", (caddr_t)system_test_c},
{"quick_exit", (caddr_t)quick_exit},
{"toolbox_move_toggle", (caddr_t)toolbox_move_toggle},
{"edit_applyz", (caddr_t)edit_applyz},
{"edit_bus_closez", (caddr_t)edit_bus_closez},
{"edit_resetz", (caddr_t)edit_resetz},
{"output_column_numz", (caddr_t)output_column_numz},
{"edit_busz", (caddr_t)edit_busz},
{"edit_send_to_pfz", (caddr_t)edit_send_to_pfz},
{"pf_descrip_form", (caddr_t)pf_descrip_form},
{"fill_descrip_form", (caddr_t)fill_descrip_form},
{"send_descrip_to_pf", (caddr_t)send_descrip_to_pf},
{"unmanage_report_hint", (caddr_t)unmanage_report_hint},
{"pf_update_alpha_list", (caddr_t)pf_update_alpha_list},
{"pf_reset_filter_lists", (caddr_t)pf_reset_filter_lists},
{"pf_deselect_alpha_list", (caddr_t)pf_deselect_alpha_list},
{"pfAlphaList_map_cb", (caddr_t)pfAlphaList_map_cb},
{"set_double_paper_size", (caddr_t)set_double_paper_size},
{"set_standard_paper_size", (caddr_t)set_standard_paper_size},
{"redraw_graph_with_new_dply_opts", (caddr_t)redraw_graph_with_new_dply_opts},
{"read_print_options", (caddr_t)read_print_options},
{"manage_plot_options_dialog", (caddr_t)manage_plot_options_dialog},
{"unmanage_print_display_opt", (caddr_t)unmanage_print_display_opt},
{"unmanage_print_page_opt", (caddr_t)unmanage_print_page_opt},
{"unmanage_user_comment_dialog", (caddr_t)unmanage_user_comment_dialog},
{"user_comment_dialog_cb", (caddr_t)user_comment_dialog_cb},
{"change_bus_name_label", (caddr_t)change_bus_name_label},
{"line_z_calc", (caddr_t)line_z_calc},
{"line_z_units_change", (caddr_t)line_z_units_change},
{"line_z_freq_change", (caddr_t)line_z_freq_change},
{"set_std_cond_values", (caddr_t)set_std_cond_values},
{"read_lcd_file", (caddr_t)read_lcd_file},
{"close_line_z_filesel", (caddr_t)close_line_z_filesel},
{"line_z_edit_insert", (caddr_t)line_z_edit_insert},
{"line_z_edit_replace", (caddr_t)line_z_edit_replace},
{"line_z_edit_delete", (caddr_t)line_z_edit_delete},
{"line_z_edit_bundle", (caddr_t)line_z_edit_bundle},
{"line_z_ok_callback", (caddr_t)line_z_ok_callback},
{"int_check", (caddr_t)int_check},
{"save_lcd_cb", (caddr_t)save_lcd_cb},
{"tap_send", (caddr_t)tap_send},
{"tap_changed", (caddr_t)tap_changed},
{"manage_branch_tap_dialog", (caddr_t)manage_branch_tap_dialog},
{"disable_unimpl_help", (caddr_t)disable_unimpl_help},
{"manage_file_save_coord_opt", (caddr_t)manage_file_save_coord_opt},
{"manage_stability_form", (caddr_t)manage_stability_form},
{"file_save_select_cb", (caddr_t)file_save_select_cb},
{"file_save_select_cancel_cb", (caddr_t)file_save_select_cancel_cb},
{"file_save_select_ok_cb", (caddr_t)file_save_select_ok_cb},
{"unmanage_save_coord_options", (caddr_t)unmanage_save_coord_options},
{"unmanage_file_save_coord_opt", (caddr_t)unmanage_file_save_coord_opt},
{"unmanage_bus_help", (caddr_t)unmanage_bus_help},
{"disable_bus_help", (caddr_t)disable_bus_help},
{"set_title_mode_2namebase", (caddr_t)set_title_mode_2namebase},
{"set_title_mode_linename", (caddr_t)set_title_mode_linename},
{"name_change_file_status_set", (caddr_t)name_change_file_status_set},
{"adjust_color", (caddr_t)adjust_color},
{"edit_kv_color", (caddr_t)edit_kv_color},
{"foreground_toggle", (caddr_t)foreground_toggle},
{"edit_colorcell", (caddr_t)edit_colorcell},
{"color_reset", (caddr_t)color_reset},
{"unmanage_color_dialog", (caddr_t)unmanage_color_dialog},
{"init_color_dialog", (caddr_t)init_color_dialog},
{"toggle_to_slider_form", (caddr_t)toggle_to_slider_form},
{"next_color_block", (caddr_t)next_color_block},
{"prev_color_block", (caddr_t)prev_color_block},
{"colorcell_edit", (caddr_t)colorcell_edit},
{"fill_16_color_form", (caddr_t)fill_16_color_form},
{"ask_to_replace_XGUI", (caddr_t)ask_to_replace_XGUI},
{"close_cflow_dialog", (caddr_t)close_cflow_dialog},
{"write_coord_plot_dlg_to_db", (caddr_t)write_coord_plot_dlg_to_db},
{"set_volts_relative_of_name", (caddr_t)set_volts_relative_of_name},
{"deactivate_coord_update", (caddr_t)deactivate_coord_update},
{"activate_coord_update", (caddr_t)activate_coord_update},
{"change_bus_icon_display_status", (caddr_t)change_bus_icon_display_status},
{"set_flow_data_segment", (caddr_t)set_flow_data_segment},
{"set_flag", (caddr_t)set_flag},
{"change_gen_icon_display_status", (caddr_t)change_gen_icon_display_status},
{"change_reac_icon_display_status", (caddr_t)change_reac_icon_display_status},
{"modifyBusLocation", (caddr_t)modifyBusLocation},
{"manage_bus_edit_dialog", (caddr_t)manage_bus_edit_dialog},
{"output_column_num", (caddr_t)output_column_num},
{"bus_edit_autoload_toggle", (caddr_t)bus_edit_autoload_toggle},
{"close_area_box", (caddr_t)close_area_box},
{"get_area_selection", (caddr_t)get_area_selection},
{"manage_area_interchange_box", (caddr_t)manage_area_interchange_box},
{"area_control_cb", (caddr_t)area_control_cb},
{"intertie_cb", (caddr_t)intertie_cb},
{"manage_area_intertie_dialog", (caddr_t)manage_area_intertie_dialog},
{"close_area_type_box", (caddr_t)close_area_type_box},
{"set_area_cont_edit", (caddr_t)set_area_cont_edit},
{"define_selection_call", (caddr_t)define_selection_call},
{"define_ok_cb", (caddr_t)define_ok_cb},
{"define_help_cb", (caddr_t)define_help_cb},
{"screen_display_setup_cb", (caddr_t)screen_display_setup_cb},
{"debug_dmgr_cb", (caddr_t)debug_dmgr_cb},
{"set_dbase_source", (caddr_t)set_dbase_source},
{"get_db_first", (caddr_t)get_db_first},
{"unmanage_dbase_dialog_cb", (caddr_t)unmanage_dbase_dialog_cb},
{"get_db_next", (caddr_t)get_db_next},
{"manage_graphelement_sreach_dlg", (caddr_t)manage_graphelement_sreach_dlg},
{"fill_graphelement_fields", (caddr_t)fill_graphelement_fields},
{"get_db_1st_link", (caddr_t)get_db_1st_link},
{"set_search_flg", (caddr_t)set_search_flg},
{"get_db_nxt_link", (caddr_t)get_db_nxt_link},
{"set_bus_icon", (caddr_t)set_bus_icon},
{"manage_bus_icon_form", (caddr_t)manage_bus_icon_form},
{"save_xgui", (caddr_t)save_xgui},
{"setup_bus_style", (caddr_t)setup_bus_style},
{"set_kv_colors", (caddr_t)set_kv_colors},
{"set_overload_colors", (caddr_t)set_overload_colors},
{"create_from_scratch", (caddr_t)create_from_scratch},
{"file_save_proc", (caddr_t)file_save_proc},
{"help_annotate_get", (caddr_t)help_annotate_get},
{"create_new_ipf_bus", (caddr_t)create_new_ipf_bus},
{"help_annotate_save", (caddr_t)help_annotate_save},
{"help_annotate_remove", (caddr_t)help_annotate_remove},
{"activate_reports_dialog", (caddr_t)activate_reports_dialog},
{"activate_quit_dialog", (caddr_t)activate_quit_dialog},
{"activate_open_file_dialog", (caddr_t)activate_open_file_dialog},
{"apply_file_selections", (caddr_t)apply_file_selections},
{"unmanage_reports_dialog", (caddr_t)unmanage_reports_dialog},
{"get_bus_selection", (caddr_t)get_bus_selection},
{"cancel_area_selection_box", (caddr_t)cancel_area_selection_box},
{"manage_network_edit_dialog", (caddr_t)manage_network_edit_dialog},
{"pfinit_cb", (caddr_t)pfinit_cb},
{"ipc_commandString_xtoc", (caddr_t)ipc_commandString_xtoc},
{"file_default_set", (caddr_t)file_default_set},
{"file_name_set", (caddr_t)file_name_set},
{"unmanage_ipc_com_dialog", (caddr_t)unmanage_ipc_com_dialog},
{"manage_not_implemented", (caddr_t)manage_not_implemented},
{"register_name", (caddr_t)register_name},
{"unmanage_help_dialog", (caddr_t)unmanage_help_dialog},
{"unmanage_annotate_dialog", (caddr_t)unmanage_annotate_dialog},
{"manage_help_annotate", (caddr_t)manage_help_annotate},
{"manage_help_dialog", (caddr_t)manage_help_dialog},
{"help_dialog_page_up", (caddr_t)help_dialog_page_up},
{"help_dialog_page_down", (caddr_t)help_dialog_page_down},
{"pfsolution_cb", (caddr_t)pfsolution_cb},
{"manage_bus_branch_dialog", (caddr_t)manage_bus_branch_dialog},
{"manage_command_dialog", (caddr_t)manage_command_dialog},
{"manage_save_dialog", (caddr_t)manage_save_dialog},
{"unmanage_file_open_dialog", (caddr_t)unmanage_file_open_dialog},
{"manage_cflow_select_dialog", (caddr_t)manage_cflow_select_dialog},
{"cflow_launch_cb", (caddr_t)cflow_launch_cb},
{"cflow_kill_cb", (caddr_t)cflow_kill_cb},
{"cflow_debug_cb", (caddr_t)cflow_debug_cb},
{"manage_error_dialog", (caddr_t)manage_error_dialog},
{"unmanage_bus_output_dialog", (caddr_t)unmanage_bus_output_dialog},
{"send_mod_data_to_powerflow", (caddr_t)send_mod_data_to_powerflow},
{"reset_data", (caddr_t)reset_data},
{"unmanage_bus_front_box", (caddr_t)unmanage_bus_front_box},
{"set_bus_type", (caddr_t)set_bus_type},
{"set_default_files", (caddr_t)set_default_files},
{"unmanage_line_z_dia", (caddr_t)unmanage_line_z_dia},
{"manage_line_z_dia", (caddr_t)manage_line_z_dia},
{"manage_bus_sect_dialog", (caddr_t)manage_bus_sect_dialog},
{"sect_init", (caddr_t)sect_init},
{"sect_ok", (caddr_t)sect_ok},
{"unmanage_bus_sect_dia", (caddr_t)unmanage_bus_sect_dia},
{"manage_line_z_filesel", (caddr_t)manage_line_z_filesel},
{"help_file_name_set", (caddr_t)help_file_name_set},
{"help_input_callback", (caddr_t)help_input_callback},
{"help_expose_callback", (caddr_t)help_expose_callback},
{"print_view_plot", (caddr_t)print_view_plot},
{"line_z_list_number_cb", (caddr_t)line_z_list_number_cb},
{"manage_line_z_save_dialog", (caddr_t)manage_line_z_save_dialog},
{"unmanage_line_z_save_dia", (caddr_t)unmanage_line_z_save_dia},
{"unmanage_ipf_rep_list_dia", (caddr_t)unmanage_ipf_rep_list_dia},
{"manage_ipf_rep_list_dia", (caddr_t)manage_ipf_rep_list_dia},
{"error_dia_help_cb", (caddr_t)error_dia_help_cb},
{"manage_ipf_alpha_bus_list", (caddr_t)manage_ipf_alpha_bus_list},
{"unmanage_ipf_alpha_bus_list", (caddr_t)unmanage_ipf_alpha_bus_list},
{"pfAlphaList", (caddr_t)pfAlphaList},
{"ipf_alpha_srch_value_chg", (caddr_t)ipf_alpha_srch_value_chg},
{"alpha_bus_list_select", (caddr_t)alpha_bus_list_select},
{"unmanage_line_tap_dialog", (caddr_t)unmanage_line_tap_dialog},
{"alphanum_check", (caddr_t)alphanum_check},
{"decimal_check", (caddr_t)decimal_check},
{"digit_check", (caddr_t)digit_check},
{"load_all_edit_widget_id", (caddr_t)load_all_edit_widget_id},
{"printGraphData", (caddr_t)printGraphData},
{"tools_set_view_mode_cb", (caddr_t)tools_set_view_mode_cb},
{"change_cursor_to", (caddr_t)change_cursor_to},
{"edit_bus", (caddr_t)edit_bus},
{"edit_init", (caddr_t)edit_init},
{"edit_reset", (caddr_t)edit_reset},
{"edit_bus_close", (caddr_t)edit_bus_close},
{"edit_send_to_pf", (caddr_t)edit_send_to_pf},
{"edit_apply", (caddr_t)edit_apply},
{"overstrike", (caddr_t)overstrike},
{"sect_bus", (caddr_t)sect_bus},
{"sect_tie", (caddr_t)sect_tie},
{"tap_apply", (caddr_t)tap_apply},
{"tap_ok", (caddr_t)tap_ok},
{"tap_init", (caddr_t)tap_init},
{"tools_zoom_cb", (caddr_t)tools_zoom_cb},
{"unmanage_save_file_dialog", (caddr_t)unmanage_save_file_dialog},
{"pfGetFilterLists", (caddr_t)pfGetFilterLists},
{"manage_pfreport_dia", (caddr_t)manage_pfreport_dia},
{"pfGetReport", (caddr_t)pfGetReport},
{"unmanage_pf_report_dia", (caddr_t)unmanage_pf_report_dia},
{"fill_branch_jacket_cb_sb", (caddr_t)fill_branch_jacket_cb_sb},
{"fill_bus_dialog_cb", (caddr_t)fill_bus_dialog_cb},
{"set_cont_type", (caddr_t)set_cont_type},
{"alpha_check", (caddr_t)alpha_check},
{"manage_area_selection_dialog", (caddr_t)manage_area_selection_dialog},
{"line_pq_list_cb", (caddr_t)line_pq_list_cb},
{"line_pq_edit_insert", (caddr_t)line_pq_edit_insert},
{"line_pq_edit_replace", (caddr_t)line_pq_edit_replace},
{"line_pq_edit_delete", (caddr_t)line_pq_edit_delete},
{"special_selection_action_cb", (caddr_t)special_selection_action_cb},
{"create_pq_record", (caddr_t)create_pq_record},
{"create_cont_record", (caddr_t)create_cont_record},
{"reports_file_ok_cb", (caddr_t)reports_file_ok_cb},
{"process_pq_radio_buttons", (caddr_t)process_pq_radio_buttons},
{"print_plot", (caddr_t)print_plot},
{"manage_print_opt_page", (caddr_t)manage_print_opt_page},
{"manage_print_opt_display", (caddr_t)manage_print_opt_display},
{"data_check", (caddr_t)data_check},
{"unmanage_solve_dialog", (caddr_t)unmanage_solve_dialog},
{"manage_solve_dialog", (caddr_t)manage_solve_dialog},
{"solve_reset", (caddr_t)solve_reset},
{"unmanage_cor_edit_dialog", (caddr_t)unmanage_cor_edit_dialog},
{"cor_selection_edit", (caddr_t)cor_selection_edit},
{"manage_cflow_socket_dia", (caddr_t)manage_cflow_socket_dia},
{"cancel_bus_settings", (caddr_t)cancel_bus_settings},
{"process_regxfmr_rb", (caddr_t)process_regxfmr_rb},
{"unmanage_save_net_dia", (caddr_t)unmanage_save_net_dia},
{"manage_save_net_dialog", (caddr_t)manage_save_net_dialog},
{"create_reac_rec", (caddr_t)create_reac_rec},
{"create_line_rec", (caddr_t)create_line_rec},
{"create_xfmr_rec", (caddr_t)create_xfmr_rec},
{"create_xfmr_shift_rec", (caddr_t)create_xfmr_shift_rec},
{"create_equiv_rec", (caddr_t)create_equiv_rec},
{"create_regxfmr_rec", (caddr_t)create_regxfmr_rec},
{"create_dc_2_term_rec", (caddr_t)create_dc_2_term_rec},
{"create_dc_multi_term_rec", (caddr_t)create_dc_multi_term_rec},
{"set_dia_flow_deflts", (caddr_t)set_dia_flow_deflts},
{"change_print_plot_opts", (caddr_t)change_print_plot_opts},
{"apply_files", (caddr_t)apply_files},
{"process_prtopt_rb", (caddr_t)process_prtopt_rb},
{"refresh_solution_data", (caddr_t)refresh_solution_data},
{"creategraphtbl", (caddr_t)creategraphtbl},
{"draw_area_input", (caddr_t)draw_area_input},
{"clear_solution_data", (caddr_t)clear_solution_data},
{"unmanage_printer_select_dia", (caddr_t)unmanage_printer_select_dia},
{"plot_destination_cb", (caddr_t)plot_destination_cb},
{"manage_printer_select_dia", (caddr_t)manage_printer_select_dia},
{"set_printer_selection", (caddr_t)set_printer_selection},
{"get_bus_alpha_select", (caddr_t)get_bus_alpha_select},
{"set_regxfmr_jckts_cb", (caddr_t)set_regxfmr_jckts_cb},
{"alphanum_sp_check", (caddr_t)alphanum_sp_check},
{"file_check_and_save_cb", (caddr_t)file_check_and_save_cb},
{"file_save_cb", (caddr_t)file_save_cb},
{"exit_ipf_quick", (caddr_t)exit_ipf_quick},
{"exit_ipf", (caddr_t)exit_ipf},
{"set_graph_unit_and_origin_cb", (caddr_t)set_graph_unit_and_origin_cb},
{"fill_area_selection_box2", (caddr_t)fill_area_selection_box2},
{"loadArea2", (caddr_t)loadArea2},
{"send_add_data_to_powerflow", (caddr_t)send_add_data_to_powerflow},
{"send_del_data_to_powerflow", (caddr_t)send_del_data_to_powerflow},
{"exit_gui", (caddr_t)exit_gui},
{"pfget_solution_params", (caddr_t)pfget_solution_params},
{"disable_exit_help", (caddr_t)disable_exit_help},
{"unmanage_file_new_message_dia", (caddr_t)unmanage_file_new_message_dia},
{"manage_file_new_message_dia", (caddr_t)manage_file_new_message_dia},
{"manage_display_dialog", (caddr_t)manage_display_dialog},
{"unmanage_coord_file_msg", (caddr_t)unmanage_coord_file_msg}};

static int reglist_num = (sizeof reglist / sizeof reglist[0]);

/*
 * Names and addresses of uil identifiers (if any) to register with Mrm.
 * These identifiers are registered with a dummy value to allow the generated 
 * code to run without error.
 * You can avoid the registration of these identifiers by simplying editing
 * this template file (vuit_main_template_c) and removing the following
 * special format comments:
 *	***VUIT ident registration***
 *	***VUIT identlist size***
 *	***VUIT register identifiers***
 * You can provide your own registration of identifiers by calling your own
 * routine in response to a callback (such as the MrmNcreateCallback for your
 * application's main window), or by modifying this template to call your
 * own registration routine.
 */


/*
 * OS transfer point.  The main routine does all the one-time setup and
 * then calls XtAppMainLoop.
 */
static char sccsid[] = "%W% %G%";
main(int argc, char *argv[])
{
    Arg arglist[2];
    int n;
    char main_window_title[256];
    int not_destroyed;
#ifdef WIN32
    extern void HCLXmInit(void);

    HCLXmInit();
#endif

#ifdef badXtGetApplicationResources
   getResources(&app_resdata, argc, argv);
#endif

    MrmInitialize();			/* Initialize MRM before initializing */
                                        /* the X Toolkit. */
    /* 
     * If we had user-defined widgets, we would register them with Mrm.here. 
     */

    /* 
     * Initialize the X Toolkit. We get back a top level shell widget.
     */
    XtToolkitInitialize();

    app_context = XtCreateApplicationContext();

    sprintf(main_window_title, "INTERACTIVE POWERFLOW GRAPHICAL USERS INTERFACE - Version AUG_07_98");
    display = XtOpenDisplay(
                             app_context,
                             NULL,
                             main_window_title,
                             "XGUI",
                             options,
                             XtNumber(options),
                             &argc,
                             argv
                           );

    if (display == NULL) 
	{
        fprintf(stderr, "%s:  Can't open display\n", argv[0]);
        exit(1);
	}

    n = 0;
    XtSetArg(arglist[n], XmNallowShellResize, True);  n++;
    toplevel_widget = XtAppCreateShell(
                                        main_window_title,
                                        "XGUI",
                                        applicationShellWidgetClass,
                                        display,
                                        arglist,
                                        n
                                      );

  if (display == NULL) 
  {
    fprintf(stderr, "%s:  Can't open display\n", argv[0]);
    exit(1);
  }

  /** all arguments not processed by X report back as errors **/
  if(argc != 1) {
    Syntax(argc, argv);
  }

   /** get the ipf project defined resources **/
#ifndef badXtGetApplicationResources
   getXtResources(&app_resdata, toplevel_widget);
#endif

    ipfdebug = (unsigned long)app_resdata.debug;
    /* 
     * Open the UID files (the output of the UIL compiler) in the hierarchy
     */
    if (MrmOpenHierarchyPerDisplay( display,
		db_filename_num,	/* Number of files. */
		db_filename_vec,	/* Array of file names.  */
		NULL,			/* Default OS extenstion. */
		&s_MrmHierarchy)	/* Pointer to returned MRM ID */
	!=MrmSUCCESS)
        s_error("can't open hierarchy");

MrmRegisterNames (reglist, reglist_num);


VUIT_Manage("gui_main");

/* These widgets must be fetched so their contents can be *
 * accessed before they are made visible to the user      */

    /* 
     * Realize the top level widget.  All managed children now become visible
     */
    XtRealizeWidget(toplevel_widget);

    /* initialize the ipc socket and optionally launch the powerflow server */
    ipc_startup(&app_resdata);

    /** automatically load files identified in XGUI file **/
    startup();
    if(app_resdata.autostart)
      autostart();
    else
    {
      Widget c_wid;
      VUIT_Manage("create_new_coord_file_dlg");     
      if( not_destroyed )
      {
        if( (c_wid = widget_id("create_new_coord_file_dlg")) );
 	{
          Widget hp_wid;
          hp_wid = XmMessageBoxGetChild(c_wid, XmDIALOG_HELP_BUTTON);
          {
            XtDestroyWidget(hp_wid);
          }
	}
      }
    }
 

    /* 
     * Sit around forever waiting to process X-events.  We never leave
     * XtAppMainLoop. From here on, we only execute our callback routines. 
     */
    XtAppMainLoop(app_context);
}


/*
 * All errors are fatal.
 */
void s_error(problem_string)
    char *problem_string;
{
    printf("%s\n", problem_string);
}

Widget VUIT_Manage(widget_name)
    char	*widget_name;
{
  Widget		id = 0;
  Window		pop_window;
  XWindowChanges	values;

  if (HashLookup(widget_name, &id))
    if (XtIsManaged(id))
    {
      pop_window = XtWindow(XtParent(id));
      values.x = values.y = values.width = values.height =
      values.border_width = values.sibling = (unsigned long)NULL;
      values.stack_mode = Above;
      XConfigureWindow(display, pop_window, CWStackMode, &values);
    }
    else
      XtManageChild(id);
  else
  {
    MrmFetchWidget(s_MrmHierarchy, widget_name, toplevel_widget, &id, 
	    &class_id);
    if( id )
    {
      XtManageChild(id);
      HashRegister(widget_name, id);
    }
    else
    {
  printf("WARNING - from VUIT_Manage (gui.c/vuit_main_template line ~820)\n");
  printf("  Unable to FetchWidget (%s)\n", widget_name );
  printf("  Suggestions: Check name spelling, check that it was registered.\n");
    }
  }
  return(id);
}

void VUIT_Unmanage(widget_name)
    char	*widget_name;
{
    Widget	id;

    if (HashLookup(widget_name, &id))
	XtUnmanageChild(id);
}

int HashRegister (widget_name, id)
/*************************************************************************\
*
* Altered by B. Rogers to fix bug 5/18/94.
\*************************************************************************/
    char		*widget_name;
    Widget		id;    
{
    int			ndx;

    for (ndx = HashFunction(widget_name, hash_table_limit);
	((hash_table[ndx].widget_name != NULL) &&
	    (ndx < hash_table_limit));
	ndx++);

    if (hash_table[ndx].widget_name != NULL)
    {						/* WR brackets added */
	for (ndx = 0;
	     ((hash_table[ndx].widget_name != NULL) &&
	      (ndx < hash_table_limit));
	    ndx++);
      if (ndx == hash_table_limit)
      {						/* WR Error msg added */
	printf("*** ERROR **** From HashRegister (vuit_main_template_c\n" );
	printf("    Widget (%s) NOT registered!  (Hash table array is full.)\n",
			widget_name );
	printf("    hash_table_limit must be increased.\n" );
	return (FALSE);
      }
    }

    hash_table[ndx].widget_name = XtCalloc(1, strlen(widget_name) + 1);
    strcpy(hash_table[ndx].widget_name, widget_name);
    hash_table[ndx].id = id;

    return (TRUE);
}


int HashLookup (name, id)
    char		*name;
    Widget		*id;
{
    int			ndx;

    for (ndx = HashFunction(name, hash_table_limit);
	((hash_table[ndx].widget_name != NULL) &&
	    (ndx <= hash_table_limit));
	ndx++)
	if (strcmp(name, hash_table[ndx].widget_name) == 0)
	    {
	    *id = hash_table[ndx].id;
	    return (TRUE);
	    }

    if (ndx > hash_table_limit)
	for (ndx = 0;
	    ((hash_table[ndx].widget_name != NULL) &&
		(ndx <= hash_table_limit));
	    ndx++)
	    {
	    if (strcmp(name, hash_table[ndx].widget_name) == 0)
		{
	 	*id = hash_table[ndx].id;
		return (TRUE);
		}
	    }

    return (FALSE);
}

int HashFunction (name, max)
    char		*name;
    int			max;

{
#define HashVecSize		20	/* plenty for 31 character names */
typedef union
    {
    short int		intname[HashVecSize];	 /* name as vector of ints */
    char		charname[2*HashVecSize]; /* name as vector of chars */
    } HashName;

    HashName		locname;	/* aligned name */
    int			namelen;	/* length of name */
    int			namelim;	/* length limit (fullword size) */
    int			namextra;	/* limit factor remainder */
    int			code = 0;	/* hash code value */
    int			ndx;		/* loop index */


    /*
     * Copy the name into the local aligned union.
     * Process the name as a vector of integers, with some remaining characters.
     * The string is copied into a local union in order to force correct
     * alignment for alignment-sensitive processors.
     */
    strcpy (locname.charname, name);
    namelen = strlen (locname.charname);
    namelim = namelen >> 1;		/* divide by 2 */
    namextra = namelen & 1;		/* remainder */

    /*
     * XOR each integer part of the name together, followed by the trailing
     * 0/1 character
     */
    for ( ndx=0 ; ndx<namelim ; ndx++ )
        code = code ^ ((locname.intname[ndx])<<ndx);

    if ( namextra > 0 )
        code = code ^ ((locname.intname[ndx])&0x00FF);

    return (code&0x7FFF) % max;
}


void HashCount()
{
  int num=0, n;

  for ( n=0; n<=hash_table_limit; n++ )
  {
    if( hash_table[n].widget_name != NULL ) num++;
  }

  printf("\n****************************************************\n");
  printf(  "*  HASH TABLE: length=%d   #filled=%d\n", hash_table_limit, num );
  printf(  "****************************************************\n");
  

}

void HashErrorMsg( char *name )
/***********************************************************************\
* Purpose: Give a partial list of the hash table at the approximate
*		area of the search.
*
* Author: Bill Rogers 		Feb 1995
\***********************************************************************/
{
  int ndx;

  printf("HASH TABLE limit: %d   List of names checked:\n", hash_table_limit );
  for (ndx = HashFunction(name, hash_table_limit);
	((hash_table[ndx].widget_name != NULL) && (ndx <= hash_table_limit));
	ndx++)
  {
    printf("                  %d (%s)\n", ndx,hash_table[ndx].widget_name);
  }
  if( hash_table[ndx].widget_name != NULL )
    printf("                  %d (%s)\n", ndx,hash_table[ndx].widget_name);
  else
    printf("                  %d (NULL)\n", ndx );

  HashCount();
}


/**************************************************************************\
*
*  fetch_widget
*
*  fetches but does not manage any widget that has not been
*  previously fetched or managed
*
*  Author: John Rutis
*
\**************************************************************************/

void fetch_widget(char *widget_name)
{
  Widget		id = 0;
  Window		pop_window;
  XWindowChanges	values;

  if (!HashLookup(widget_name, &id))
  {
    MrmFetchWidget(s_MrmHierarchy, widget_name, toplevel_widget, &id, 
	    &class_id);
    HashRegister(widget_name, id);
  }
}

Widget reparent(widget_name, parent)
    char        *widget_name;
    Widget      parent;
{
  Widget              id = 0;
  MrmType class_id;
  Window              pop_window;
  XWindowChanges      values;

  if (HashLookup(widget_name, &id))
    if (XtIsManaged(id))
    {
      pop_window = XtWindow(XtParent(id));
      values.x = values.y = values.width = values.height =
      values.border_width = values.sibling = (unsigned long)NULL;
      values.stack_mode = Above;
      XConfigureWindow(display, pop_window, CWStackMode, &values);
    }
    else
      XtManageChild(id);
  else
  {
    MrmFetchWidget(s_MrmHierarchy, widget_name, parent, &id, &class_id);
    if( id )
    {
      XtManageChild(id);
      HashRegister(widget_name, id);
    }
    else
    {
  printf("WARNING - from VUIT_Manage (gui.c/vuit_main_template line ~1050)\n");
  printf("  Unable to FetchWidget (%s)\n", widget_name );
  printf("  Suggestions: Check name spelling, check that it was registered.\n");
    }
  }
    return(id);
}

void unmanage_test_all(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("exit_warning_box");
VUIT_Unmanage("area_selection_dialog");
VUIT_Unmanage("area_interchange_box");
VUIT_Unmanage("bus_sect_dialog");
VUIT_Unmanage("bus_edit_dialog");
VUIT_Unmanage("modify_bus_coord_dia");
VUIT_Unmanage("cflow_selection_dialog");
VUIT_Unmanage("cflow_socket_request_dia");
VUIT_Unmanage("open_file_dialog");
VUIT_Unmanage("command_warning_dia");
VUIT_Unmanage("bus_front_box");
VUIT_Unmanage("save_file_dialog");
VUIT_Unmanage("save_network_dialog");
VUIT_Unmanage("stability_save_form");
VUIT_Unmanage("help_dialog");
VUIT_Unmanage("help_annotate_dialog");
VUIT_Unmanage("error_message_dialog");
VUIT_Unmanage("text_input_error_dialog");
VUIT_Unmanage("unimplemented_feature_box");
VUIT_Unmanage("ipc_command_board");
VUIT_Unmanage("ipf_report_list_dialog");
VUIT_Unmanage("ipf_alpha_bus_list_dialog");
VUIT_Unmanage("line_tap_dialog");
VUIT_Unmanage("line_z_filesel");
VUIT_Unmanage("line_z_save_dialog");
VUIT_Unmanage("print_opt_page_dialog");
VUIT_Unmanage("plot_options_dialog");
VUIT_Unmanage("user_comment_dialog");
VUIT_Unmanage("printer_select_dialog");
VUIT_Unmanage("select_reports_dialog");
VUIT_Unmanage("pf_report_dialog");
VUIT_Unmanage("reports_file_select_dia");
VUIT_Unmanage("reports_not_selected_dia");
VUIT_Unmanage("solve_dialog");
VUIT_Unmanage("bus_help_dialog");
VUIT_Unmanage("pf_descp_form");
VUIT_Unmanage("save_base_file_error_box");
VUIT_Unmanage("display_menu_dialog");
VUIT_Unmanage("file_new_message_dia");
VUIT_Unmanage("define_selection_dialog");
system_test_c(w, tag, reason);
}
void system_test_pcdr(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("exit_warning_box");
VUIT_Manage("area_selection_dialog");
VUIT_Manage("area_interchange_box");
manage_bus_edit_dialog(w, tag, reason);
VUIT_Manage("bus_edit_dialog");
VUIT_Manage("modify_bus_coord_dia");
VUIT_Manage("cflow_selection_dialog");
activate_open_file_dialog(w, tag, reason);
set_default_files(w, tag, reason);
VUIT_Manage("command_warning_dia");
VUIT_Manage("bus_front_box");
VUIT_Manage("save_file_dialog");
VUIT_Manage("save_network_dialog");
VUIT_Manage("stability_save_form");
VUIT_Manage("help_dialog");
VUIT_Manage("help_annotate_dialog");
VUIT_Manage("error_message_dialog");
VUIT_Manage("unimplemented_feature_box");
VUIT_Manage("text_input_error_dialog");
VUIT_Manage("ipc_command_board");
VUIT_Manage("ipf_report_list_dialog");
VUIT_Manage("ipf_alpha_bus_list_dialog");
VUIT_Manage("line_tap_dialog");
VUIT_Manage("line_z_save_dialog");
VUIT_Manage("print_opt_page_dialog");
VUIT_Manage("plot_options_dialog");
VUIT_Manage("user_comment_dialog");
VUIT_Manage("printer_select_dialog");
VUIT_Manage("select_reports_dialog");
VUIT_Manage("pf_report_dialog");
VUIT_Manage("reports_file_select_dia");
VUIT_Manage("solve_dialog");
VUIT_Manage("reports_not_selected_dia");
VUIT_Manage("bus_help_dialog");
VUIT_Manage("pf_descp_form");
manage_bus_sect_dialog(w, tag, reason);
VUIT_Manage("line_z_filesel");
VUIT_Manage("display_menu_dialog");
VUIT_Manage("file_new_message_dia");
VUIT_Manage("define_selection_dialog");
VUIT_Manage("save_base_file_error_box");
VUIT_Manage("test_continue_dialog");
}
void test_abort(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("test_continue_dialog");
}
void system_test_c(Widget w, XtPointer tag, XtPointer reason)
{
test_graphdata(w, tag, reason);
}
void quick_exit(Widget w, XtPointer tag, XtPointer reason)
{
exit_ipf_quick(w, tag, reason);
}
void pf_descrip_form(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("pf_descp_form");
}
void unmanage_report_hint(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("report_hint_msg");
}
void manage_plot_options_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("plot_options_dialog");
}
void unmanage_print_display_opt(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("plot_options_dialog");
}
void unmanage_print_page_opt(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("print_opt_page_dialog");
}
void unmanage_user_comment_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("user_comment_dialog");
}
void user_comment_dialog_cb(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("user_comment_dialog");
}
void close_line_z_filesel(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("line_z_filesel");
}
void manage_branch_tap_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("line_tap_dialog");
}
void manage_file_save_coord_opt(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("file_save_coord_options_form");
}
void manage_stability_form(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("stability_save_form");
}
void file_save_select_cancel_cb(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("file_save_select_dia");
}
void unmanage_save_coord_options(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("file_save_coord_options_form");
}
void unmanage_file_save_coord_opt(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("file_save_coord_options_form");
}
void unmanage_bus_help(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("bus_help_dialog");
}
void edit_kv_color(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("color_edit_dialog");
init_color_dialog(w, tag, reason);
}
void unmanage_color_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("color_edit_dialog");
}
void toggle_to_slider_form(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("color_edit_colorcells");
VUIT_Manage("color_edit_slider_box");
VUIT_Unmanage("colorcell_edit_kv_pb");
VUIT_Manage("colorcell_show_pb");
}
void colorcell_edit(Widget w, XtPointer tag, XtPointer reason)
{
edit_colorcell(w, tag, reason);
}
void ask_to_replace_XGUI(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("replace_xgui_question_dialog");
}
void close_cflow_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("cflow_selection_dialog");
}
void manage_bus_edit_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("bus_edit_dialog");
edit_init(w, tag, reason);
}
void close_area_box(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("area_interchange_box");
}
void manage_area_interchange_box(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("area_interchange_box");
}
void manage_area_intertie_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("area_add_type_dialog");
}
void close_area_type_box(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("area_add_type_dialog");
}
void define_selection_call(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("define_selection_dialog");
}
void screen_display_setup_cb(Widget w, XtPointer tag, XtPointer reason)
{
setup_screen_display(w, tag, reason);
}
void debug_dmgr_cb(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("debug_dmgr_dialog");
}
void unmanage_dbase_dialog_cb(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("debug_dmgr_dialog");
}
void manage_graphelement_sreach_dlg(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("graphelement_search_dialog");
}
void manage_bus_icon_form(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("bus_shapes_dialog");
}
void activate_reports_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("select_reports_dialog");
}
void activate_quit_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("exit_warning_box");
}
void activate_open_file_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("open_file_dialog");
}
void apply_file_selections(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("open_file_dialog");
apply_files(w, tag, reason);
}
void unmanage_reports_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("select_reports_dialog");
}
void cancel_area_selection_box(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("area_selection_dialog");
}
void manage_network_edit_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("bus_edit_dialog");
}
void unmanage_ipc_com_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("ipc_command_board");
}
void manage_not_implemented(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("unimplemented_feature_box");
}
void register_name(Widget w, XtPointer tag, XtPointer reason)
{
HashRegister(XtName(w), w);
}
void unmanage_help_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("help_dialog");
}
void unmanage_annotate_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("help_annotate_dialog");
}
void manage_help_annotate(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("help_annotate_dialog");
}
void manage_help_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("help_dialog");
}
void manage_bus_branch_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("bus_branch_select_dialog");
}
void manage_command_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("ipc_command_board");
}
void manage_save_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("save_file_dialog");
}
void unmanage_file_open_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("open_file_dialog");
}
void manage_cflow_select_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("cflow_selection_dialog");
}
void manage_error_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("error_message_dialog");
}
void unmanage_bus_output_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("bus_output_dialog");
}
void unmanage_bus_front_box(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("bus_front_box");
}
void unmanage_line_z_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("line_Z_calc_dialog");
}
void manage_line_z_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("line_Z_calc_dialog");
}
void manage_bus_sect_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("bus_sect_dialog");
sect_init(w, tag, reason);
}
void unmanage_bus_sect_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("bus_sect_dialog");
}
void manage_line_z_filesel(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("line_z_filesel");
}
void print_view_plot(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("print_dialog");
}
void manage_line_z_save_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("line_z_save_dialog");
}
void unmanage_line_z_save_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("line_z_save_dialog");
}
void unmanage_ipf_rep_list_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("ipf_report_list_dialog");
}
void manage_ipf_rep_list_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("ipf_report_list_dialog");
}
void manage_ipf_alpha_bus_list(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("ipf_alpha_bus_list_dialog");
}
void unmanage_ipf_alpha_bus_list(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("ipf_alpha_bus_list_dialog");
}
void unmanage_line_tap_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("line_tap_dialog");
}
void unmanage_save_file_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("save_file_dialog");
}
void manage_pfreport_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("pf_report_dialog");
}
void unmanage_pf_report_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("pf_report_dialog");
}
void manage_area_selection_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("area_selection_dialog");
}
void manage_print_opt_page(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("print_opt_page_dialog");
}
void manage_print_opt_display(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("plot_options_dialog");
}
void unmanage_solve_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("solve_dialog");
}
void manage_solve_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("solve_dialog");
pfget_solution_params(w, tag, reason);
}
void unmanage_cor_edit_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("cor_edit_dia");
}
void manage_cflow_socket_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("cflow_socket_request_dia");
}
void unmanage_save_net_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("save_network_dialog");
}
void manage_save_net_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("save_network_dialog");
}
void unmanage_printer_select_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("printer_select_dialog");
}
void plot_destination_cb(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("printer_select_dia");
}
void manage_printer_select_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("printer_select_dialog");
}
void exit_gui(Widget w, XtPointer tag, XtPointer reason)
{
exit_ipf(w, tag, reason);
exit(0);
}
void unmanage_file_new_message_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("file_new_message_dia");
}
void manage_file_new_message_dia(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("file_new_message_dia");
}
void manage_display_dialog(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Manage("display_menu_dialog");
}
void unmanage_coord_file_msg(Widget w, XtPointer tag, XtPointer reason)
{
VUIT_Unmanage("create_new_coord_file_dlg");
}

