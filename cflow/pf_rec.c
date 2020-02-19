/* pf_rec.c */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "ft.h"
#include "cflowlib.h"

#define  MAX_CFLOW_BUF    4096            /* note: CFLOW_IPC_BUFF_SIZE = 8192 */

int srv_cmdprs (char *, char *);

/******** debug stuff ** next statement added *********/
/********* #include "cf_util.h" *************/ 
/************ debug stuff **********************/
static int error, ipf_status;        /* "=cflow_ref_input," */
static char Area[]=       "/get_data,type=cflow_input,    table=area,action=";
static char Branch[]=     "/get_data,type=cflow_input,    table=branch,action=";
static char Bus[]=        "/get_data,type=cflow_input,    table=bus,action=";
static char Cbus[]=       "/get_data,type=cflow_input,    table=cbus,action=";
static char Itie[]=       "/get_data,type=cflow_input,    table=itie,action=";
static char Qcurve[]=     "/get_data,type=cflow_input,    table=qcurve,action=";
static char Xdata[]=      "/get_data,type=cflow_input,    table=xdata,action=";
static char Output[]=     "/get_data,type=output    ";       /* "=ref_output" */
static char GetComments[]=   "/get_data,type=comments";
static char PutComments[]=   "/put_data,type=comments";
static char GetAreaList[]=   "/get_data,type=area_list";
static char GetBusList[]=    "/get_data,type=bus_list";
static char GetBsekvList[]=  "/get_data,type=bsekv_list";
static char GetOwnerList[]=  "/get_data,type=owner_list";
static char GetZoneList[]=   "/get_data,type=zone_list";
static char GetRectypeList[]="/get_data,type=record_list";
static char AreaOfZone[]=    "/get_data,type=area_of_zone, zone=";
static char GetBusExists[]=  "/get_data,type=bus_exists,bus=";
static char GetSystemInfo[]= "/get_data,type=system";
static char Eom[]=           "*[EOM]";
static char RtnStat[]=       "return status: ";
static char Changes[]=       "/changes, file=*";
static char Solution[]=      "/solution";
static char Initialize[]=    "/initialize";
static char UserInitDef[]=   "/initdef";
static char UserLoadDef[]=   "/loaddef";
static char UserSubDef[]=    "/subdef,source=";
static char UserReports[]=   "/reports,select user_analysis,file=";
static char SolveArea[]=     "/get_data,type=load_area";
static char SolveRefArea[]=  "/get_data,type=load_ref_area";

#ifdef TEST
char __far pf_cflow_outbuf[CFLOW_IPC_BUFF_SIZE], __far pf_cflow_inbuf[CFLOW_IPC_BUFF_SIZE];
int pf_cflow_ipc() {
   printf(pf_cflow_outbuf);
   return 0;
}
#endif

#define SEPCHARS "\n"

static int query_pf(char *cmd, char *data) {
  char *line, *stat, *p, *p1, *p2;
  int finished, len_cmd, len_data;

  err_buf[0] = '\0';
  reply_pf[0] = '\0';
  len_cmd = strlen (cmd);
  len_data = strlen (data);
  if (len_cmd + len_data > MAX_CFLOW_BUF) {
    strncpy (err_buf, cmd, 80);
    err_buf[80] = '\0';
    cf_exit (1, "Fatal error: pf_rec.c/pf_cflow_outbuf overflow %d + %d > %d \n [%s]\n",
      len_cmd, len_data, MAX_CFLOW_BUF, err_buf);
  }
  if (len_data > 0) {
    sprintf(pf_cflow_outbuf,"%s\n%s\n%s\n",cmd,data,Eom);
  } else {
    sprintf(pf_cflow_outbuf,"%s\n%s\n",cmd,Eom);
  }
  if (cf_debug) {
    printf("************cflow_outbuf=\n%s************\n",pf_cflow_outbuf);
    fflush(stdout);

/************* debug stuff *******************************/
   cf_logQuery(pf_cflow_outbuf);  
/************* debug stuff *******************************/

  }
/*  if (error=pf_cflow_ipc()) return error;*/

  error=srv_cmdprs (pf_cflow_outbuf, pf_cflow_inbuf);
  
  if (cf_debug) {
    printf("************cflow_inbuf=\n%s************\n",pf_cflow_inbuf);
    fflush(stdout);
  }
  
/*  if (error) return error; */

  if ((stat=strstr(pf_cflow_inbuf,RtnStat)) != NULL) {
    ipf_status = (int)atol(stat + strlen(RtnStat));
  } else ipf_status = -1;

/********** Beginning block of test code added 2-Dec-1998 by WLP **********
 * Test for two consecutive "*[EOM]"'s, which denote a truncated buffer.
 * If found, set pointer "p" to the second "*[EOM]"; otherwise, set pointer
 * p to the first "*[EOM]".
 * The basis for this test is due to the idiosyncracy of "strtok".  It will
 * break the original string into NULL-terminated substrings, returning with
 * a pointer to each substring.  One of those pointers will be to either 
 * "*[EOM]"
 */

  p = strstr (pf_cflow_inbuf, "*[EOM]");  
  if (p != NULL) {
    p1 = p;
    p2 = strstr (++p1, "*[EOM]");
    if (p2 != NULL) {
      p = p2;
    }
  } else {
    p = pf_cflow_inbuf;        /* Don't allow a NULL pointer "p" here */
  }
/********** Ending block of test code added 2-Dec-1998 by WLP *************/

  line = strtok(pf_cflow_inbuf,SEPCHARS);
  finished = 0;
  while (line != NULL && finished == 0) {
    if (line[0] == '/') { /* echo of command */

/************* Begin testing - code disabled ***********
    } else if ((line[0] == '*') || ((line[1] == '*') && (line[2] == '*'))
                                || ((line[1] == '*') && (line[2] == 'I'))) {
    /* check if line is error and if it is append to error buffer */
/************* (still) testing ***********
      strcat(err_buf,line);
      strcat(err_buf,"\n");
************** End testing - code disabled *********/

/********** Beginning block of test code added 2-Dec-1998 by WLP **********/
    } else if ((line[0] == '*' && line[1] == '*' && line[2] == '*') ||
               (line[0] == '*' && line[1] == '*' && line[2] == 'I')) {
    /* check if line is error and if it is append to error buffer */
      strcat(err_buf,line);
      strcat(err_buf,"\n");

    } else if (strstr (line, "*[EOM]") == p) {
      finished = 1;
/********** Ending block of test code added 2-Dec-1998 by WLP *************/

    } else {
      strcat(reply_pf,line); /* otherwise send reply back to cflow user */
      strcat(reply_pf,"\n");
    }
    line=strtok(NULL,SEPCHARS);
  }
  return ipf_status;
}

#ifdef MSC
static char __far data[CFLOW_IPC_BUFF_SIZE];
#else
static char data[CFLOW_IPC_BUFF_SIZE];
#endif

int pf_del_area(char *area) {
   strcpy(data,"DA "); 
   strcat(data,area); 
   query_pf(Changes,data);
   return ipf_status;
}
int pf_del_zone(char *zone) {
   strcpy(data,"DZ "); 
   strcat(data,zone); 
   query_pf(Changes,data);
   return ipf_status;
}
int pf_rename_area(char *oldname, char *newname) {
   sprintf(data,"ZA %-10.10s  %-10.10s",oldname,newname); 
   query_pf(Changes,data);
   return ipf_status;
}
int pf_rename_bus(char *oldname, float oldkv, char *newname, float newkv) {
   sprintf(data,"ZB %-8.8s%4.4s  %-8.8s%4.4s",oldname,f2a(oldkv,4,0),newname,f2a(newkv,4,0));
   query_pf(Changes,data);
   return ipf_status;
}
int pf_rename_zone(char *oldname, char *newname) {
   sprintf(data,"Z  %-2.2s%-2.2s",oldname,newname); 
   query_pf(Changes,data);
   return ipf_status;
}
int pf_load_changes(char *filename) {
   char cmd[128];
   strcpy(cmd,"/changes, file="); 
   strcat(cmd,filename);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_load_netdata(char *filename) {
   char cmd[128];
   strcpy(cmd,"/network_data, file="); 
   strcat(cmd,filename);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_load_oldbase(char *filename) {
   char cmd[128];
   strcpy(cmd,"/old_base, file="); 
   strcat(cmd,filename);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_put_inrec(char *record) {
   query_pf(Changes,record);
   return ipf_status;
}
int pf_rec_area(pf_rec *r, char *action) {
   char cmd[128];
   strcpy(cmd,Area);
   strcat(cmd,action);
   switch(toupper(action[0])) {
      case 'F': data[0] = '\0';
                if (action[1] != '\0' && action[1] != ' ') {
                    cv_pf_b2a(data, r);
                }
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'N': if (query_pf(cmd,"")==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'G': cv_pf_b2a(data,r);
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'D': cv_pf_b2a(data,r); 
                data[2]='D'; 
                query_pf(Changes,data); 
                break;
      case 'A': cv_pf_b2a(data,r); 
                query_pf(Changes,data); 
                break;
      case 'M': cv_pf_b2a(data,r); 
                data[2]='M'; 
                query_pf(Changes,data); 
                break;
      case 'O': cv_pf_b2a(data,r); 
                if (query_pf(Output,data)==0) cv_pfs_a2b(r,reply_pf);
                break;
      case 'E': printf("Documented but not coded\n");
      default: fprintf(stderr,"pf_rec_area: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}
int pf_rec_branch(pf_rec *r, char *action) {
   char cmd[128];
   strcpy(cmd,Branch);
   strcat(cmd,action);
   switch(toupper(action[0])) {
      case 'F': data[0] = '\0';
                if (action[1] != '\0' && action[1] != ' ') {
                    cv_pf_b2a(data, r);
                }
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'N': if (query_pf(cmd,"")==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'G': cv_pf_b2a(data,r);
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'D': cv_pf_b2a(data,r); 
                data[2]='D'; 
                query_pf(Changes,data); 
                break;
      case 'A': cv_pf_b2a(data,r); 
                query_pf(Changes,data); 
                break;
      case 'M': cv_pf_b2a(data,r); 
                data[2]='M'; 
                query_pf(Changes,data); 
                break;
      case 'O': cv_pf_b2a(data,r); 
                if (query_pf(Output,data)==0) cv_pfs_a2b(r,reply_pf);
                break;
      default: fprintf(stderr,"pf_rec_branch: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}

int pf_rec_bus(pf_rec *r, char *action) {
   char cmd[128];
   strcpy(cmd,Bus);
   strcat(cmd,action);
   switch(toupper(action[0])) {
      case 'F': data[0] = '\0';
                if (action[1] != '\0' && action[1] != ' ') {
                    cv_pf_b2a(data, r);
                }
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'N': if(query_pf(cmd,"") == 0) cv_pf_a2b(r,reply_pf);
                break;
      case 'G': cv_pf_b2a(data,r); 
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'D': cv_pf_b2a(data,r); 
                data[2]='D'; 
                query_pf(Changes,data); 
                break;
      case 'A': cv_pf_b2a(data,r); 
                query_pf(Changes,data); 
                break;
      case 'M': cv_pf_b2a(data,r); 
                data[2]='M'; 
                query_pf(Changes,data); 
                break;
      case 'O': {  char *line;
                   cv_pf_b2a(data,r);
                   if (query_pf(Output,data)==0) {
                     line=strtok(reply_pf,SEPCHARS);
                     cv_pfs_a2b(r,line);
                   }
/* temp code to fix Paul's problem, inserted 5-3-96, should be used only once
   to create a special version of SIX.EXE then removed */
/*
                   else {
                     line=strtok(reply_pf,SEPCHARS);
                     cv_pfs_a2b(r,line);
                   }
*/
/* end special code fix */
                   break;
                }
      default: fprintf(stderr,"pf_rec_bus: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}

int pf_rec_cbus(pf_rec *r, char *action) {
   char cmd[128];
   strcpy(cmd,Cbus);
   strcat(cmd,action);
   switch(toupper(action[0])) {
      case 'F': data[0] = '\0';
                if (action[1] != '\0' && action[1] != ' ') {
                    cv_pf_b2a(data, r);
                }
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'N': if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'G': cv_pf_b2a(data,r); 
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'D': cv_pf_b2a(data,r); 
                data[2]='D'; 
                query_pf(Changes,data); 
                break;
      case 'A': cv_pf_b2a(data,r); 
                query_pf(Changes,data); 
                break;
      case 'M': cv_pf_b2a(data,r); 
                data[2]='M'; 
                query_pf(Changes,data); 
                break;
      case 'O': cv_pf_b2a(data,r); 
                if (query_pf(Output,data)==0) cv_pfs_a2b(r,reply_pf);
                break;
      default: fprintf(stderr,"pf_rec_cbus: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}
int pf_rec_comments(pf_comments *r, char *action) {
   int i;
   switch(toupper(action[0])) {
      case 'G': if (query_pf(GetComments,"")==0) {
                   int c=1;
                   char *line=strtok(reply_pf,SEPCHARS);
                   while(line != NULL) {
                      if (c==1) strcpy(r->case_name,&line[10]);
                      else if (c==2) strcpy(r->case_desc,&line[10]);
                      else if ((c>=3)&&(c<=5)) strcpy(r->h[c-3],&line[1]);
                      else strcpy(r->c[c-6],&line[1]);
                      ++c;
                      line=strtok(NULL,SEPCHARS);
                   }  
                }
                break;
      case 'M': sprintf(data,"case_id=%s\ncase_ds=%s\nH %s\nH %s\n",
                   r->case_name, r->case_desc, r->h[1], r->h[2]);
                for (i=0; i<20 && *r->c[i]!='\0'; i++) {
                   strcat(data,"C ");  strcat(data, r->c[i]);  strcat(data,"\n");
                }
                query_pf(PutComments,data); 
                break;
      default: fprintf(stderr,"pf_rec_comments: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}
int pf_rec_itie(pf_rec *r, char *action) {
   char cmd[128];
   strcpy(cmd,Itie);
   strcat(cmd,action);
   switch(toupper(action[0])) {
      case 'F': data[0] = '\0';
                if (action[1] != '\0' && action[1] != ' ') {
                    cv_pf_b2a(data, r);
                }
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'N': if (query_pf(cmd,"")==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'G': cv_pf_b2a(data,r); 
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'D': cv_pf_b2a(data,r); 
                data[2]='D'; 
                query_pf(Changes,data); 
                break;
      case 'A': cv_pf_b2a(data,r); 
                query_pf(Changes,data); 
                break;
      case 'M': cv_pf_b2a(data,r); 
                data[2]='M'; 
                query_pf(Changes,data); 
                break;
      case 'O': cv_pf_b2a(data,r); 
                if (query_pf(Output,data)==0) cv_pfs_a2b(r,reply_pf); 
                break;
      default: fprintf(stderr,"pf_rec_itie: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}
int pf_rec_qcurve(pf_rec *r, char *action) {
   char cmd[128];
   strcpy(cmd,Qcurve);
   strcat(cmd,action);
   switch(toupper(action[0])) {
      case 'F': data[0] = '\0';
                if (action[1] != '\0' && action[1] != ' ') {
                    cv_pf_b2a(data, r);
                }
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'N': if (query_pf(cmd,"")==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'G': cv_pf_b2a(data,r); 
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'D': cv_pf_b2a(data,r); 
                data[2]='D'; 
                query_pf(Changes,data); 
                break;
      case 'A': cv_pf_b2a(data,r); 
                query_pf(Changes,data); 
                break;
      case 'M': cv_pf_b2a(data,r); 
                data[2]='M'; 
                query_pf(Changes,data); 
                break;
      default: fprintf(stderr,"pf_rec_qcurve: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}
int pf_rec_xdata(pf_rec *r, char *action) {
   char cmd[128];
   strcpy(cmd,Xdata);
   strcat(cmd,action);
   switch(toupper(action[0])) {
      case 'F': data[0] = '\0';
                if (action[1] != '\0' && action[1] != ' ') {
                    cv_pf_b2a(data, r);
                }
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'N': if (query_pf(cmd,"")==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'G': cv_pf_b2a(data,r); 
                if (query_pf(cmd,data)==0) cv_pf_a2b(r,reply_pf);
                break;
      case 'A': cv_pf_b2a(data,r); 
                query_pf(Changes,data); 
                break;
      case 'M': cv_pf_b2a(data,r); 
                data[2]='M'; 
                query_pf(Changes,data); 
                break;
      case 'O': cv_pf_b2a(data,r); 
                if (query_pf(Output,data)==0) cv_pfs_a2b(r,reply_pf);
                break;
      default: fprintf(stderr,"pf_rec_xdata: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}
int pf_save_changes(char *filename) {
   char cmd[128];
   strcpy(cmd,"/save_file, type=changes, file=");
   strcat(cmd,filename);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_save_newbase(char *filename) {
   char cmd[128];
   strcpy(cmd,"/save_file, type=new_base, file=");
   strcat(cmd,filename);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_save_netdata(char *filename, char *dialect, char *ratings, int size) {
   char cmd[128];
   sprintf(cmd,"/save_file, type=network_data,\n file=%s,\n dialect=%s,\n ratings=%s,\n size=%d\n",
     filename,dialect,ratings,size);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_save_wscc_stab_data(char *filename, char *type) {
   char cmd[128];
   strcpy(cmd,"/save_file, type=wscc_");
   strcat(cmd,type);
   strcat(cmd,"_stability, file=");
   strcat(cmd,filename);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_solution() {
   query_pf(Solution,"");
   return ipf_status;
}
int pf_init() {
   query_pf(Initialize,"");
   return ipf_status;
}
int pf_get_list( char *list, int listlen, int type, char *data ) {
   char *cmd, *buf, *listend;
   int  len;
   switch ( type ) {
      case AREA_LIST:    cmd=GetAreaList;    len = 11; break;
      case BUS_LIST:     cmd=GetBusList;     len = 13; break;
      case KV_LIST:      cmd=GetBsekvList;   len = 7;  break;
      case OWNER_LIST:   cmd=GetOwnerList;   len = 4;  break;
      case REC_TYPE_LIST:cmd=GetRectypeList; len = 3;  break;
      case ZONE_LIST:    cmd=GetZoneList;    len = 3;  break;
      default:
         fprintf( stderr, " pf_get_list : illegal list type\n" );
         return -1;
   }
   if (query_pf(cmd,data)==0) {
      listend = list + (listlen * len);
      buf = reply_pf;
      do {
         if ((*buf) == '\0') break;
         *list = *buf++;
         if ((*list) == '\n') *list='\0';
      } while((list++) < listend);
      do {
         *list = '\0';
      } while((list++) < listend);
   }
   return ipf_status;
}
int pf_area_of_zone(char *area, char *zone) {
   char tzon[5];
   sprintf(tzon,"\"%2.2s\"\0",zone);
   if (query_pf(AreaOfZone,tzon) == 0) {
     strncpy(area,reply_pf,10);
     area[10] = '\0';
   }
   return ipf_status;
}
int pf_case_info(pf_case_stats *r) {                   /* coded by WDR DEC'94 */
  char *info, *cp;
  if (query_pf(GetSystemInfo,"") == 0) {
    printf("%s", reply_pf);       /* get rid of this line later final testing */
    if ( (info = strstr(reply_pf, "PRG_VERS")) != NULL ) {
        sscanf(info, "%*s%*s %10c", r->PF_version);
        cp = strchr(r->PF_version, '\n');
        if (cp == NULL) r->PF_version[10] = '\0';
        else *cp = '\0';
    }
    if ( (info = strstr(info==NULL?reply_pf:info, "BASE_MVA")) != NULL )
        sscanf(info, "%*s%*s %f", &r->base_mva);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_DC_SYS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_DC_systems);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_AREA")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_areas);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_ITIE")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_ities);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_ZONE")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_zones);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_OWN")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_owners);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_BUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_buses);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_AREA_SBUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_area_slack_buses);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_DC_BUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_DC_buses);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_AGC_BUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_AGC_buses);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_BX_BUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_BX_buses);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_ADJ_BUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_adjustable_buses);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_PCT_VAR_BUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_pct_var_ctrl_buses);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_BRN")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_branches);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_CKT")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_circuits);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_DC_LINE")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_DC_lines);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_LTC")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_LTC_xfmrs);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_PHAS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_phase_shifters);
    if ( (info = strstr(info==NULL?reply_pf:info, "SOLN_STATUS")) != NULL )
        sscanf(info, "%*s%*s %d", &r->case_soln_status);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_KV")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_diff_kv);
    if ( (info = strstr(info==NULL?reply_pf:info, "NUM_REC_TYP")) != NULL )
        sscanf(info, "%*s%*s %d", &r->num_rec_types);
   }
   return ipf_status;
}
void pf_init_area(pf_rec *r, char *type, char *name) {
   pf_init_rec(r,AREA);
   put_fld_b(r,type,AREA_TYPE);
   put_fld_b(r,name,AREA_NAME);
}
void pf_init_branch(pf_rec *r, char *type, char *name1, float kv1,
                    char *name2, float kv2, char cid, int sid) {
   pf_init_rec(r,L_LINE);
   put_fld_b(r,type,L_TYPE);
   put_fld_b(r,name1,L_BUS1_NAME);
   r->i.L.bus1_kv = kv1;
   put_fld_b(r,name2,L_BUS2_NAME);
   r->i.L.bus2_kv = kv2;
   r->i.L.ckt_id = cid; /*  put_fld_b(r,cid,L_CKT_ID); */
   r->i.L.section = sid;
}
void pf_init_bus(pf_rec *r, char *type, char *name, float kv) {
   pf_init_rec(r,AC_BUS);
   put_fld_b(r,type,AC_BUS_TYPE);
   put_fld_b(r,name,AC_BUS_NAME);
   r->i.ACbus.kv = kv;
}
void pf_init_cbus(pf_rec *r, char *type, char *owner, char *name,
	  float kv, char *year) {
   pf_init_rec(r,CBUS);
   put_fld_b(r,type,CBUS_TYPE);
   put_fld_b(r,owner,CBUS_OWNER);
   put_fld_b(r,name,CBUS_NAME);
   r->i.cbus.kv = kv;
   put_fld_b(r,year,CBUS_CODE_YEAR);
}
void pf_init_itie(pf_rec *r, char *type, char *area1, char *area2) {
   pf_init_rec(r,ITIE);
   put_fld_b(r,type,ITIE_TYPE);
   put_fld_b(r,area1,ITIE_AREA1_NAME);
   put_fld_b(r,area2,ITIE_AREA2_NAME);
}
void pf_init_qcurve(pf_rec *r, char *type, char *name, float kv) {
   pf_init_rec(r,QCURVE);
   put_fld_b(r,type,QCURVE_TYPE);
   put_fld_b(r,name,QCURVE_BUS_NAME);
   r->i.qcurve.bus_kv = kv;
}
int pf_bus_exists(char *name, float kv) { /* WDRogers 8-17-94 */
   sprintf(data, "\"%-8.8s%6.1f\"", name, kv);
   query_pf(GetBusExists, data);
   return ipf_status;
}
int pf_rec_a2b(char *net_data, pf_rec *r, char *action) {
   switch(toupper(action[0])) {
      case 'I': cv_pf_a2b(r,net_data);
                break;
      case 'O': cv_pfs_a2b(r,net_data);
                break;
      default: fprintf(stderr,"pf_rec_a2b: invalid action code %s\n",action);
               return -1;
   }
   return ipf_status;
}
int pf_rec_b2a(char *net_data, pf_rec *r, char *action) { /* WDRogers 1-23-95 */
   int len;
   switch(toupper(action[0])) {
      case 'I': cv_pf_b2a(net_data,r);
                break;
      case 'A': cv_pf_b2a(net_data,r);
                break;
      case 'D': cv_pf_b2a(net_data,r); net_data[2]='D';
		break;
      case 'M': cv_pf_b2a(net_data,r); net_data[2]='M';
		break;
      case 'O': {  char *line;
                   cv_pf_b2a(data,r); /* convert name and kV data */
                   cv_pfs_b2a(net_data,r); /* get solution data */
                   len = strspn(net_data, " ");
                   strcpy(&data[len], &net_data[len]);/* append solution data */
                   line=strtok(data,SEPCHARS);
                   sprintf(net_data, "%s", line);
                   break;
                }
      default: fprintf(stderr,"pf_rec_b2a: invalid action code %s\n",action);
               return -1;
   }
   return 0;
}
int pf_user_init_def() {
   query_pf(UserInitDef,"");
   return ipf_status;
}
int pf_user_load_def(char *definitions) {
   query_pf(UserLoadDef, definitions);
   return ipf_status;
}
int pf_user_sub_def(char *base) {
   char cmd[128];
   sprintf(cmd, "%s%s", UserSubDef, base);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_user_report(char *filename, char *output, char action) {
  char cmd[128];
  if (toupper(action)=='O') {      /* O - overwrite */
    sprintf(cmd, "%s%s,overwrite=%s", UserReports, filename, output);
  }
  else if (toupper(action)=='A') { /* A - append */
    sprintf(cmd, "%s%s,output=%s", UserReports, filename, output);
  }
  else {
    fprintf(stderr,"pf_user_report: invalid action code %c\n", action);
    return -1;
  }
  query_pf(cmd,"");
  return ipf_status;
}
int pf_user_define(char *symbol, char *id, char *type) {
   char def[128];
   sprintf(def, "> DEFINE_TYPE %s\n LET %s = %s\n", type, symbol, id);
   query_pf(UserLoadDef, def);
   return ipf_status;
}
int pf_user_comment(char *symbol, char *suffix, char *format) {
   char com[128];
   sprintf(com, "C %s%s = $%s%s%s\n", symbol, suffix, symbol, suffix, format);
   query_pf(UserLoadDef, com);
   return ipf_status;
}
int pf_user_quantity(char *symbol, char *suffix, float *quantity) {
   char *sp, search[20];
   sprintf(search, "%s%s = ", symbol, suffix);
   if ( (sp = strstr(reply_pf, search)) != NULL ) {
      strcat(search, "%f");
      if (sscanf(sp, search, quantity) == 1) return 0;
   }
   fprintf(stderr,"pf_user_quantity: cannot find \"%s%s =\"\n", symbol, suffix);
   return -1;
}
int pf_user_string(char *symbol, int length, char *info) {
   char *sp, search[32];
   sprintf(search, "%s = ", symbol);
   if ( (sp = strstr(reply_pf, search)) != NULL ) {
      sprintf(search, "%s = %%%d[^\n]", symbol, length);
      if (sscanf(sp, search, info) == 1) return 0;
   }
   fprintf(stderr,"pf_user_string: cannot find \"%s\"\n", symbol); return -1;
}
int pf_user_branch(char *symbol, pf_rec *r, char *type) {
   char def[128], temp[128], t1, t2, name1[9], name2[9], *bp, id;
   t1 = (r->i.branch.meter==1) ? '*' : ' ';
   t2 = (r->i.branch.meter==2) ? '*' : ' ';
   id = (r->i.branch.ckt_id==' ') ? '#' : r->i.branch.ckt_id;
   strcpy(name1, r->i.branch.bus1_name);
   for ( bp = name1 ; (bp=strchr(bp, ' ')) != NULL ; *bp = '#' );
   strcpy(name2, r->i.branch.bus2_name);
   for ( bp = name2 ; (bp=strchr(bp, ' ')) != NULL ; *bp = '#' );
   switch (toupper(type[0])) {
      case 'P': strcpy(def, "> DEFINE_TYPE BRANCH_P\n"); break;
      case 'Q': strcpy(def, "> DEFINE_TYPE BRANCH_Q\n"); break;
      default : fprintf(stderr,"pf_user_branch: invalid type %s\n", type);
                return -1;
   }
   sprintf(temp, "%s LET %s = %s %4.1f%c %s %4.1f%c %c\n", def, symbol,
      name1, r->i.branch.bus1_kv, t1, name2, r->i.branch.bus2_kv, t2, id);
   sprintf(def, "%sC %s = $%s/F15.7\n", temp, symbol, symbol);
   query_pf(UserLoadDef, def);
   return ipf_status;
}
int pf_user_itie(char *symbol, pf_rec *r, char *type) {
   char def[128], temp[128], name1[11], name2[11], *bp;
   strcpy(name1, r->i.itie.area1_name);
   for ( bp = name1 ; (bp=strchr(bp, ' ')) != NULL ; *bp = '#' );
   strcpy(name2, r->i.itie.area2_name);
   for ( bp = name2 ; (bp=strchr(bp, ' ')) != NULL ; *bp = '#' );
   switch (toupper(type[0])) {
      case 'P': strcpy(def, "> DEFINE_TYPE INTERTIE_P\n"); break;
      case 'Q': strcpy(def, "> DEFINE_TYPE INTERTIE_Q\n"); break;
      case 'S': strcpy(def, "> DEFINE_TYPE INTERTIE_P_SCHEDULED\n"); break;
      default : fprintf(stderr,"pf_user_itie: invalid type %s\n", type);
                return -1;
   }
   sprintf(temp, "%s LET %s = %s %s\n", def, symbol, name1, name2);
   sprintf(def, "%sC %s = $%s/F15.7\n", temp, symbol, symbol);
   query_pf(UserLoadDef, def);
   return ipf_status;
}
int pf_user_bus(char *symbol, pf_rec *r, char *suffix) {
   char def[128], temp[128], name[9], *bp;
   strcpy(name, r->i.ACbus.name);
   for ( bp = name ; (bp=strchr(bp, ' ')) != NULL ; *bp = '#' );
   sprintf(def, "> DEFINE_TYPE BUS_INDEX\n");
   sprintf(temp, "%s LET %s = %s %4.1f\n", def, symbol, name, r->i.ACbus.kv);
   sprintf(def,"%sC %s%s = $%s%s/F15.7\n",temp, symbol, suffix, symbol, suffix);
   query_pf(UserLoadDef, def);
   return ipf_status;
}
int pf_plot(char *cor_filename, char *ps_filename, char *options) {
   char cmd[2*FILENAME_MAX+11];
   sprintf(cmd, "/plot\n %s\n %s\n", cor_filename, ps_filename);
   query_pf(cmd, options);
   return ipf_status;
}
int pf_load_refbase(char *filename) {
   char cmd[128];
   strcpy(cmd,"/get_data,type=load_ref_base, file="); strcat(cmd,filename);
   query_pf(cmd,"");
   return ipf_status;
}
int pf_select_base(char base) {
/* base = 'O'-oldbase; 'R'-refbase; */
  if (toupper(base)=='O') {
    strcpy(Area,       "/get_data,type=cflow_input,    table=area,action=");
    strcpy(Branch,     "/get_data,type=cflow_input,    table=branch,action=");
    strcpy(Bus,        "/get_data,type=cflow_input,    table=bus,action=");
    strcpy(Cbus,       "/get_data,type=cflow_input,    table=cbus,action=");
    strcpy(Itie,       "/get_data,type=cflow_input,    table=itie,action=");
    strcpy(Xdata,      "/get_data,type=cflow_input,    table=xdata,action=");
    strcpy(Output,     "/get_data,type=output");
  }
  else if (toupper(base)=='R') {
    strcpy(Area,       "/get_data,type=cflow_ref_input,table=area,action=");
    strcpy(Branch,     "/get_data,type=cflow_ref_input,table=branch,action=");
    strcpy(Bus,        "/get_data,type=cflow_ref_input,table=bus,action=");
    strcpy(Cbus,       "/get_data,type=cflow_ref_input,table=cbus,action=");
    strcpy(Itie,       "/get_data,type=cflow_ref_input,table=itie,action=");
    strcpy(Xdata,      "/get_data,type=cflow_ref_input,table=xdata,action=");
    strcpy(Output,     "/get_data,type=ref_output");
  }
  else {
    fprintf(stderr,"pf_select_base: invalid action code %c\n", base);
    return 1;
  }
  return 0;
}
int pf_solve_area(char base) {
/* call once after each old base is loaded if area interie solution is needed */
/* base = 'O'-oldbase; 'R'-refbase; */
  if (toupper(base)=='O') {
    query_pf(SolveArea, "");
  }
  else if (toupper(base)=='R') {
    query_pf(SolveRefArea, "");
  }
  else {
    fprintf(stderr,"pf_solve_area: invalid action code %c\n", base);
    return 1;
  }
  return ipf_status;
}
int pf_command(char *command) {
  query_pf(command, "");
  return ipf_status;
}
