/************************************************************************\
* file: graph_data.h
* purpose: 
* author: Dan Clark	December 12, 1991
*
* Used by: base_data.c
*	   busfilter.c
*	   bussect.c
*	   edge.c
*	   graphpscor.c
*	   graphbase.c
*	   graphcor.c
*	   graphdata.c
*	   graphpscor.c
*	   guilib.m
*	   libgui.m
*	   linetap.c
*	   linetap2.c
*	   networkedt.c
*	   pf_cb.c
*	   printopts.c
*	   pscordat.c
*	   reports.c
*	   toolbox.c
*	   vertex.c
*
\************************************************************************/


typedef long GraphDisplay;
#define GraphDisplayOff			0
#define GraphDisplayOn	 		1
#define GraphDisplayRequestOn	 	2

typedef long GraphClass;
#define GraphClassVertexBus	 	0
#define GraphClassVertexName	 	1
#define GraphClassVertexBendPoint	2
#define GraphClassVertexGenerator	3
#define GraphClassEdgeSection	 	4
#define GraphClassEdgeComplexSection	5
#define GraphClassEdgeSubSection	6
#define GraphClassVertexGroup 		7
#define GraphClassVertexDrawPoint 	8
#define GraphClassEdgeDraw	 	9
#define GraphClassPaperVertex 		10
#define GraphClassPaperEdge 		11
#define GraphClassVertexComment 	12
#define GraphClassVertexSubcomment 	13
#define GraphClassVertexOrgComment	14

#define GraphClassVertexLegendCorner 	15
#define GraphClassVertexLegendText 	16
#define GraphClassEdgeLegend 		17

#define GraphClassVertexLabelCorner	18
#define GraphClassVertexLabelText	19
#define GraphClassEdgeLabel		20
#define GraphClassVertexBorder		21
#define GraphClassEdgeBorder		22
#define GraphClassVertexFont		23
#define GraphClassEdgeBusLink		24
#define GraphClassVertexReactor		25
#define GraphClassVertexCapacitor	26

typedef long GraphType;
#define GraphTypeVertex			0
#define GraphTypeEdge			1
#define ANY_TYPE			99

#define LG_RADIUS		9
#define MED_RADIUS		5
#define SM_RADIUS		3
#define PT_RADIUS		2	/* NOTE: if changed, then re-do	    */
					/* 	 the pt_pb pixmap (main.u ) */
#define PT1_RADIUS		1
#define REACT_RADIUS		5
#define HIDDEN_BUS_RADIUS	2
#define G_DIST			36	/* bus-gen line lenght */
#define R_DIST			20	/* react-gen line lenght */
#define GEN_RADIUS		5
#define NO_HIDE			' '
#define HIDE			'1'
#define SHUNT_N			"ZZShunt     "

#define	PARTIAL_RESCALE		0
#define FULL_RESCALE		1

#define SCRN_BORDER		15

#define HOLLOW		0
#define FILLED		1

#define CIRCLE		0
#define SQUARE		1
#define BAR		2
#define RECTANGLE	3

#define BUS_UNKN	0
#define BUS_SM_CIR      1
#define BUS_MD_CIR      2
#define BUS_LR_CIR      3
#define BUS_SM_CIR_B    4
#define BUS_MD_CIR_B    5
#define BUS_LR_CIR_B    6
#define BUS_SM_SQR      7
#define BUS_MD_SQR      8
#define BUS_LR_SQR      9
#define BUS_SM_SQR_B    10
#define BUS_MD_SQR_B    11
#define BUS_LR_SQR_B    12
#define BUS_SM_BAR      13
#define BUS_MD_BAR      14
#define BUS_LR_BAR      15
#define BUS_SM_BAR_B    16
#define BUS_MD_BAR_B    17
#define BUS_LR_BAR_B    18
#define BUS_RECT_RD     19
#define BUS_RECT_SQR    20
#define BUS_DEFAULT	2


/********************** graphElement structure **************************/
/** primatives, attributes, transformations **/
typedef struct graphElement 
{
  long	 idx;		/* order that elements added	  */
  long	 display;		/* on screen (on or off)	  */
  char	 hide;			/* draw "invisible" bus icon 	  */
  int	 dply_seg;		/* segment to draw flow data	  */
  long	 type;			/* of graph node (vertex or edge) */
  long	 class;			/* for node of this type 	  */
  int	 x;
  int	 y;
  Widget wid;
  char	 vertex_id[12];		/* vertex id */
  char	 farvertex_id[12];	/* vertex id */
  char   eor[1];
} GraphElement;
typedef GraphElement * pGraphElement;

typedef struct graphLink {
 pGraphElement graphnode;
} GraphLink;

typedef GraphLink * pGraphLink;

typedef struct displayelement * DisplayElementptr;


static enum GRAPH_KEY 
{ 
  GRAPH_DFN,  

  GRAPH_DISPLAY,
  GRAPH_TYPE,
  GRAPH_VERTEX_ID,
  GRAPH_TYPE_VERTEX_ID,
  GRAPH_DISPLAY_TYPE,
  GRAPH_TYPE_VERTEX_FARVERTEX,
  GRAPH_CLASS_IDX,

  GRAPH_VERTEX_LINK,
  GRAPH_EDGE_LINK
};

extern DBID graph_db;		/* database key */
extern long graph_idx;

static char *optse = "12--56789";
static char *optsv = "12345-789";
static char *optsn = "123-6-789";

extern int creategraphtbl	( Widget w, XtPointer tag,
				  XmAnyCallbackStruct *cbs );
extern void createGraphVertex	( GraphClass class, char *namebase,
	                          int x, int y, char hide,
				  GraphElement **pgraphrec );

extern int turnOnSubedge	( GraphElement *pcmplxedge );
extern int setDashAllSubEdges	( GraphElement *pparentedge, Boolean flag );

extern DB_STAT findComplexEdge	( GraphElement **ppcmplxedge,
				  GraphElement *pcuredge);
extern DB_STAT findGraphEdgebyName( char *namebase1, char *namebase2,
	                              GraphElement **pedge );
extern DB_STAT findEdgeVertexes	( GraphElement *pedge, GraphElement **vertex_1,
				  GraphElement **vertex_2 );
extern int  findnextedge	(GraphElement *pvcur, GraphElement *pecur, 
				  GraphElement **penxt);

extern DB_STAT findBusGraphVertexRec( char *namebase, GraphElement **rec );
extern int  findBusfromNameVertex(GraphElement **pbus, GraphElement *pvtx);
extern int  findnextvertex	(GraphElement *pecur, GraphElement *pvcur, 
				  GraphElement **pvnxt);
extern void findVertxLinkedToVertx(GraphElement **pvtxname, GraphElement *pvtx,
				GraphClass class );

extern int  createBendArray	( GraphElement *bends[], GraphElement *pe,
				  GraphType type );
extern void destroyGadgets	( Widget w, XtPointer tag,
				  XmAnyCallbackStruct *cbs);
extern void mergeCoordBase	();
extern void updateGraphDisplay ( GraphElement *pge, GraphDisplay graphdisplay );
extern int  checkOn		(GraphElement *pelement, GraphElement *pparent,
				  GraphLink *plink, char *userdata);
extern void graphCorOn		(Widget w, XtPointer tag,
				  XmAnyCallbackStruct *callback_data);
extern int  printAllGraphData	(Widget widget, XtPointer tag,
				  XmAnyCallbackStruct *cbs);
extern void printGraphData	( Widget w, XtPointer tag,
				  XmAnyCallbackStruct *cbs);
extern void printPSBusRecord	( FILE *pcorfile, GraphElement *pv );
extern void printPSBranchRecord	( FILE *pcorfile, GraphElement *pe );
extern void printPSDrawRecord  	( FILE *pcorfile, GraphElement *pv );
extern int  sprintfGraphElement	( char *outstr, GraphElement *pgraphnodenxt);
extern int  printGraphElement	( GraphElement *pgraphnodenxt);
extern void printGraphCompositeChildren ();

extern void switchEdge		( GraphElement *pedge,
		                  GraphElement *poldvertex,
		                  GraphElement *pnewvertex );
extern DB_STAT farendGraphVertex ( GraphElement *vertex,
		                   GraphElement *edge,
		                   GraphElement **farvertex );
extern int changeVertexName	( GraphElement *pvtxname, GraphElement *pvtx);
extern int changeGraphBusVertex	( char *oldnamebase, char *namebase);
extern int deleteGraphElement	( GraphElement *pdelelement);
extern int deleteGraphComplexEdge( GraphElement *prmedge);
extern int deleteEdgebyName	( char *type,char *name1base1,char *name2base2);
extern int deleteBusVertex	( GraphElement *pvtx);
extern int deleteVertexbyName	( char *namebase);
extern int deleteGraphLink	( GraphElement *pe1, GraphElement *pe2);

#ifdef PROTOTYPE_PROBLEM
extern int forEachLink(IPFCONST DBID db, GraphElement *pgraphrec, IPFCONST long key,
  int (*do_fnct)(), char *userdata);
#else
extern int forEachLink(IPFCONST DBID db, GraphElement *pgraphrec, IPFCONST long key,
  int (*do_fnct)( GraphElement *pchild, GraphElement *pparent, GraphLink *plink,
  char *do_userdata),
  char *userdata);
#endif
extern int replaceGraphLink	( GraphElement *pe, GraphElement *pold,
				  GraphElement *pnew );
extern int addGraphLink		( GraphElement *pe1, GraphElement *pe2 );
extern int rmLink		( GraphElement *pchild, GraphElement *pparent,
				  GraphLink *plink, char *userdata );
extern int rmBackLink		( GraphElement *pchild, GraphElement *pparent,
				  GraphLink *plink, char *userd );
extern int turnoffGraphElement	(GraphElement *pelement );
extern int turnoffGraphComplexEdge(GraphElement *poffedge );
extern int turnoffGraphVertex	(GraphElement *pvtx );
extern int turnoffEdgebyName	( char *type, char *name1base1,
				  char *name2base2 );
extern int turnoffVertexbyName	( char *namebase );
extern int checkLineOn		( GraphElement *pgraphrec );

extern void tools_set_view_mode_cb( Widget w, int *tag,
				     XmAnyCallbackStruct *reason);
extern DB_STAT firstGE		( GraphElement **ptr, GraphDisplay gr_dpy );
extern DB_STAT nextGE		( GraphElement **ptr, int type );

