/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fir_gen.h
* Desc: general defines that get shared by Fofem and Batch Fofem
* Date: 2/15/98
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/

#define  eC_PthFil 200                 /* length of all path/file names      */


/* Project file name used to dump a project file when burnup does a divide   */
/*  by zero exception error                                                  */
#define e_CrashSave_FN  "FofemDump.prj"


/*File Name for the input file which comes from the Log Transect program     */
#define e_FofLog_FN  "~foflog.tmp"

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                       Fofem Input Data File Names                         */
#define  e_FN_SAF   "FOF_SAF.DAT"       /* SAF Cover/Fuel                    */
#define  e_FN_NVCS  "FOF_NVCS.DAT"      /* MVCS Cover/Fuel                   */
#define  e_FN_FCC   "FOF_FCCS.DAT"      /* FCC Cover/Fuel file               */
#define  e_FN_FLM   "FOF_FLM.DAT"       /* FLM Cover/Fuel file               */

#define  e_SPP_FN   "FOF_SPP.DAT"       /* Species File                      */
#define  e_Hlp_FN   "fof_help.dat"      /* Help text file                    */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                         Soil File Names/extensions                        */

#define  e_DSI_FNin  "~dufsim.tmp"      /* temp input for Duff Sim           */
#define  e_EID_FNin  "~expheat.tmp"     /* temp input for Exp Heat           */

#define  e_SPt_FN    "~Soilpts.tmp"     /* Duff & Exp sims can output this   */

#define  e_SoiPtFN   "SoilPts"          /* Default File Name & Ext used when */
#define  e_SoiPtExt  "txt"              /* User Saves the Points File        */

/* NOTE - make sure you keep the file extensions the same....................*/
/* as those use in the Sample File Names                                     */
#define  e_SoilDufExt "duf"             /* Ext user input file soil Duff Sim */
#define  e_SoilExpExt "exp"             /* Ext user input file soil Exp Heat */
#define e_Sample_Duff "Sample.duf"      /* names user to create sample files */
#define e_Sample_Exp  "Sample.exp"

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                        Burnup File Names/Extnesions                       */
#define  e_BurTmpIn_FN   "~brnup.tmp"   /* temp input file for burnup        */
#define  e_BrnAmtExt     "Amt"          /* Amount & Time/Heat file ext.      */
#define  e_BrnHeaExt     "hea"          /*  out file when using user in file */
#define  e_BrnExt        "brn"          /* User Option Input file ext        */

#define  e_BrnTmpAmt     "~brn.amt"     /* 'outfile' in  'BurnUp::Summary'   */
#define  e_BrnTmpHea     "~brn.hea"     /* 'histfile' in 'BurnUp::Stash'     */


#define  e_BrnEmi        "Emission"     /* def fil nam for saving Emissn File*/
#define  e_BrnEmiExt     "txt"          /* the fil nam extension             */


/* Used to dump Mortaliyt user species out and read back in to format in Wnd */
#define  e_TmpMor        "~mor.tmp"



/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
#define    e_PrjExt      "prj"          /* Project file extension            */
#define    e_RptExt      "rpt"          /* report  file extension            */

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*                    Graph Enhanced Meta File Names/Extension               */
#define  e_GraSoilFN "~Soilmf.tmp"      /* temp files created when drawing   */
#define  e_GraMortFN "~Mortmf.tmp"      /* graphs                            */
#define  e_GraFuelFN "~Fuelmf.tmp"
#define  e_GraSmokFN "~Smokmf.tmp"
#define  e_EMFExt    "emf"              /* ext when user saves graph meta fil*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* This file name is used with the t_DeleteFile function                     */
/* see the comments in the function                                          */
#define  e_TestFN    "~fofem"

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*                        Sample Batch Input File Names                      */
/* These are used when use does the menu option to create sample input       */
/*  files that can be used as with the Batcth Input option                   */
#define e_SIB_Soil  "Soil-In.txt"
#define e_SIB_ConE  "ConE-In.txt"
#define e_SIB_Mort  "Mort-In.txt"

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */

int copyfile (char cr_From[], char cr_To[]);

void t_DeleteFile (char cr_FN[]);

int   Error_Window (char cr_Mess[], char cr_Function[]);
int   Warning_Window (char cr_Mess[], char cr_Function[]);
int   Info_Window (char cr_Mess[],char cr_Function[]);
void  MM_Clear (void);
int   MM_isMess (void);
void  GetDate (char cr_Date[]);
void  xstrupr (char cr[]);

int  Win_CrashSave (void);
