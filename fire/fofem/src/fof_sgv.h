//
// $Id$
//

float  GramSqMt_To_Pounds  (float f_Gram);


typedef   struct {
    float f_Sec;
    float f_Amt;
    float f_PM2_5;
    float f_PM10;
    float f_CH4;
    float f_CO2;
    float f_CO;
    float f_NOX;
    float f_SOX;
    float f_Inten;
    } d_SGV ;



#define e_SGV_PM2_5  "PM 2.5"
#define e_SGV_PM10   "PM 10"
#define e_SGV_CH4    "CH 4"
#define e_SGV_CO2    "CO 2"
#define e_SGV_CO     "CO"

#define e_SGV_NOX    "NOX"
#define e_SGV_SOX    "SO2"

#define e_SGV_Inten  "Intensity"

int   SGV_Init  (void);
int   SGV_GetTimPPA (int iX, float *af_Time,  float *af_Amt);
int   SGV_Set  (d_SGV  *a_SGV);
float SGV_Prepare (char cr_Name[], float *af_MaxTim, float *af_MaxAmt);
