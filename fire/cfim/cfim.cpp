// cfim.cpp : Defines the entry point for the console application.
//----------
//  $Id$
//----------
// Cruz, M.G.; Butler, B.W.; Alexander, M.E.; Viegas, D.X. 2006.
// Development and evaluation of a semi-physical crown fire initiation model.
// In: D.X. Viegas, editor. Proceedings of the 5th International Conference on
// Forest Fire Research, November 27-30, 2006, Figueira da Foz, Portugal.
// Elsevier, Amsterdam, The Netherlands. 17 p.
//

#include "stdafx.h"

//#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <conio.h>
#include <sys/timeb.h>


#define NRANSI

#include "nr.h"
#include "nrutilcpp.h"

#define LENGTH 256
#define SUCCESS 0
#define FAIL 1
#define PI 3.14159265358979324
#define SB 0.00000005669
#define Rhoa 1.177          //kg/m^3
#define Alpha 0.16          //coefficient used in plume model
#define Beta 0.5            //coefficient used in plume model
#define g 9.81              //acceleration of gravity
#define Cpdry 2100.0        //canopy fuel specific heat (J/kg/K)
#define Cpwater 4187.0      //water specific heat (J/kg/K)
#define L 2254000.0         //water latent heat of vaporization
#define EPS 3.0e-11         //parameter in Gaussian quadrature

#define NVAR 6              //number of differential equations solved in plume model
#define NSTEP 10000           //number of steps when solving differential equations (s-points)

#include "rkdumb.h"


typedef struct input {
    double u10;double slope;double Ta;double sh;double alpha;double igtemp;double tstep;long iters;double xstart;
    long FuelModelNumber;double FuelMoisture[5];double rho_surf;double sigma_surf;double hc;
    double sigma_can;double canbaseht;double diameter;double FMC;double length;double rho_can;

}Input;
Input in;

extern "C" { void CFIM_DRIVER(float CFIM_Input[], float CFIM_output[], float fminfo[]); }
//extern "C" { void CFIM_DRIVER(float CFIM_In, float CFIM_out); }
//void CFIM_DRIVER(float CFIM_In, float CFIM_out);
double windprofile(Input IN,double Z);
double maxflametemp(double Us,Input IN);
double reaction_time(double R,double Ua,double beta,double gamma,Input IN);
double behave(double MidflameWindspeed,Input IN,double *FirelineIntensity,double *FlameLength, double *HeatPerUnitArea);
double pow2(double base);
void SetStandardFuelModel(long number);
void SetFuelMoistures(double ones, double tens, double hundreds, double liveh, double livew);
double CalcSpreadRate(double *Fuel, double *Moisture, double Slope,
                       double WindSpeed, double *FlameLength,
                       double *FirelineIntensity, double *HeatPerUnitArea);
void derivs(double x,double y[],double dydx[]);
void locate(double x2[],unsigned long n,double x,long *j);
double linterp(double X[],double Y[],long n,double xi);
double ptinterp(double x1,double x2,double y1,double y2,double xi);
void fillconvect0(void);
void fillconvect1(void);
double gettemp(double x);
double getspeed(double x);
int plumemodel(void);
void calc_xplumert(void);
void calc_xplumelt(void);
double fxn(double pt[],double wgt);
double getrad(double Dx,double Tp);
double radflametemp(double X);
double getconvectflux(double Dx,double Tp);
void gauleg(double x1, double x2, long n);
double qgaus(double (*func)(double,double,double), double a, double b,double Dx,double Tp);
double xx2(double y);
double xx1(double y);
double f2(double x,double Dx,double Tp);
double f1(double y,double Dx,double Tp);
double integration_function(double x,double y,double Dx,double Tp);

//FILE *fcustomin;
FILE *fout;
static int bPrintOut = 1;

static double FuelModel[13];
static double fuelmoisture[5];
static double rhoa = 1.177;         //  kg/m^3
static double gamma = 0.15;         //  fuels unavailable for combustion
static double combust_efficiency= 0.85;     //fuels available for flaming combustion in surface fire
static double ROS;
static double avail_surf_fuel;
static double iByram;               //  byram's intensity
static double beta_surf;            //  packing ratio for surface fuel bed
//char custominfile[LENGTH];
double FirelineIntensity, FlameLength, HeatPerUnitArea;
static double taur;                 //  residence time
static double flamedepth;
static double flameheight;
static double Ti=800.0;             //initial temperature of the plume(plume model)
static double Cp=1.05;              //specific heat of air(plume model)
static double ucantop,umid,uz,maxflmtemp;
static double Up;

extern double **y,*xx;   /* referencing declaration */
static double **finalplume,**rawplume;  //declare matrices to store plume info
static double s_canopyht,s_rtcanopyht,s_ltcanopyht;      //plume model variables
static double xplumeright,xplumeleft,xplumemid;     //x value of the respective place in the plume at the canopy height (global coord. sys.)
static double **convect,**convect2;            //matrix of x, air temp, velocity in the plume at the canopy height
static long j;
static double zinternrt1,zinternrt2,zinternlt1,zinternlt2,zerror1,zerror2;
static double xinternrt,xinternlt;
static double x1=0.0, x2=4.0, px2;
static double NPTS;            //number of values used in convect array
static double radenergy,convectenergy;
static double part_vol,part_surf_area,C,xpos,Tp,Qtotal,sink;
static double maxparttemp=0.0;

long idum;      /* for ranno */
int ndim=4;       /* for fxn */
double pemiss=1.0,femiss=1.0;       //emissivity of the particle and flame
double attenuation=0.02;\
double Lp=0.001,Wp=0.001,Wf=20.0;   //length and width of particle, width of flame

static int radflag=0;       //if 0, quadruple integral Monte Carlo used;if 1, double integral Gaussian Quadrature used;
static double Y1=-Wf/2.0, Y2=Wf/2.0;    //limits of integration for Gaussian radiation model;
static long n=21;       //parameter for Gaussian quadrature integration (controls how many "iterations" happen)
static double ysav,x[1408],w[1408];     //used in Gaussian quadrature; length of vectors must be n+1

static int convectflag=0;   //if 0, gaussian profile of plume follows the "c" line;if 1, gaussian profile follow canbaseht line;

//TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!
 //double Tp=300.0;    //temp of particle
//double Dx=1.0;      //distance from flm leading edge to particle center
//TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!TEMPORARY!!!

int _tmain(int argc, _TCHAR* argv[])
{
	int retcode;
	float cfim_in[24];
	float cfim_out[10];
	float fminfo[13];
	CFIM_DRIVER(cfim_in, cfim_out, fminfo);
    return(0);
}
void CFIM_DRIVER(float CFIM_Input[], float CFIM_output[], float fminfo[])
{
	long i;
    long printed_iters=5;
    int flag=0;
	float ukm, ROSmin;
	double surfuel;

	//INSTEAD OF READING INPUT FROM A FILE, IT HAS BEEN PASSED IN AS AN ARRAY
    in.u10 = CFIM_Input[0];
	in.slope = CFIM_Input[1];
	in.Ta = CFIM_Input[2];
	in.sh = CFIM_Input[3];
	in.alpha = CFIM_Input[4];
	in.igtemp = CFIM_Input[5];
	in.tstep = CFIM_Input[6];
	in.iters = CFIM_Input[7];
	in.xstart = CFIM_Input[8];
    in.FuelModelNumber = CFIM_Input[9];
	in.FuelMoisture[0] = CFIM_Input[10];
	in.FuelMoisture[1] = CFIM_Input[11];
	in.FuelMoisture[2] = CFIM_Input[12];
	in.FuelMoisture[3] = CFIM_Input[13];
	in.FuelMoisture[4] = CFIM_Input[14];
	in.sigma_surf = CFIM_Input[15];
	in.rho_surf = CFIM_Input[16];
	in.hc = CFIM_Input[17];
    in.sigma_can = CFIM_Input[18];
	in.canbaseht = CFIM_Input[19];
	in.diameter = CFIM_Input[20];
	in.FMC = CFIM_Input[21];
	in.length = CFIM_Input[22];
	in.rho_can = CFIM_Input[23];
	surfuel = CFIM_Input[24];

	//set the FM input variables into the CFIM variable FuelModel
	for(i=0;i<=12;i++)        
		FuelModel[i] = fminfo[i];

	if (bPrintOut == 1)
	{
		if((fout=fopen("CFIMdebug.txt","w+")) == NULL)
			bPrintOut = 0;
		else
			bPrintOut = 2;
	}
	//now print the inputs to the model
	if (bPrintOut > 1)
	{
		if (bPrintOut == 2)
		{
			//print headers
			fprintf(fout,"wind\t slope\t temp\t StandHeight\t" );
			fprintf(fout,"Moisture\t Moisture\t Moisture\t Moisture\t Moisture\t" );
			fprintf(fout,"CanBaseHt\t FMC\t SurfFuel\t" );
			fprintf(fout,"Tp\t flag\t Byram\t taur\t ROS\t");
			fprintf(fout,"flamedepth\t flameheight\t FlameLength\n");
			bPrintOut++;
		}
			
			//fprintf(fout,"%lf\t%lf\t%lf\t%lf\t%lf\n",(double)(i-1)*in.tstep,Tp,Qtotal,convectenergy,radenergy);
		fprintf(fout,"%lf\t%lf\t%lf\t%lf\t",in.u10, in.slope, in.Ta, in.sh);
		fprintf(fout,"%lf\t%lf\t%lf\t%lf\t%lf\t",in.FuelMoisture[0],in.FuelMoisture[1],in.FuelMoisture[2],in.FuelMoisture[3],in.FuelMoisture[4]);
		fprintf(fout,"%lf\t%lf\t%lf\t",in.canbaseht, in.FMC, surfuel);

		in.u10 = CFIM_Input[0];
		in.slope = CFIM_Input[1];
		in.Ta = CFIM_Input[2];
		in.sh = CFIM_Input[3];
		in.alpha = CFIM_Input[4];
		in.igtemp = CFIM_Input[5];
		in.tstep = CFIM_Input[6];
		in.iters = CFIM_Input[7];
		in.xstart = CFIM_Input[8];
		in.FuelModelNumber = CFIM_Input[9];
		in.FuelMoisture[0] = CFIM_Input[10];
		in.FuelMoisture[1] = CFIM_Input[11];
		in.FuelMoisture[2] = CFIM_Input[12];
		in.FuelMoisture[3] = CFIM_Input[13];
		in.FuelMoisture[4] = CFIM_Input[14];
		in.sigma_surf = CFIM_Input[15];
		in.rho_surf = CFIM_Input[16];
		in.hc = CFIM_Input[17];
		in.sigma_can = CFIM_Input[18];
		in.canbaseht = CFIM_Input[19];
		in.diameter = CFIM_Input[20];
		in.FMC = CFIM_Input[21];
		in.length = CFIM_Input[22];
		in.rho_can = CFIM_Input[23];
		surfuel = CFIM_Input[24];

	}
	
    ucantop=windprofile(in,in.sh);          //windspeed at the canopy top
    umid=windprofile(in,0.1*in.sh);         //surface fire midflame windspeed
	ukm = in.u10 * 60. * 60. / 1000.;		//change from m/s to km/h

    ROS=behave(umid,in,&FirelineIntensity,&FlameLength,&HeatPerUnitArea);	//original CFIM ROS equation
	ROSmin = 0.4666 + 0.0703*ukm + .0044*ukm*ukm;		//ROS in m/min (new equation)		
	ROS = ROSmin / 60.0;								    //ROS in m/sec  NOTE: we are replacing the BEHAVE ROS with a new ROS
	FirelineIntensity = 300 * surfuel * ROSmin;				//SFI = 300*SFC*ROS NOTE:we are replacing the BEHAVE FirelineIntensity with a new FireLineIntensity
															//BUT NOTE: this produces a value that is 1000x higher than the one from BEHAVE but there is no
															//impact because the fireline intensity is never used.
    avail_surf_fuel=0.2242*FuelModel[0];        //0.2242 converts to correct units
    maxflmtemp=maxflametemp(umid,in);
    FirelineIntensity*=3461.4693327428;             //convert to W/m
    FlameLength*=0.3048;                            //convert to m
    HeatPerUnitArea*=11356.526682227;               //convert to j/m^2
    iByram=combust_efficiency*ROS*avail_surf_fuel*in.hc;
	FlameLength = 0.0775 * pow(iByram, 0.46);
    beta_surf=avail_surf_fuel/(FuelModel[11]*in.rho_surf*(1.0-gamma));  //packing ratio for surface fuel bed
    taur=reaction_time(ROS,umid,beta_surf,gamma,in);					//original CFIM equation
	taur = 0.39 * pow(surfuel,.25) * pow(ucantop,1.51) / ROS;			//new equation (overwrites origina)
    flamedepth=ROS*taur;
    flameheight=iByram/(385.0*umid);
    part_vol=PI*pow2(in.diameter)/4.0*in.length;
    part_surf_area=PI*in.diameter*in.length;
    Up=pow(((2.0*g*iByram)/(rhoa*Cp*(in.Ta))),(1.0/3.0));

    flag = plumemodel();

	if (flag <= 0)
	{
		xpos=in.xstart-ROS*in.tstep;   //initialize x position to one step before xstart
		Tp=in.Ta;                   //initialize the fuel particle temperature to ambient temperature
		C=((Cpdry+in.FMC*Cpwater)*(373.0-in.Ta)+(in.FMC*L)+Cpdry*(in.igtemp-373.0))/(in.igtemp-in.Ta);    //"effective" Cp of moist fuel

		for(i=1;i<=in.iters;i++)        //solving for particle temperature
		{
			xpos=xpos-ROS*in.tstep;     //position of the particle from the fire leading edge coordinate system
			convectenergy=getconvectflux(xpos,Tp)*part_surf_area;    //get the convective energy (W)
			radenergy=getrad(xpos,Tp);            //get the radiative energy (W)
			Qtotal=in.tstep*(radenergy+convectenergy);    //in Joules
			sink=in.rho_can*part_vol*C;             //sink is rho*V*C
			Tp=Tp+Qtotal/sink;                      //calculate the new temperature of the particle
			if(Tp>maxparttemp)
				maxparttemp=Tp;
			if(Tp>=in.igtemp)
				flag=1;
		}
		free_matrix(convect,1,3,1,NPTS+1);
	}

	CFIM_output[0] = maxparttemp;
	CFIM_output[1] = flag;
	CFIM_output[2] = iByram;
	CFIM_output[3] = taur;
	CFIM_output[4] = ROS;
	CFIM_output[5] = flamedepth;
	CFIM_output[6] = flameheight;
	CFIM_output[7] = FlameLength;


	if (bPrintOut > 1)
	{
		fprintf(fout,"%lf\t%hd\t%lf\t%lf\t%lf\t",Tp, flag, iByram, taur, ROS);
		fprintf(fout,"%lf\t%lf\t%lf\n",flamedepth, flameheight, FlameLength);
	}

//    if(flag<=0)
//    {
//        printf("\n\n****The canopy DID NOT ignite.****\n");
//    }else{
//        printf("\n\n****The canopy DID ignite.****\n");
//    }

    free_matrix(rawplume,1,6,1,NSTEP+1);
    free_vector(xx,1,NSTEP+1);

	return;
}

double windprofile(Input IN,double Z)
    {
     double u,ush;

     ush=IN.u10*((log((IN.sh-0.64*IN.sh)/(0.13*IN.sh)))/(log(((10.0+IN.sh)-0.64*IN.sh)/(0.13*IN.sh))));
     if(Z>=0.6*IN.sh)
        {
         u=ush*exp(IN.alpha*(Z/IN.sh-1.0));
        }
     else
        {
         u=ush*exp(IN.alpha*(IN.sh*0.6/IN.sh-1.0));
        }

     return u;
    }

double maxflametemp(double Us,Input IN)
    {
     double beta1=300.684,beta2=136.791,beta3=0.506,beta4=100.448,beta5=-0.531;
     double maxtemp;

     maxtemp=300.0+beta1*avail_surf_fuel+beta2*pow(Us,beta3)+beta4*pow(IN.FuelMoisture[0],beta5);
     return maxtemp;
    }

double getleadingxpos(double Time,double Ros,double Startx)
{
    double Xpos=Startx+Ros*Time;
    return (Xpos);
}


double reaction_time(double R,double Ua,double BETA,double gamma,Input IN)
    {
     double Zini=0.0,taufbini=75571.0*BETA*FuelModel[11],taufbold=300.0,epsilon=0.0035,rhoa=1.2,Tab=IN.Ta-273.0,Cp=1.05,Tx=500.0,Hiprime=504.0,
            deltaHc=31200.0,SBolt=5.67*pow(10.0,-11.0),Ts=400,kc=6.63*pow(10.0,-5.0),Qf=711,Qm=2570,nu=1.13*pow(10.0,-4.0),
            taufb=taufbini,Z=Zini,IBreacT=0.85*IN.hc*(1.0-gamma)*IN.rho_surf*BETA*FuelModel[11]*R,theta=IN.sigma_surf*BETA*FuelModel[11],F=0.283+0.178*log(theta),
            HI=(Hiprime+2570.0*IN.FuelMoisture[0])/F,Hp=207.0,HR=175.0*(1.0-gamma),HS=836.0*IN.FuelMoisture[0],deltaHv=((1.0-epsilon)*IN.hc-(gamma-epsilon)*deltaHc)/(1.0-gamma),
            deltaHvhigh=deltaHv+1580.0,Nv=deltaHvhigh/3270.0,HN,X1,HA1,Tm,Uv,A,rhom,V,Z1,hr,hc,hef,taufb1,taur;
//    double Hd=772.0,Cps=2.09;
    do
        {
        taufbold=taufb;
        Nv=deltaHvhigh/3270.0;
        HN=HI+Hp+HR+HS;
        X1=(HN+1045.0*IN.FuelMoisture[0]+1.05*(700.0-25.0)*Z)/((1.0-gamma)*(deltaHv-525.0-1024.0*Nv));
//        HA1=X1*(1.0-gamma)*(deltaHv-525.0-1024.0*Nv)-1045.0*IN.FuelMoisture[0]-709.0*Z;
        Tm=500.0+(500.0*(2.09*IN.FuelMoisture[0]+1.05*X1*(1.0-gamma)*(1.0+Nv)))/(2.09*IN.FuelMoisture[0]+1.05*Z+1.05*(1.0-gamma)*(1.0+X1*Nv));
        Uv=pow(((2.0*g*IBreacT)/(rhoa*Cp*(Tab+273.0))),(1.0/3.0));
        A=atan(Ua/Uv);
        rhom=rhoa*((Tab+273.0)/(Tm+273.0));
        V=(HN*BETA*(1.0-gamma)*FuelModel[11]*IN.rho_surf*(1.0/(cos(A)))/(taufb*rhom*Cp*(Tm-Tx)));
        Z1=((taufb*cos(A)*rhom*V)/avail_surf_fuel)-IN.FuelMoisture[0]-(1.0+X1*Nv);
        hr=0.5*SBolt*((Tm+273.0)+(Ts+273.0))*(pow((Tm+273.0),2.0)+pow((Ts+273.0),2.0));
        hc=0.344*((IN.sigma_surf*kc)/4.0)*pow(((4.0*V)/(IN.sigma_surf*nu)),0.56);
        hef=hr+hc;
//        taup=2.0*(1.0-gamma)*IN.rho_surf*BETA*IN.sigma_surf*(Qf+Qm*IN.FuelMoisture[0])/(hef*(Tm-Ts)*(1.0-BETA));
        taufb1=2.0*(1.0-gamma)*IN.rho_surf*BETA*FuelModel[11]*(Qf+Qm*IN.FuelMoisture[0])/(hef*(Tm-Ts)*(1.0-BETA));
        Z=Z1;
        taufb=taufb1;
//        printf("\n%lf %lf",A,taufb);
        }
     while(fabs(taufbold-taufb)>0.0001);
     taur=taufb;
    return taur;
    }


///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
double behave(double MidflameWindspeed,Input IN,double *FirelineIntensity,double *FlameLength, double *HeatPerUnitArea)
    {
     double SpreadRate;
     long fuelmodelnumber=IN.FuelModelNumber;
     double slope=IN.slope;


     int i;
     for (i=0;i<5;i++)
     {
        fuelmoisture[i]=IN.FuelMoisture[i];
     }

     MidflameWindspeed*=2.23694;                    //convert to mph
//     MidflameWindspeed=4.0;                       // miles per hour
//     Slope=30.0;                                  // percent

     // set fuel model parameters into FuelModel array to pass to spread func
	 //don't need to do this anymore because it is passed in
//     SetStandardFuelModel(fuelmodelnumber);

     // moistures for comparison of BEHAVE outputs
     //SetFuelMoistures(FuelMoisture[0], FuelMoisture[1], FuelMoisture[2], FuelMoisture[3], FuelMoisture[4]);        // dry, Andrews 1986 p19
     //SetFuelMoistures(0.12, 0.13, 0.14, 1.70, 1.70);      // moderate, Andrews 1986 p19

     // Call Spread function
     SpreadRate=CalcSpreadRate(FuelModel, fuelmoisture, MidflameWindspeed, slope,
                    FlameLength, FirelineIntensity, HeatPerUnitArea);

     SpreadRate=SpreadRate*0.00508;         // convert to m/s
//     printf("%lf %lf %lf %lf\n", SpreadRate, FirelineIntensity, FlameLength, HeatPerUnitArea);
     return SpreadRate;
    }


double pow2(double base)
{
     return base*base;
}


void SetStandardFuelModel(long number)
{
    char junk[LENGTH];
    if(number>=14)
    {
        /*sprintf(custominfile,"custom_%ld.txt",number);
        if((fcustomin = fopen (custominfile, "r")) == NULL)
        {
            printf("A custom fuel model input file cannot be found.  Exiting program...\n");
            exit(0);
        }
        fscanf(fcustomin,"%s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s %lf %s",&junk,&FuelModel[0],&junk,&FuelModel[1],&junk,&FuelModel[2],&junk,&FuelModel[3],&junk,&FuelModel[4],&junk,&FuelModel[5],&junk,&FuelModel[6],&junk,&FuelModel[7],&junk,&FuelModel[8],&junk,&FuelModel[9],&junk,&FuelModel[10],&junk,&FuelModel[11],&junk,&FuelModel[12],&junk);
		*/
    }else{
        FuelModel[3]=0.0;                                 // loading for live herb
        FuelModel[6]=1800.0;                              // surf for live herb
        FuelModel[8]=FuelModel[9]=FuelModel[10]=8000.0;   // heat contents all the same
    	switch(number)
    	{	case 1: FuelModel[0]=0.74; FuelModel[1]=0.0; FuelModel[2]=0.0; FuelModel[4]=0.0;FuelModel[5]=3500;FuelModel[7]=1500;FuelModel[11]=1.0;FuelModel[12]=.12;break;
    		case 2: FuelModel[0]=2.0; FuelModel[1]=1.0; FuelModel[2]=0.5; FuelModel[4]=.5;FuelModel[5]=3000.0;FuelModel[7]=1500.0;FuelModel[11]=1.0;FuelModel[12]=.15;break;
    		case 3: FuelModel[0]=3.01; FuelModel[1]=0.0; FuelModel[2]=0.0; FuelModel[4]=0.0;FuelModel[5]=1500.0;FuelModel[7]=1500.0;FuelModel[11]=2.5;FuelModel[12]=.25;break;
    		case 4: FuelModel[0]=5.01; FuelModel[1]=4.01; FuelModel[2]=2.0; FuelModel[4]=5.01;FuelModel[5]=2000.0;FuelModel[7]=1500.0;FuelModel[11]=6.0;FuelModel[12]=.20;break;
    		case 5: FuelModel[0]=1.0; FuelModel[1]=0.5; FuelModel[2]=0.0; FuelModel[4]=2.0;FuelModel[5]=2000.0;FuelModel[7]=1500.0;FuelModel[11]=2.0;FuelModel[12]=.20;break;
    		case 6: FuelModel[0]=1.5; FuelModel[1]=2.5; FuelModel[2]=2.0; FuelModel[4]=0.0;FuelModel[5]=1750.0;FuelModel[7]=1500.0;FuelModel[11]=2.5;FuelModel[12]=.25;break;
    		case 7: FuelModel[0]=1.13; FuelModel[1]=1.87; FuelModel[2]=1.5; FuelModel[4]=0.37;FuelModel[7]=1550.0;FuelModel[5]=1750.0;FuelModel[11]=2.5;FuelModel[12]=.40;break;
    		case 8: FuelModel[0]=1.5; FuelModel[1]=1.0; FuelModel[2]=2.5; FuelModel[4]=0.0;FuelModel[5]=2000.0;FuelModel[7]=1500.0;FuelModel[11]=0.2;FuelModel[12]=.30;break;
    		case 9: FuelModel[0]=2.92; FuelModel[1]=0.41; FuelModel[2]=0.15; FuelModel[4]=0.0;FuelModel[5]=2500.0;FuelModel[7]=1500.0;FuelModel[11]=0.2;FuelModel[12]=.25;break;
    		case 10: FuelModel[0]=3.01; FuelModel[1]=2.0; FuelModel[2]=5.01; FuelModel[4]=2.0;FuelModel[5]=2000.0;FuelModel[7]=1500.0;FuelModel[11]=1.0;FuelModel[12]=.25;break;
    		case 11: FuelModel[0]=1.5; FuelModel[1]=4.51; FuelModel[2]=5.51; FuelModel[4]=0.0;FuelModel[5]=1500.0;FuelModel[7]=1500.0;FuelModel[11]=1.0;FuelModel[12]=.15;break;
    		case 12: FuelModel[0]=4.01; FuelModel[1]=14.03; FuelModel[2]=16.53; FuelModel[4]=0.0;FuelModel[5]=1500.0;FuelModel[7]=1500.0;FuelModel[11]=2.3;FuelModel[12]=.20;break;
    		case 13: FuelModel[0]=7.01; FuelModel[1]=23.04; FuelModel[2]=28.05; FuelModel[4]=0.0;FuelModel[5]=1500.0;FuelModel[7]=1500.0;FuelModel[11]=3.0;FuelModel[12]=.25;break;
 	    }
    }
    if(fuelmoisture[0]>=FuelModel[12])
    {
//        printf("\n\nSurface fire will not spread because you have \nreached the moisture of extinction.\nExiting...\n");
        exit(0);
    }

     // constants for 13 standard models
}


double CalcSpreadRate(double *Fuel, double *Moisture, double WindSpeed,
                       double Slope, double *FlameLength,
                       double *FirelineIntensity, double *HeatPerUnitArea)
{// Rothermel spread equation based directly on BEHAVE source code
	long i, j, ndead=0, nlive=0;
     double seff[3][2]={{.01,.01},{.01,.01},{.01,0}};	     //mineral content
	double wtfact, fined=0, finel=0, wmfd=0, fdmois=0, w=0, wo=0, beta;
	double rm, sigma=0, rhob=0, sum3=0, betaop=0, rat, aa, gammax=0, gamma=0, wind=0;
	double xir, rbqig=0, xi=0, b, c, e, part1=0, slopex=0;
	double ewind, wlim, sum1=0, sum2=0, phis, phiw, phiew;
     double rateo, SpreadRate;



    double mois[3][2]=		               // fraction of oven-dry weight
	{	{Moisture[0], Moisture[3]},
		{Moisture[1], Moisture[4]},
		{Moisture[2], 0.0},
	};

	if(Fuel[0]) ndead++;
	if(Fuel[1]) ndead++;
	if(Fuel[2]) ndead++;
	if(Fuel[3]) nlive++;
	if(Fuel[4]) nlive++;

     if(nlive>0)
          nlive=2;                 // boost to max number
     if(ndead>0)
          ndead=3;

	double nclas[2]={ndead,nlive};  // # of dead & live fuel classes

    	double load[3][2]=			// tons per acre, later converted to lb/ft2
	{	{Fuel[0], Fuel[3]},
		{Fuel[1], Fuel[4]},
		{Fuel[2], 0.0},
	};

	double sav[3][2]=			 // 1/ft
	{	{Fuel[5], Fuel[6]},
		{109.0, Fuel[7]},
		{30.0, 0.0},
	};

	double heat[3][2]=
     {    {Fuel[8], Fuel[9]},
          {Fuel[8], Fuel[10]},
          {Fuel[8], 0.0},
     };

	double depth=Fuel[11];

	double wn[3][2]={{0,0},{0,0},{0,0}};
	double qig[3][2]={{0,0},{0,0},{0,0}};
	double a[3][2]={{0,0},{0,0},{0,0}};
	double f[3][2]={{0,0},{0,0},{0,0}};
	// double g[3][2]={0,0,0,0,0,0,};
	double ai[2]={0,0};
	double fi[2]={0,0};
	double hi[2]={0,0};
	double se[2]={0,0};
	double xmf[2]={0,0};
	double si[2]={0,0};
	double wni[2]={0,0};
	double etam[2]={0,0};
	double etas[2]={0,0};
	double rir[2]={0,0};
	double xmext[2]={Fuel[12], 0};

	wind=WindSpeed*88.0;                    // ft/minute
	slopex=Slope/100.0; //tan((double) Slope/180.0*PI);  	// convert from degrees to tan

	// fuel weighting factors
	for(i=0; i<2; i++)
	{	for(j=0; j<nclas[i]; j++)
		{    a[j][i]=load[j][i]*sav[j][i]/32.0;
			ai[i]=ai[i]+a[j][i];
			wo=wo+0.04591*load[j][i];
		}
		if(nclas[i]!=0)
		{	for(j=0;j<nclas[i];j++)
			{    if(ai[i]>0.0)
                    	f[j][i]=a[j][i]/ai[i];
                    else
                         f[j][i]=0.0;
               }
		}
	}
	fi[0]=ai[0]/(ai[0]+ai[1]);
	fi[1]=1.0-fi[0];

	/* no need for this, because extinction moistures are assigned */
	/* as on last page of Burgan and Rothermel 1984 */
	/*	rhob=(wo/depth);
		beta=rhob/32;
		xmext[0]=.12+4.*beta;
	*/

	//moisture of extinction
	if(nclas[1]!=0)
	{	for(j=0; j<nclas[0]; j++)
		{    wtfact=load[j][0]*exp(-138.0/sav[j][0]);
			fined=fined+wtfact;
			wmfd=wmfd+wtfact*mois[j][0];
		}
		fdmois=wmfd/fined;
		for(j=0; j<nclas[1]; j++)
			finel=finel+load[j][1]*exp(-500.0/sav[j][1]);
		w=fined/finel;
		xmext[1]=2.9*w*(1.0-fdmois/xmext[0])-0.226;
		if(xmext[1]<xmext[0])
			xmext[1]=xmext[0];
	}

	// intermediate calculations, summing parameters by fuel component
	for(i=0;i<=1;i++)
	{	if(nclas[i]!=0)
		{	for(j=0;j<nclas[i];j++)
			{	wn[j][i]=0.04591*load[j][i]*(1-0.0555);
				qig[j][i]=250.0+1116.0*mois[j][i];
				hi[i]=hi[i]+f[j][i]*heat[j][i];
				se[i]=se[i]+f[j][i]*seff[j][i];
				xmf[i]=xmf[i]+f[j][i]*mois[j][i];
				si[i]=si[i]+f[j][i]*sav[j][i];
				sum1=sum1+0.04591*load[j][i];
				sum2=sum2+0.04591*load[j][i]/32.0;
				sum3=sum3+fi[i]*f[j][i]*qig[j][i]*exp(-138.0/sav[j][i]);
			}
			for(j=0; j<nclas[i]; j++)
				wni[i]=wni[i]+f[j][i]*wn[j][i];  /* g[j][i] should be subst for f[j][i] in the wni[i] equation */
										   /* if the above g-factors are calculated */
			rm=xmf[i]/xmext[i];
			etam[i]=1.0-2.59*rm+5.11*pow2(rm)-3.52*pow(rm,3.0);
			if(xmf[i] >= xmext[i])
				etam[i]=0;
			etas[i]=0.174/(pow(se[i],0.19));
			if(etas[i]>1.0)
				etas[i]=1.0;
			sigma=sigma+fi[i]*si[i];
			rir[i]=wni[i]*hi[i]*etas[i]*etam[i];
		}
	}

	/* final calculations */
	rhob=sum1/depth;
	beta=sum2/depth;
	betaop=3.348/pow(sigma,0.8189);
	rat=beta/betaop;
	aa=133.0/pow(sigma,0.7913);
	gammax=pow(sigma,1.5)/(495.0+0.0594*pow(sigma,1.5));
	gamma=gammax*pow(rat,aa)*exp(aa*(1.0-rat));
	xir=gamma*(rir[0]+rir[1]);
	rbqig=rhob*sum3;
	xi=exp((0.792+0.681*pow(sigma,0.5))*(beta+0.1))/(192.0+0.2595*sigma);
/*	flux=xi*xir;*/
	rateo=xir*xi/rbqig;   /* this is in English units */

	phis=5.275*pow(beta,-0.3)*pow2(slopex);
		c=7.47*exp(-0.133*pow(sigma,0.55));
		b=0.02526*pow(sigma,0.54);
		e=0.715*exp(-0.000359*sigma);
		part1=c*pow(rat,-e);
	phiw=pow(wind,b)*part1;

	wlim=0.9*xir;

     SpreadRate=(rateo*(1.0+phiw+phis));                          // ft/min
     *FirelineIntensity=384.0*xir*SpreadRate/(60.0*sigma);        // btu/ft/sec
	*FlameLength=0.45*pow(*FirelineIntensity,0.46);              // ft
	*HeatPerUnitArea=xir*384.0/sigma;                            // btu/ft2




/*   maximum windspeed effect on ros*/
	phiew=phiw+phis;
	ewind=pow(((phiew*pow(rat,e))/c),1.0/b);

	if(ewind>wlim)
	{	ewind=wlim;
		phiew=c*pow(wlim,b)*pow(rat,-e);
		SpreadRate=rateo*(phiew+1.0);
		*FirelineIntensity=384.0*xir*SpreadRate/(60.0*sigma);
		*FlameLength=0.45*pow(*FirelineIntensity, 0.46);
	}
//	react=xir*1.633;  				// convert btu/f2/s to kW/m2

     return SpreadRate;
}
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////

void derivs(double x,double y[],double dydx[])
{
	dydx[1] = Rhoa*(Alpha*(y[2]/y[1]-(windprofile(in,y[6]+flameheight))*cos(y[3]))+Beta*(windprofile(in,y[6]+flameheight))*sin(y[3]));
	dydx[2]=Rhoa*(Alpha*(y[2]/y[1]-(windprofile(in,y[6]+flameheight))*cos(y[3]))+Beta*(windprofile(in,y[6]+flameheight))*sin(y[3]))*(windprofile(in,y[6]+flameheight))*cos(y[3])+((y[1]*y[4]*g*sin(y[3]))/(y[2]*in.Ta));
	dydx[3]=-(Rhoa*(Alpha*(y[2]/y[1]-(windprofile(in,y[6]+flameheight))*cos(y[3]))+Beta*(windprofile(in,y[6]+flameheight))*sin(y[3]))*(windprofile(in,y[6]+flameheight))*sin(y[3])-((y[1]*y[4]*g*cos(y[3]))/(y[2]*in.Ta)))/y[2];
	dydx[4]=0.0*y[1]*sin(y[3]);
    dydx[5]=cos(y[3]);
    dydx[6]=sin(y[3]);
}

void locate(double x2[],unsigned long n,double x,long *j)
{
    unsigned long ju,jm,jl;        //function locates the value j such that x
    int ascnd;                     //is between x2[j] and x2[j+1];x2 must be
                                   //monotonic (either always increasing or
    jl=0;                          //always decreasing)
    ju=n+1;
    ascnd=(x2[n]>=x2[1]);          //x2[] is a vector (can also pass a row or
    while(ju-jl>1)                 //column of a matrix)
    {
        jm=(ju+jl)>>1;
        if(x>=x2[jm]==ascnd)
            jl=jm;
        else
            ju=jm;
    }
    if(x==x2[1])*j=1;
    else if(x==x2[n])*j=n-1;
    else *j=jl;
}

double linterp(double X[],double Y[],long n,double xi)
{
    long J;
    double yi,slope,b;
    locate(X,n,xi,&J);
    slope=(Y[J+1]-Y[J])/(X[J+1]-X[J]);
    b=Y[J]-slope*X[J];
    yi=slope*xi+b;
    return yi;
}

double ptinterp(double x1,double x2,double y1,double y2,double xi)
{
    double yi,slope,b;
    slope=(y2-y1)/(x2-x1);
    b=y1-slope*x1;
    yi=slope*xi+b;
    return yi;
}

void fillconvect0(void)
    {
        long jleft,jright,i,k,split;
        double maxsize=0.02;            //maximum x-distance between values in the convect array
        double dist,diff,size;
        double N1=1.35, lamda=1.0;      //constants controlling temp/vel profiles
        locate(xx,NSTEP,s_ltcanopyht,&jleft);
        locate(xx,NSTEP,s_rtcanopyht,&jright);
        jleft++;
        jright++;
        NPTS=jright-jleft+1;
        convect=matrix(1,3,1,NPTS+1);
        convect[1][1]=xplumeleft;
        convect[2][1]=in.Ta;
        convect[3][1]=0.0;
        i=2;
        for(j=jleft;j<jright;j++)
        {
            convect[1][i]=(in.canbaseht-rawplume[5][j]-rawplume[4][j]/(tan(rawplume[6][j])))*tan(rawplume[6][j])*(-1.0);
            dist=pow((pow2(rawplume[4][j]-convect[1][i])+pow2(in.canbaseht-rawplume[5][j])),0.5);
            convect[2][i]=in.Ta+(N1/pow2(lamda))*(rawplume[1][j]-in.Ta)*exp(-(pow2(dist)/(pow2(lamda)*pow2(rawplume[3][j]))));
            convect[3][i]=(N1/pow2(lamda))*(rawplume[2][j])*exp(-(pow2(dist)/(pow2(lamda)*pow2(rawplume[3][j]))));
            diff=convect[1][i]-convect[1][i-1];
            if(diff>maxsize)
            {
                split=ceil(diff/maxsize);
                size=diff/(double)split;
                convect2=matrix(1,3,1,NPTS+1+split-1);
                for(k=1;k<i;k++)
                {
                    convect2[1][k]=convect[1][k];
                    convect2[2][k]=convect[2][k];
                    convect2[3][k]=convect[3][k];
                }
                free_matrix(convect,1,3,1,NPTS+1);
                NPTS=NPTS+split-1;
                convect=matrix(1,3,1,NPTS+1);
                for(k=1;k<i;k++)
                {
                    convect[1][k]=convect2[1][k];
                    convect[2][k]=convect2[2][k];
                    convect[3][k]=convect2[3][k];
                }
                free_matrix(convect2,1,3,1,NPTS+1+split-1);
                convect[1][i+split-1]=(in.canbaseht-rawplume[5][j]-rawplume[4][j]/(tan(rawplume[6][j])))*tan(rawplume[6][j])*(-1.0);
                //                convect[1][i+split-1]=(in.canbaseht-(rawplume[5][j]+1.0/rawplume[6][j]*rawplume[4][j]))*tan(rawplume[6][j])*(-1.0);
                dist=pow((pow2(rawplume[4][j]-convect[1][i+split-1])+pow2(in.canbaseht-rawplume[5][j])),0.5);
                convect[2][i+split-1]=in.Ta+(N1/pow2(lamda))*(rawplume[1][j]-in.Ta)*exp(-(pow2(dist)/(pow2(lamda)*pow2(rawplume[3][j]))));
                convect[3][i+split-1]=(N1/pow2(lamda))*(rawplume[2][j])*exp(-(pow2(dist)/(pow2(lamda)*pow2(rawplume[3][j]))));
//                printf("\ndiff>maxsize\ti=%ld",i);
                for(k=i;k<i+split-1;k++)
                {
//                    printf("\ninner-loop");
                    convect[1][k]=convect[1][k-1]+size;
                    convect[2][k]=ptinterp(convect[1][i-1],convect[1][i+split-1],convect[2][i-1],convect[2][i+split-1],convect[1][k]);
                    convect[3][k]=ptinterp(convect[1][i-1],convect[1][i+split-1],convect[3][i-1],convect[3][i+split-1],convect[1][k]);
                }
                i=i+split;
                continue;
            }
            i++;
        }
    }

void fillconvect1(void)
{
    double cellsize=0.02;            //maximum x-distance between values in the convect array
    double N1=1.35, lamda=1.0;      //constants controlling temp/vel profiles
    double leftdist,rightdist,dist;
    double Tplumemid,Uplumemid;     //temp/vel at plume centerline and canopy height
    Tplumemid=linterp(xx,rawplume[1],NSTEP,s_canopyht);
    Uplumemid=linterp(xx,rawplume[2],NSTEP,s_canopyht);
    leftdist=xplumemid-xplumeleft;
    rightdist=xplumeright-xplumemid;
    long lnum,rnum,i;
    lnum=ceil(leftdist/cellsize);
    rnum=ceil(rightdist/cellsize);
    NPTS=lnum+rnum+1;
    convect=matrix(1,3,1,NPTS+1);
    for(i=1;i<=lnum+1;i++)
    {
        convect[1][i]=xplumemid-(lnum-i+1)*cellsize;
        dist=(lnum-i+1)*cellsize;
        convect[2][i]=in.Ta+(N1/pow2(lamda))*(Tplumemid-in.Ta)*exp(-(pow2(dist)/(pow2(lamda)*pow2(lnum*cellsize))));
        convect[3][i]=(N1/pow2(lamda))*(Uplumemid)*exp(-(pow2(dist)/(pow2(lamda)*pow2(lnum*cellsize))));
    }
//    convect[1][lnum+1]=xplumemid;
//    convect[2][lnum+1]=Tplumemid;
//    convect[3][lnum+1]=Uplumemid;
    for(i=lnum+2;i<=NPTS;i++)
    {
        convect[1][i]=xplumemid+(i-lnum-1)*cellsize;
        dist=(i-lnum-1)*cellsize;
        convect[2][i]=in.Ta+(N1/pow2(lamda))*(Tplumemid-in.Ta)*exp(-(pow2(dist)/(pow2(lamda)*pow2(rnum*cellsize))));
        convect[3][i]=(N1/pow2(lamda))*(Uplumemid)*exp(-(pow2(dist)/(pow2(lamda)*pow2(rnum*cellsize))));
    }
}

double gettemp(double x)
{
    double temp;
    if(x<=xplumeleft)
    {
        temp=in.Ta;
    }else if(x>=xplumeright)
    {
        temp=in.Ta;
    }else
    {
        temp=linterp(convect[1],convect[2],NPTS,x);
    }
    return temp;
}

double getspeed(double x)
{
    double speed;
    if(x<=xplumeleft)
    {
        speed=windprofile(in,in.canbaseht);
    }else if(x>=xplumeright)
    {
        speed=windprofile(in,in.canbaseht);
    }else
    {
        speed=linterp(convect[1],convect[3],NPTS,x);
    }
    return speed;
}


int plumemodel(void)
{
	double *vstart;

   do
   {
	vstart=vector(1,NVAR);
	/* Note: The arrays xx and y must have indices up to NSTEP+1 */
	xx=vector(1,NSTEP+1);
	y=matrix(1,NVAR,1,NSTEP+1);
    vstart[1]=(rhoa*(in.Ta/Ti))*(flamedepth/2.0)*(pow(((2.0*g*iByram)/(rhoa*Cp*(in.Ta))),(1.0/3.0)));
    vstart[2]=(rhoa*(in.Ta/Ti))*(flamedepth/2.0)*(pow((pow(((2.0*g*iByram)/(rhoa*Cp*(in.Ta))),(1.0/3.0))),2.0));
    vstart[3]=PI/2.0;
    vstart[4]=(rhoa*(in.Ta/Ti))*(flamedepth/2.0)*(pow(((2.0*g*iByram)/(rhoa*Cp*(in.Ta))),(1.0/3.0)))*(Ti-in.Ta);
    vstart[5]=0.0;
    vstart[6]=0.0;
    rkdumb(vstart,NVAR,x1,x2,NSTEP,derivs);
    free_vector(vstart,1,NVAR);
    rawplume=matrix(1,6,1,NSTEP+1);
    for(j=1;j<=NSTEP;j++)
    {

//        printf("\nTa %lf\ty4 %lf\ty1 %lf",in.Ta,y[4][j],y[1][j]);
        rawplume[1][j]=in.Ta+y[4][j]/y[1][j];           //Tp (plume temperature)
        rawplume[2][j]=y[2][j]/y[1][j];                 //Up (plume velocity)
        rawplume[3][j]=y[1][j]*y[1][j]/(y[2][j]*(Rhoa*in.Ta/rawplume[1][j]));   //b (plume half-width)
        rawplume[4][j]=y[5][j]-flamedepth/2.0;          //x (horizontal distance from the point of interest to the flame leading edge)
        rawplume[5][j]=y[6][j]+flameheight;             //z (vertical distance from the point of interest to the ground)
        rawplume[6][j]=y[3][j];                         //theta (angle between s line and horizontal)
    }
    free_matrix(y,1,NVAR,1,NSTEP+1);
    if(flameheight>=in.canbaseht)                       //exit program if flames touch the canopy
    {
//        printf("\n*************Crown fire initiation HAS occured.*************\n**The flame height is greater than the canopy base height.**\n");
//        exit(1);
		return(1);
    }
    s_canopyht=linterp(rawplume[5],xx,NSTEP,in.canbaseht);      //get the s value at the canopy base height
    xplumemid=linterp(xx,rawplume[4],NSTEP,s_canopyht);         //get the x value where the plume centerline crosses the canopy base height

    calc_xplumert();

   }while(px2!=x2);

    calc_xplumelt();


    if(convectflag<=0){
        fillconvect0();
    }else{
        fillconvect1();
    }
	return(0);
}

void calc_xplumert(void)
{
  do{
    j=0;
    px2=x2;
    do
    {
        j++;
        if(j>NSTEP)
        {   x2+=2.0;
//            printf("Trying x2=%lf\n", x2);
            break;
        }
        zinternrt2=rawplume[3][j]*cos(rawplume[6][j]);
//        printf("%ld\n", j);
    }while((rawplume[5][j]-zinternrt2)<in.canbaseht);
    if(px2!=x2)
        break;
    zerror2=(rawplume[5][j]-zinternrt2)-in.canbaseht;
//    printf("\nj is %i\n",j);
//    printf("\nzerror2 is %lf\n",zerror2);
    j--;
    zinternrt1=rawplume[3][j]*cos(rawplume[6][j]);
    zerror1=(rawplume[5][j]-zinternrt1)-in.canbaseht;
    s_rtcanopyht=ptinterp(zerror1,zerror2,xx[j],xx[j+1],0.0);           //s value of the right(down wind) side of the plume at canopy height
//    printf("\nj is %i\n",j);
//    printf("\nzerror1 is %lf\n",zerror1);
//    printf("\ns_rtcanopyht is %lf\n",s_rtcanopyht);
//    double check;
//    check=linterp(xx,rawplume[5],NSTEP,s_rtcanopyht)-zinternrt1+flameheight;
//    printf("\ncheck is %lf\n",check);
    xinternrt=linterp(xx,rawplume[3],NSTEP,s_rtcanopyht)*sin(linterp(xx,rawplume[6],NSTEP,s_rtcanopyht));
    xplumeright=xinternrt+linterp(xx,rawplume[4],NSTEP,s_rtcanopyht);      //x value of the right side of the plume at the canopy base height
//    printf("\nxplumeright is %lf\n",xplumeright);
  }while(px2!=x2);
}

void calc_xplumelt(void)
{
    j=0;
    do
    {
        j++;
        zinternlt2=rawplume[3][j]*sin((PI/2.0)-rawplume[6][j]);
    }while((rawplume[5][j]+zinternlt2)<in.canbaseht);
    zerror2=(rawplume[5][j]+zinternlt2)-in.canbaseht;
//    printf("\nj is %i\n",j);
//    printf("\nzerror2 is %lf\n",zerror2);
    j--;
    zinternlt1=rawplume[3][j]*sin((PI/2.0)-rawplume[6][j]);
    zerror1=(rawplume[5][j]+zinternlt1)-in.canbaseht;
    s_ltcanopyht=ptinterp(zerror1,zerror2,xx[j],xx[j+1],0.0);           //s value of the left (up wind) side of the plume at canopy height
//    printf("\nj is %i\n",j);
//    printf("\nzerror1 is %lf\n",zerror1);
//   printf("\ns_ltcanopyht is %lf\n",s_ltcanopyht);
//    check=linterp(xx,rawplume[5],NSTEP,s_ltcanopyht)+zinternlt1+flameheight;
//    printf("\ncheck is %lf\n",check);
    xinternlt=linterp(xx,rawplume[3],NSTEP,s_ltcanopyht)*cos((PI/2.0)-(linterp(xx,rawplume[6],NSTEP,s_ltcanopyht)));
    xplumeleft=linterp(xx,rawplume[4],NSTEP,s_ltcanopyht)-xinternlt;      //x value of the left side of the plume at the canopy base height
}

double fxn(double pt[],double wgt)  //function inside the quadruple integral for radiation transfer
{
	double ans,Trad,S;

//    Trad=(maxflmtemp-in.igtemp)/flamedepth*pt[1]+maxflmtemp;
    Trad=radflametemp(-pt[1]);
    S=pow((pow(pt[4]-pt[2],2.0)+pow(in.canbaseht,2.0)+pow(pt[1]-pt[3],2.0)),0.5);
    ans=(femiss*SB*pow(Trad,4.0)*exp(-attenuation*S)*pow(in.canbaseht/S,2.0))/(PI*pow(S,2.0));
	return ans;
}

double getrad(double Dx,double Tp)
{
  double Ap;            //Ap is the area of the particle
  double rad,I12;       //I12 is the irradiation from flame to fuel particle
  if(radflag<=0)
  {
    int init,itmax,j,ncall,nprn=0;      //This function uses the VEGAS Monte Carlo program from Numerical Recipes in C on
	double avgi,chi2a,sd,xoff;          //page 320.  Most of the variables used here are described in the book.
	double *regn;                       //Dx is distance from flame leading edge to particle center; Tp is particle temperature.
    double temp;
    long i;
    timeb t;        //from the header file <sys/timeb.h>

	regn=vector(1,8);
//	printf("IDUM=\n");
//	scanf("%ld",&idum);
    ftime(&t);                //set seed (idum) for rand2 using time from the system
	srand(t.time+t.millitm);
    idum=-((rand()%10000)+1);

	if (idum > 0) idum = -idum;

//		printf("ENTER NCALL,ITMAX,NPRN\n");
//		if (scanf("%d %d %d",&ncall,&itmax,&nprn) == EOF) break;
	avgi=sd=chi2a=0.0;
    regn[1]=-flamedepth;    //set the limits of integration
    regn[2]=-Wf/2.0;        // 1-4 are the "lower limits" and 5-8 are the "upper limits"
    regn[3]=Dx-Lp/2.0;      //   1&5 = flame "x" limits
    regn[4]=-Wp/2.0;        //   2&6 = flame "y" limits
    regn[5]=0.0;            //   3&7 = particle "x" limits
    regn[6]=Wf/2.0;         //   4&8 = particle "y" limits
    regn[7]=Dx+Lp/2.0;
    regn[8]=Wp/2.0;

//    maxflmtemp=maxflametemp(wind);

	init = 0;
    ncall=1000;             //controls the number of iterations in the monte-carlo algorithm for radiation
    itmax=5;
	vegas(regn,ndim,fxn,init,ncall,itmax,nprn,&avgi,&sd,&chi2a);
//	printf("Number of iterations performed: %d\n",itmax);
//	printf("Integral, Standard Dev., Chi-sq. = %12.6f %12.6f% 12.6f\n",
//		avgi,sd,chi2a);
	init = 1;
    ncall=10000;             //controls the number of iterations in the monte-carlo algorithm for radiation
    itmax=1;
	vegas(regn,ndim,fxn,init,ncall,itmax,nprn,&avgi,&sd,&chi2a);
//	printf("Additional iterations performed: %d \n",itmax);
//	printf("Integral, Standard Dev., Chi-sq. = %12.6f %12.6f% 12.6f\n",
//			avgi,sd,chi2a);

//    for(i=0;i<=5;i++){
//    init = 2;
//    ncall=100000;
//    itmax=1;
//	vegas(regn,ndim,fxn,init,ncall,itmax,nprn,&avgi,&sd,&chi2a);
//	printf("Additional iterations performed: %d \n",itmax);
//	printf("Integral, Standard Dev., Chi-sq. = %12.6f %12.6f% 12.6f\n",
//			avgi,sd,chi2a);
//    }
    Ap=Wp*Lp;
    I12=avgi/Ap;    //avgi is the total radiation to the particle; Ap is the assumed rectangular particle area for radiation calculation
    rad=I12*pemiss*part_surf_area*0.5-part_surf_area*(pemiss*SB*pow(Tp,4.0)-SB*pow(in.Ta,4.0));
                        //note that energy recieved by the particle from the surroundings is not accounted for; assumed negligible
//    printf("\nI12=%lf  rad=%lf",I12,rad);
//    printf("\nradflux = %lf\n",rad);
//    for(i=0;i<=301;i++)
//    {
//        temp=(maxflmtemp-in.igtemp)/flamedepth*(double)i/(-100.0)+maxflmtemp;
//        printf("\nX = %lf\ttemperature = %lf",(double)(i)/(-100.0),temp);
//    }

	free_vector(regn,1,20);
//	printf("\n\nNormal completion\n");
	return rad;
  }else{
    gauleg(0.0,1.0,n);
    I12=qgaus(f1,Y1,Y2,Dx,Tp);
    rad=I12*pemiss*part_surf_area*0.5-part_surf_area*(pemiss*SB*pow(Tp,4.0)-SB*pow(in.Ta,4.0));
                        //note that energy recieved by the particle from the surroundings is not accounted for; assumed negligible
    //printf("\nDx=%lf flame=%lf loss=%lf rad=%lf I=%lf",Dx,I12*pemiss*part_surf_area*0.5,-part_surf_area*(pemiss*SB*pow(Tp,4.0)-SB*pow(in.Ta,4.0)),rad,I12);

    return rad;
  }
}

double integration_function(double x,double y,double Dx,double Tp)
{                               //function integrated by gaussian quadrature to obtain radiant flux to fuel particle
    double ans,Trad,S;
    Trad=radflametemp(-x);
    S=pow((pow(0.0-y,2.0)+pow(in.canbaseht,2.0)+pow(x-Dx,2.0)),0.5);
    ans=(femiss*SB*pow(Trad,4.0)*exp(-attenuation*S)*pow(in.canbaseht/S,2.0))/(PI*pow(S,2.0));
    return ans;
}

double f1(double y,double Dx,double Tp)
{                               //used in gaussian quadrature integration to obtain radiant flux to fuel particle
//    i++;
//    printf("\nf1 %ld",i);
    ysav=y;
    return qgaus(f2,xx1(y),xx2(y),Dx,Tp);
}

double f2(double x,double Dx,double Tp)
{                               //used in gaussian quadrature integration to obtain radiant flux to fuel particle
//    j++;
//    printf("\nf2 %ld",j);
    return integration_function(ysav,x,Dx,Tp);
}

double xx1(double y)
{                               //used in gaussian quadrature integration to obtain radiant flux to fuel particle
    return 0.0;
}

double xx2(double y)
{                               //used in gaussian quadrature integration to obtain radiant flux to fuel particle
    return in.canbaseht;
}

double qgaus(double (*func)(double,double,double), double a, double b,double Dx,double Tp)
{                               //gaussian quadrature function
	long j;
	double xr,xm,dx,s;
//	static double x[]={0.0,0.1488743389,0.4333953941,
//		0.6794095682,0.8650633666,0.9739065285};
//	static double w[]={0.0,0.2955242247,0.2692667193,
//		0.2190863625,0.1494513491,0.0666713443};

	xm=0.5*(b+a);
	xr=0.5*(b-a);
	s=0;
	for (j=1;j<=n;j++) {
		dx=xr*x[j];
		s += w[j]*((*func)(xm+dx,Dx,Tp)+(*func)(xm-dx,Dx,Tp));
	}
	return s *= xr;
}

void gauleg(double x1, double x2, long n)
{                               //function to give absicissas and weights for the Gaus-Legendre case of Gaussian quadrature
	long m,j,i;
	double z1,z,xm,xl,pp,p3,p2,p1;

	m=(n+1)/2;
	xm=0.5*(x2+x1);
	xl=0.5*(x2-x1);
	for (i=1;i<=m;i++) {
		z=cos(3.141592654*(i-0.25)/(n+0.5));
		do {
			p1=1.0;
			p2=0.0;
			for (j=1;j<=n;j++) {
				p3=p2;
				p2=p1;
				p1=((2.0*j-1.0)*z*p2-(j-1.0)*p3)/j;
			}
			pp=n*(z*p1-p2)/(z*z-1.0);
			z1=z;
			z=z1-p1/pp;
		} while (fabs(z-z1) > EPS);
		x[i]=xm-xl*z;
		x[n+1-i]=xm+xl*z;
		w[i]=2.0*xl/((1.0-z*z)*pp*pp);
		w[n+1-i]=w[i];
	}
}

double radflametemp(double X)
{
    double beta=8.0,k=1.0,Adimen600,rise,ig,decay,coef,res,t;

    t=X/ROS;
    Adimen600=(in.igtemp-in.Ta)/(maxflmtemp-in.Ta);
    rise=sqrt(-1.0*pow(beta,2.0)*(log(Adimen600)));
    ig=-rise;
    decay=(-1.0*(log(Adimen600)))/(taur+ig);
    if((t+ig)<0.0)
    {
        res=k*exp(-1.0*(pow((t+ig),2.0))/pow(beta,2.0))*(maxflmtemp-in.Ta)+in.Ta;
    }else
    {
        res=k*exp(-1.0*(decay*(t+ig)))*(maxflmtemp-in.Ta)+in.Ta;
    }
    return(res);
}

double getconvectflux(double Dx,double Tp)  //Dx is distance from flame leading edge to particle center, Tp is particle temperature.
{
    double Tfilm,Tair,Vair,rhoair,cpair,visco,kinemvis,thermcond,thermdif,Reynolds,Nusselt,h,flux;

    Tair=gettemp(Dx);
    Vair=getspeed(Dx);
    Tfilm=(Tair+Tp)/2.0;
    rhoair=358.98*pow(Tfilm,-1.0046);
    cpair=-3.0*pow(10,-10.0)*pow(Tfilm,3.0)+7.0*pow(10.0,-7.0)*pow2(Tfilm)-0.0003*Tfilm+1.0486;
    visco=pow(10.0,-14.0)*pow(Tfilm,3.0)-4.0*pow(10.0,-11.0)*pow2(Tfilm)+7.0*pow(10.0,-8.0)*Tfilm+8.0*pow(10.0,-7.0);
    kinemvis=visco/rhoair;
    thermcond=2.0*pow(10.0,-11.0)*pow(Tfilm,3.0)-6.0*pow(10.0,-8.0)*pow2(Tfilm)+0.0001*Tfilm-0.0015;
    Reynolds=(Vair*in.diameter)/kinemvis;
    Nusselt=0.1417*pow(Reynolds,0.6053);
    h=Nusselt*thermcond/in.diameter;        //in W/m^2/K
//    printf("\nh= %lf\n",h);
//    printf("\nTair= %lf\n",Tair);
    //fprintf(fout,"%lf\t",Tair);
    flux=h*(Tair-Tp);       //positive flux means energy goes INTO the fuel particle
    return(flux);
}


#undef NRANSI

