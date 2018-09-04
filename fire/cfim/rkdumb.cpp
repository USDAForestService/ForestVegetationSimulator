//----------
// FIRE-CFIM $Id$
//----------
#include "stdafx.h"

#define NRANSI
#include "nrutilcpp.h"

double **y,*xx;

/*typedef struct input {
    double u10;double slope;double Ta;double sh;double alpha;double igtemp;
    long FuelModelNumber;double FuelMoisture[5];double rho_surf;double delta;double Wa;double sigma_surf;double hc;
    double sigma_can;

}Input;
Input in;
*/

void rkdumb(double vstart[], int nvar, double x1, double x2, int nstep,
	void (*derivs)(double, double [], double []))
{
	void rk4(double y[], double dydx[], int n, double x, double h, double yout[],
		void (*derivs)(double, double [], double []));
	int i,k;
	double x,h;
	double *v,*vout,*dv;

	v=vector(1,nvar);
	vout=vector(1,nvar);
	dv=vector(1,nvar);
	for (i=1;i<=nvar;i++) {
		v[i]=vstart[i];
		y[i][1]=v[i];
	}
	xx[1]=x1;
	x=x1;
	h=(x2-x1)/nstep;
	for (k=1;k<=nstep;k++) {
		(*derivs)(x,v,dv);
		rk4(v,dv,nvar,x,h,vout,derivs);
		if ((double)(x+h) == x) nrerror("Step size too small in routine rkdumb");
		x += h;
		xx[k+1]=x;
		for (i=1;i<=nvar;i++) {
			v[i]=vout[i];
			y[i][k+1]=v[i];
		}
	}
	free_vector(dv,1,nvar);
	free_vector(vout,1,nvar);
	free_vector(v,1,nvar);
}
#undef NRANSI
