//----------
//  $Id$
//----------
#include "stdafx.h"

#define NRANSI
#include "nrutilcpp.h"

/*typedef struct input {
    double u10;double slope;double Ta;double sh;double alpha;double igtemp;
    long FuelModelNumber;double FuelMoisture[5];double rho_surf;double delta;double Wa;double sigma_surf;double hc;
    double sigma_can;

}Input;
Input in;
*/
void rk4(double y[], double dydx[], int n, double x, double h, double yout[],
	void (*derivs)(double, double [], double []))
{
	int i;
	double xh,hh,h6,*dym,*dyt,*yt;

	dym=vector(1,n);
	dyt=vector(1,n);
	yt=vector(1,n);
	hh=h*0.5;
	h6=h/6.0;
	xh=x+hh;
	for (i=1;i<=n;i++) yt[i]=y[i]+hh*dydx[i];
	(*derivs)(xh,yt,dyt);
	for (i=1;i<=n;i++) yt[i]=y[i]+hh*dyt[i];
	(*derivs)(xh,yt,dym);
	for (i=1;i<=n;i++) {
		yt[i]=y[i]+h*dym[i];
		dym[i] += dyt[i];
	}
	(*derivs)(x+h,yt,dyt);
	for (i=1;i<=n;i++)
		yout[i]=y[i]+h6*(dydx[i]+dyt[i]+2.0*dym[i]);
	free_vector(yt,1,n);
	free_vector(dyt,1,n);
	free_vector(dym,1,n);
}
#undef NRANSI
