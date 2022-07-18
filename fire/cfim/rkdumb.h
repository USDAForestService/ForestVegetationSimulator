//----------
//  $Id$
//----------


#ifndef RKDUMB
#define RKDUMB

void rkdumb(double vstart[], int nvar, double x1, double x2, int nstep,
	void (*derivs)(double, double [], double []));


#endif     // RKDUMB


