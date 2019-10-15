#include "SUN.h"


/*************************************************************
 *                                                           *
 *     Kennedy-Pendleton su2 heatbath code                   *
 *     p contains the 'staple'                               *
 *     act contains the resulting action                     *
 *                                                           *
 *     act = u*p', weight exp[-u*p']                         *
 *                                                           *
 ************************************************************/


#define looplim 200


//this is might be too much to have in a single kernel
loop_callable
void KennedyPendleton(
  matrix<2,2,cmplx<double>> &U,
  matrix<2,2,cmplx<double>> &staple
)
{
  int nloops;
  matrix<2,2,cmplx<double>> a,ua;
  double e,f,r1,r2,r3,rd,b,theta;
  double pi2 = PI*2.0;

  /* first, normalize and move to u [-] because want exp[up] */

  b = 1.0/sqrt(det(staple).re);
  ua = -b*staple;


  /*     generate random su2 variable according to Pendleton
   *     and Kennedy, PL 156B(1985)393
   */

  nloops = 0;
  do {
    r1 = log(1.0-hila_random());
    r2 = log(1.0-hila_random());
    r3 = cos(pi2*hila_random());
    r3 *= r3;
    e  = 1.0 + (r1+r2*r3)*b;
    r3 = hila_random();
    f  = e - 2.0*r3*r3 + 1.0;
  } while (nloops++ < looplim && (isnan(e) || f <= 0.0));

  if (nloops >= looplim) {
    //printf(" ---> K-P loop limit %d exceeded!\n",looplim);
    //printf(" ---> staple magnitude %g\n",1/b);
    /* exit(0); */
    e = 1e-9;
  }

  /* generate random vector on s2, radius sqrt(1.0-a.d**2) */
  a.c[0][0].re = e;
  a.c[1][1].re = e;
  rd    = 1.0 - e*e;
  if (rd <= 0.0) {
    // TODO: Is this ok way to handle warnings? We are wasting 31 of 32 bits
    // Could we perhaps do better?
//    warning=1;
    //fprintf(outf," ---> neg. K-P sqrt, val %g\n",rd);
    rd = 0.0;
  }
  r3    = 2.0*hila_random() - 1.0;
  a.c[0][0].im = sqrt(rd)*r3;
  a.c[1][1].im =-sqrt(rd)*r3;
  theta = pi2*hila_random();
  rd    = sqrt(rd*(1.0 - r3*r3));
  a.c[0][1].re = rd*cos(theta);
  a.c[1][0].re =-rd*cos(theta);
  a.c[0][1].im = rd*sin(theta);
  a.c[1][0].im = rd*sin(theta);

  /* a = -u*p' => u = -a*p = a*ua */
  U = a*ua;
}
