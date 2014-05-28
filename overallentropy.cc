#include <iostream>
#include <cmath>
#include "fmath.hpp"

using namespace std;

extern "C" void calc_overall_entropy(int nklass,int *ptrainmtx,int *punselected,int lenunselected,int *ptermscnt,int *ppredistr,int cntspre,int totaltermscnt,double *poverallentropy);

void calc_overall_entropy(int nklass,int *ptrainmtx,int *punselected,int lenunselected,int *ptermscnt,int *ppredistr,int cntspre,int totaltermscnt,double *poverallentropy)
{
	double logoveralldistr[nklass];
	double preoverallentropy=0;
	for (size_t i=0; i<nklass; ++i)
		{
		if (ppredistr[i]!=0)
			{logoveralldistr[i]=-ppredistr[i]*fmath::log(ppredistr[i]);
			preoverallentropy+=logoveralldistr[i];}
		else
			logoveralldistr[i]=0;
		}

		
	for (size_t i=0; i<lenunselected; ++i)
		{
		int col=punselected[i]-1 ; //in c and c++, index start from 0,in R start from 1
		int startidx=col*nklass;
		double supentropy=0; //support entropy
		for (size_t row=0; row<nklass; ++row)
			{
			int idx=startidx+row;
			if (ptrainmtx[idx]!=0)
				{
				poverallentropy[i]-=(ptrainmtx[idx]+ppredistr[row])*fmath::log(ptrainmtx[idx]+ppredistr[row]);
				supentropy+=logoveralldistr[row];
				}
			}
		poverallentropy[i]+=(preoverallentropy-supentropy);
		poverallentropy[i]+= (ptermscnt[col]+cntspre)*fmath::log(ptermscnt[col]+cntspre);
		}
		
  return;
}
