#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

extern void calc_overall_entropy(int nklass,int *ptrainmtx,int *punselected,int lenunselected,int *ptermscnt,int *ppredistr,int cntspre,int totaltermscnt,double *poverallentropy);

void  get_overallentropy(SEXP nCLASS,SEXP trainmtx,SEXP unselected,SEXP lengthunselected,SEXP termscnt,SEXP predistr,SEXP precnts, SEXP totaltermscnt, SEXP overallentropy)
{

	int nklass;
	int *ptrainmtx;
	int *punselected;
	int lenunselected;
	int *ptermscnt;
	int *ppredistr;
	int cntspre;
	int cnttotalterms;
	
	nklass=Rf_asInteger(nCLASS);
	
	PROTECT(trainmtx = coerceVector(trainmtx, INTSXP));
	ptrainmtx=INTEGER(trainmtx); 
	
	PROTECT(unselected = coerceVector(unselected, INTSXP));
	punselected=INTEGER(unselected); 
	
	lenunselected=Rf_asInteger(lengthunselected);
	
	PROTECT(termscnt = coerceVector(termscnt, INTSXP));
	ptermscnt=INTEGER(termscnt); 
	
	PROTECT(predistr = coerceVector(predistr, INTSXP));
	ppredistr=INTEGER(predistr); 

	cntspre=Rf_asInteger(precnts);
	
	cnttotalterms=Rf_asInteger(totaltermscnt);
	//=============================================	
	//SEXP theta;
	//theta = Rf_allocVector(REALSXP,lenunselected);
	//double *ptheta=REAL(theta); 
	
	overallentropy = coerceVector(overallentropy, REALSXP);
	double *poverallentropy=REAL(overallentropy); 
	
	calc_overall_entropy(nklass,ptrainmtx,punselected,lenunselected,ptermscnt,ppredistr,cntspre,cnttotalterms,poverallentropy);
	
	//===============================================
	UNPROTECT(4);

	return ;
	
}
