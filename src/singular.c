#include <string.h>
#include "splitting.h"

#ifdef __unix__ 
#define SINGULAR "Singular"
#elif defined _WIN32 
#define SINGULAR "bash.exe Singular"
#endif

extern int verbose;

void Singular(int *edges1, int *edges2, int *N, int *S, int *V,
         double *graded, int *pd, int *reg, int *punted, char **TEMPNAME,
			int *QUIET)
{
	int i,j,k;
   int **edges;
	int s=*S,n=*N;
	Graph *g;
	MFR *mfr;
	char *tempname;
	int quiet=*QUIET;

	tempname = *TEMPNAME;
	verbose=*V;

	edges = Calloc(s,int *);
	for(i=0;i<s;i++){
	   edges[i] = Calloc(2,int);
		edges[i][0] = edges1[i]-1;
		edges[i][1] = edges2[i]-1;
	}
	g = makeGraph(edges,s,n);
	mfr = singular(g,tempname,quiet);
	(*punted)++;
	*pd = mfr->pd;
	*reg = mfr->reg;

	k=0;
	for(i=1;i<mfr->reg+1;i++){
	   for(j=1;j<mfr->pd+2;j++){
		   graded[k] = (double)(mfr->graded[i][j]);
			k++;
		}
	}

	freeGraph(&g);
	freeMFR(&mfr);

}

MFR *singular(Graph *g, char *tempname, int quiet)
{
	int i,j,n=g->n,s=g->s;
	int pd,reg;
	MFR *mfr;
   unsigned long **M;
	FILE *fp;
	char tempout[200];
	char str[1100];
	int **edges=g->edges;
	int junk;
	char *junkc;
	double t1,t2;


	if(verbose>0){
	   Rprintf("Calling Singular\n");
	}
	if(!quiet){
		if((n>=20) && (s>=25)){
			Rprintf("WARNING: Calling Singular with a (%d,%d) graph\n",n,s);
			Rprintf("This may take a long time.\n");
			t1 = exp(s/3-7.5367);
			t2 = exp(0.3669*s-6.8894);
			if(t1>120){
				Rprintf("A very crude estimate is: between %lf and %lf minutes.\n",
				   t1/60,t2/60);
			} else {
				Rprintf("A very crude estimate is: between %lf and %lf seconds.\n",
				   t1,t2);
			}
		}
		if((s>=35) && (n>=30)){
		   Rprintf("You are in uncharted here.\n");
		}
	}
	if((fp=fopen(tempname,"w"))==NULL){
	   error("Could not open temporary Singular input file %s\n",
		        tempname);
	}

	fprintf(fp,"ring R=0, (x(1..%d)), dp;\n",n);
	fprintf(fp,"ideal I =");
	if(s>1){
		for(i=0;i<s-1;i++){
			fprintf(fp,"x(%d)*x(%d),\n",edges[i][0]+1,edges[i][1]+1);
		}
	}
	fprintf(fp,"x(%d)*x(%d);\n",edges[s-1][0]+1,edges[s-1][1]+1);
	fprintf(fp,"resolution fI=res(I,0);\n");
	fprintf(fp,"print(betti(fI),\"betti\");");
	fprintf(fp,"quit;\n");
	fclose(fp);
	sprintf(tempout,"%s.out",tempname);

	sprintf(str,"%s < %s > %s",SINGULAR,tempname,tempout);
	junk = system(str);
	if(junk == -1) {
	  error("Call to singular failed. See %s for the input and %s for output\n",
	  tempname,tempout);
	}
	remove(tempname);

	if((fp=fopen(tempout,"r")) == NULL){
	   error("Could not open temporary output file %s\n",tempout);
	}
	pd = 0;
	while(fgets(str,1000,fp) != NULL){
	   if(strstr(str,"FB Mathematik")){
		   junkc = fgets(str,1000,fp);
			for(i=strlen(str)-2;str[i]!=' ';i--);
			junk = sscanf(&str[i],"%d",&pd);
			pd++;
		   junkc = fgets(str,1000,fp);
		   junkc = fgets(str,1000,fp);
			while(strstr(str,"-----") == NULL){
			   junk = fscanf(fp,"%d:",&reg);
				junkc = fgets(str,1000,fp);
			}
			reg++;
			break;
		}
	}
	fclose(fp);
	pd--;
	mfr = makeMFR(pd,reg);
	if((fp=fopen(tempout,"r")) == NULL){
	   error("Could not open temporary output file %s\n",tempout);
	}
	M = mfr->graded;
	while(fgets(str,1000,fp) != NULL){
	   if(strstr(str,"FB Mathematik")){
		   junkc = fgets(str,1000,fp);
		   junkc = fgets(str,1000,fp);
			for(i=0;i<reg;i++){
				junk = fscanf(fp,"%*d:");
				for(j=0;j<=pd;j++){
				   junk = fscanf(fp,"%ld",&M[i+1][j+1]);
				}
			}
			break;
		}
	}
	M[1][1] = 1;
	fclose(fp);
	remove(tempout);
	return(mfr);
}
