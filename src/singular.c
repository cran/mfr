#include <string.h>
#include "splitting.h"

extern int verbose;

void Singular(int *edges1, int *edges2, int *N, int *S, int *V,
         double *graded, int *pd, int *reg, int *punted, char **TEMPNAME)
{
	int i,j,k;
   int **edges;
	int s=*S,n=*N;
	Graph *g;
	MFR *mfr;
	char *tempname;

	tempname = *TEMPNAME;
	verbose=*V;

	edges = Calloc(s,int *);
	for(i=0;i<s;i++){
	   edges[i] = Calloc(2,int);
		edges[i][0] = edges1[i]-1;
		edges[i][1] = edges2[i]-1;
	}
	g = makeGraph(edges,s,n);
	mfr = singular(g,tempname);
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

MFR *singular(Graph *g, char *tempname)
{
	int i,j,n=g->n,s=g->s;
	int pd,reg;
	MFR *mfr;
   unsigned long **M;
	FILE *fp;
	char tempout[200];
	char str[1100];
	int **edges=g->edges;


	if(verbose>0){
	   fprintf(stderr,"Calling Singular\n");
	}
	if(n>=14){
		fprintf(stderr,"WARNING: Calling Singular with a (%d,%d) graph\n",n,s);
		fprintf(stderr,"Estimated time to complete: %f minutes\n",
		        exp(1.5*n-15.)/60.);
		fprintf(stderr,"(Estimate from a 2.1 Ghz laptop)\n");
		fprintf(stderr,"This estimate is quite crude, and probably high\n");
		fprintf(stderr,"(wildly innaccurate is another way of saying it)\n");
		fprintf(stderr,"if the graph is not dense.\n");
		if(n>19){
		   fprintf(stderr,
			   "However, with n=%d, it is likely to take a VERY long time\n",
				n);
		}
	}
	if((fp=fopen(tempname,"w"))==NULL){
	   fprintf(stderr,"Could not open temporary Singular input file %s\n",
		        tempname);
	   exit(3);
	}

	fprintf(fp,"ring R=0, (x(1..%d)), dp;\n",n);
	fprintf(fp,"ideal I =");
	if(s>1){
		for(i=0;i<s-1;i++){
			fprintf(fp,"x(%d)*x(%d),\n",edges[i][0]+1,edges[i][1]+1);
		}
	}
	fprintf(fp,"x(%d)*x(%d);\n",edges[s-1][0]+1,edges[s-1][1]+1);
	fprintf(fp,"resolution fI=mres(I,0);\n");
	fprintf(fp,"print(betti(fI),\"betti\");");
	fprintf(fp,"quit;\n");
	fclose(fp);
	sprintf(tempout,"%s.out",tempname);

	sprintf(str,"Singular < %s > %s",tempname,tempout);
	system(str);
	remove(tempname);

	if((fp=fopen(tempout,"r")) == NULL){
	   fprintf(stderr,"Could not open temporary output file %s\n",tempout);
		exit(4);
	}
	pd = 0;
	while(fgets(str,1000,fp) != NULL){
	   if(strstr(str,"FB Mathematik")){
		   fgets(str,1000,fp);
			for(i=strlen(str)-2;str[i]!=' ';i--);
			sscanf(&str[i],"%d",&pd);
			pd++;
		   fgets(str,1000,fp);
		   fgets(str,1000,fp);
			while(strstr(str,"-----") == NULL){
			   fscanf(fp,"%d:",&reg);
				fgets(str,1000,fp);
			}
			reg++;
			break;
		}
	}
	fclose(fp);
	pd--;
	mfr = makeMFR(pd,reg);
	if((fp=fopen(tempout,"r")) == NULL){
	   fprintf(stderr,"Could not open temporary output file %s\n",tempout);
		exit(4);
	}
	M = mfr->graded;
	while(fgets(str,1000,fp) != NULL){
	   if(strstr(str,"FB Mathematik")){
		   fgets(str,1000,fp);
		   fgets(str,1000,fp);
			for(i=0;i<reg;i++){
				fscanf(fp,"%*d:");
				for(j=0;j<=pd;j++){
				   fscanf(fp,"%ld",&M[i+1][j+1]);
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
