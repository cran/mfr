#include "splitting.h"

extern int verbose;

int components(Graph *g,int *comp)
{
   int i,j;
	int n=g->n,s=g->s;
	int **edges=g->edges;
	int curcomp,v,changed,zeros;

	memset(comp,0,sizeof(int));

	curcomp = 1;
	zeros=1;
	comp[0] = curcomp;

	while(zeros){
		changed=1;
		zeros=0;
		while(changed){
			changed=0;
			for(j=0;j<n;j++){
				if(comp[j]==curcomp){
					for(i=0;i<s;i++){
						if((comp[edges[i][0]]==0) | (comp[edges[i][1]]==0)){
							if(edges[i][0]==j) {
								changed=1;
								comp[edges[i][1]] = curcomp;
							}
							if(edges[i][1]==j) {
								changed=1;
								comp[edges[i][0]] = curcomp;
							}
						}
					}
				}
			}
		}
		curcomp++;
		for(i=0;i<n;i++){
		   if(comp[i]==0){
			   zeros=1;
				v=i;
				comp[i]=curcomp;
				break;
			}
		}
	}
	return(curcomp-1);
}

