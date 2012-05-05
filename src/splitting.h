#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <R.h>
#include <Rdefines.h>
#include <R_ext/Utils.h>


#define max(x,y) (((x)>(y))?(x):(y))
#define min(x,y) (((x)<(y))?(x):(y))

typedef struct {
	int n,s; 					/* order and size of graph */
	int **edges; 				/* edge pairs */
	int *degrees; 				/* degrees of the vertices */
} Graph;

typedef struct {
	unsigned long **graded; 				/* graded betti numbers */
	int pd,reg;
	int nrow,ncol;
	int emptygraph;
	int notfound;
} MFR;

/* extern int **adjacency2edges(int **, int , int *); */
extern Graph *makeGraph( int **, int , int );
extern MFR *makeMFR( int , int );
extern Graph *subgraph( Graph *, int *);
extern void freeGraph(Graph **);
extern void freeMFR(MFR **);
extern unsigned long mychoose(int, int);
extern MFR *mfrSplitting(Graph *, int , int *, int *, int, char *, int );
extern void fprintfMFR(MFR *);
extern int isStar(Graph *,int *);
extern int isEmpty(Graph *);
extern MFR *mfrStar(int );
extern MFR *mfrEmpty(void);
extern int isComplete(Graph *);
extern MFR *mfrComplete(int);
extern int isCompleteBipartite(Graph *, int *, int *);
extern MFR *mfrCompleteBipartite(int, int);
extern int isCycle(Graph *);
extern MFR *mfrCycle(int );
extern int *neighborhood(Graph *,int );
extern int numberVerticesActive(Graph *);
extern MFR *singular(Graph *, char * , int );
extern Graph *removeEdge(Graph *, int );
extern void splittingEdges(Graph *, int *);
extern void removeIsolates(Graph *);
extern int components(Graph *, int *);
extern void copyGraph(Graph *, Graph *);
void combineGraded(MFR *, MFR *);
extern MFR *checkDataBase(Graph *);
extern MFR *chordalComp(Graph *);
