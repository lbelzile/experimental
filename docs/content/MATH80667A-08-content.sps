GET 
  FILE='C:\Users\11187439\Documents\SPSS\SSVB21_S2.sav'. 
DATASET NAME SSVB21S2 WINDOW=FRONT. 
DATASET ACTIVATE SSVB21S2.

GLM post BY condition WITH prior
  /METHOD=SSTYPE(2)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(condition) WITH(prior=MEAN) 
  /CONTRAST(condition)= SPECIAL(1 -1 0)
  /CONTRAST(condition)= SPECIAL(0.5 0.5 -1)
  /PRINT BP HOMOGENEITY 
  /BPDESIGN=condition
  /DESIGN=prior condition.
 
/* Scatterplot with common slope */
 GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=prior post condition
  /GRAPHSPEC SOURCE=INLINE 
  /FITLINE TOTAL=YES SUBGROUP=NO. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: prior=col(source(s), name("prior")) 
  DATA: post=col(source(s), name("post")) 
  DATA: condition=col(source(s), name("condition"), unit.category()) 
  GUIDE: axis(dim(1), label("prior")) 
  GUIDE: axis(dim(2), label("post")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("condition")) 
  SCALE: cat(aesthetic(aesthetic.color.interior), include( 
"1", "2", "3")) 
  ELEMENT: point(position(prior*post), color.interior(condition)) 
END GPL.
 
/* Test for parallel slopes */ 
GLM post BY condition WITH prior
  /METHOD=SSTYPE(2)
  /DESIGN=prior condition.

DATASET CLOSE SSVB21S2.

/* Example of moderation analysis */
GET 
  FILE='C:\Users\11187439\Documents\SPSS\GSBE10.sav'. 
DATASET NAME GSBE10 WINDOW=FRONT.
DATASET ACTIVATE GSBE10. 



EXAMINE VARIABLES=sexism 
  /PLOT NONE 
  /STATISTICS NONE 
  /PERCENTILES(25,50,75).


 UNIANOVA respeval BY protest WITH sexism 
  /METHOD=SSTYPE(2) 
  /EMMEANS=TABLES(protest) WITH(sexism=4.5) 
  /EMMEANS=TABLES(protest) WITH(sexism=5.12) 
  /EMMEANS=TABLES(protest) WITH(sexism=5.62)   
  /DESIGN=sexism protest sexism*protest.
  
GGRAPH 
  /GRAPHDATASET NAME="graphdataset" VARIABLES=protest sexism respeval
  /GRAPHSPEC SOURCE=INLINE 
  /FITLINE TOTAL=NO SUBGROUP=yes. 
BEGIN GPL 
  SOURCE: s=userSource(id("graphdataset")) 
  DATA: sexism=col(source(s), name("sexism")) 
  DATA: respeval=col(source(s), name("respeval")) 
  DATA: protest=col(source(s), name("protest"), unit.category()) 
  GUIDE: axis(dim(1), label("sexism")) 
  GUIDE: axis(dim(2), label("appropriateness of response")) 
  GUIDE: legend(aesthetic(aesthetic.color.interior), label("experimental condition")) 
   ELEMENT: point(position(sexism*respeval), color.interior(protest)) 
END GPL.

/* Same thing with the PROCESS MACRO */
PROCESS 
     y=respeval /
     x= protest/
     w=sexism /
     model=1/
     plot=1/
     boot=10000.


/* Dichotomize explanatory factor */
recode protest (1 = 1)(else = 2) into protestb.
add value labels protestb 1 'no protest' 2 'protest'.
variable labels protestb 'Experimental manipulation'.

PROCESS 
     y=respeval /
     x= protestb/
     w=sexism /
     model=1/
     plot=1/
     jn=1/
     boot=10000.

DATASET CLOSE GSBE10prote.
