GET 
  FILE='C:\Users\11187439\Documents\SPSS\GK14_S3.sav'. 
DATASET NAME GK14S3 WINDOW=FRONT.

IF (condition=1 OR condition=2) target=0.
IF (condition=3 OR condition=4) target=1.
IF (condition=1 OR condition=3) distance=0.
IF (condition=2 OR condition=4) distance=1.

VALUE LABELS target
   0 'self' 
   1 'other'. 
VALUE LABELS distance
   0 'immersed' 
   1 'distanced'.
EXECUTE.   

/* To get results for multiple response variables at once, use "UNIANOVA limits compr persp change BY target distance"
    
UNIANOVA change BY target distance
    /PLOT profile(target*distance)
    /DESIGN target distance target*distance
    /EMMEANS = TABLES(target*distance)
    /EMMEANS(distance) compare(distance) /* marginal effects and pairwise contrasts
    .

UNIANOVA change BY condition
    /DESIGN condition
    /CONTRAST(condition) SPECIAL(0 -1 0.5 0.5) 
    /CONTRAST(condition) SPECIAL(-1 0 0.5 0.5)
    /CONTRAST(condition) SPECIAL(0 0 -1 1)
    /CONTRAST(condition) SPECIAL(1 -1 0 0)
    .
 
   
DATASET CLOSE GK14S3.


GET 
  FILE='C:\Users\11187439\Documents\SPSS\teller.sav'. 
DATASET NAME teller WINDOW=FRONT. 

UNIANOVA error BY course nweeks 
  /DESIGN=course nweeks course*nweeks
  /EMMEANS = TABLES(course*nweeks) COMPARE(course) ADJ(SIDAK)
.

DATASET CLOSE teller.