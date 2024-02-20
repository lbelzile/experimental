GET 
  FILE='C:\Users\11187439\Documents\SPSS\popcorn.sav'. 
DATASET NAME popcorn WINDOW=FRONT.

UNIANOVA percentage BY brand power time
 /DESIGN brand power time brand*power brand*time power*time brand*power*time
 /PLOT PROFILE(brand*time*power)
 /EMMEANS=TABLES(brand) compare(brand) /* comparing between brands, assuming no interaction (INCORRECT HERE, for the sake of demonstration
.

/* For pairwise comparisons, create a variable for power and time

IF (power=1 AND time=1) powtime=1.
IF (power=2 AND time=1) powtime=2.
IF (power=1 AND time=2) powtime=3.
IF (power=2 AND time=2) powtime=4.
IF (power=1 AND time=3) powtime=5.
IF (power=2 AND time=3) powtime=6.

VALUE LABELS powtime
   1 '4m 500W' 
   2 '4m 600W'
   3 '4m30 500W'
   4 '4m30 600W'
   5 '5m 500W'
   6 '5m 600W'.
EXECUTE.


UNIANOVA percentage BY brand powtime
 /DESIGN brand powtime brand*powtime
 /EMMEANS=TABLES(powtime) compare(powtime) /*Only adjustments available are Sidak and Bonferroni...
.

/* For specific contrasts, fit the model with the three-way interaction, removing main effects (only ANOVA F-test table is off)

UNIANOVA percentage BY brand power time
 /DESIGN brand*power*time
 /EMMEANS=TABLES(brand*power*time)
  /LMATRIX brand*power*time 1 1 1 -1 -1 -1 1 1 1 -1 -1 -1 1 1 1 -1 -1 -1 /* 500W vs 600W
  /LMATRIX brand*power*time 0 1 -1 0 1 -1 0 -1 1 0 -1 1 0 0 0 0 0 0  /* 4.5min vs 5, for nat 1 vs 2 (interaction)
   /LMATRIX brand*power*time 1 -1 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0; brand*power*time 1 0 -1 1 0 -1  0 0 0 0 0 0 0 0 0 0 0 0  /* main effect of time by brand (national 1)
  /LMATRIX brand*power*time  0 0 0 0 0 0 1 -1 0 1 -1 0 0 0 0 0 0 0; brand*power*time  0 0 0 0 0 0 1 0 -1 1 0 -1 0 0 0 0 0 0  /* main effect of time by brand (national 2)
  /LMATRIX brand*power*time  0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 1 -1 0; brand*power*time  0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 1 0 -1  /* main effect of time by brand (local)
.

DATASET CLOSE popcorn.