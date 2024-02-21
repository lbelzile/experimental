GET 
  FILE='C:\Users\11187439\Documents\SPSS\C16.sav'. 
DATASET NAME C16 WINDOW=FRONT.

UNIANOVA madjust BY anchor magnitude
  /DESIGN anchor magnitude anchor*magnitude
  /EMMEANS = TABLES(anchor) compare(anchor) /* Marginal means, and pairwise comparisons overall for anchor
  /EMMEANS = TABLES(magnitude)
  /EMMEANS = TABLES(anchor*magnitude) compare(anchor)  /* Marginal means, and pairwise comparisons for anchor separately for each magnitude
  /PLOT = PROFILE(anchor*magnitude) /* interaction plot
  /PRINT DESCRIPTIVE HOMOGENEITY.
 
DATASET CLOSE C16.

GET 
  FILE='C:\Users\11187439\Documents\SPSS\MP14_S1.sav'. 
DATASET NAME MP14S1 WINDOW=FRONT.


DATASET ACTIVATE MP14S1.
IF (station=1 and direction=1) treatment=1.
IF (station=2 and direction=1) treatment=2.
IF (station=3 and direction=1) treatment=3.
IF (station=4 and direction=1) treatment=4.
IF (station=1 and direction=2) treatment=5. 
IF (station=2 and direction=2) treatment=6.
IF (station=3 and direction=2) treatment=7.
IF (station=4 and direction=2) treatment=8.
EXECUTE.

UNIANOVA distance BY direction station
  /METHOD=SSTYPE(3) /* Default option
  /DESIGN direction station direction*station
  /PRINT DESCRIPTIVE HOMOGENEITY.
 
UNIANOVA distance BY treatment
   /DESIGN treatment
   /CONTRAST(treatment) SPECIAL(-1 2 0 0 0 0 2 -1) /* H3: CONTRAST results are displayed before LMATRIX
   /CONTRAST(treatment) SPECIAL(0 0 2 -1 -1 2 0 0) /* H4
   /LMATRIX = "Joint test of symmetry" treatment 1 0 0 0 0 0 0 -1 ; treatment 0 0 1 0 0 -1 0 0; treatment 0 1 0 0 0 0 -1 0; treatment 0 0 0 1 -1 0 0 0
   /LMATRIX = "H1" treatment  0 1 -1 0 0 -1 1 0
   /LMATRIX = "H2" treatment 1 0 0 -1 -1 0 0 1.

DATASET CLOSE MP14S1.
