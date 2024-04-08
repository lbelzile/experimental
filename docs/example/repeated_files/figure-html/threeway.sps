/* Three-way ANOVA with 'words' data

GET 
  FILE='C:\Users\11187439\Documents\SPSS\words.sav'. 
DATASET NAME words WINDOW=FRONT.
 
/* Interaction plot
 
GRAPH 
  /LINE(MULTIPLE)=MEAN(words) BY feedback BY material 
  /PANEL ROWVAR=age ROWOP=CROSS.

UNIANOVA words BY feedback age material
  /DESIGN= feedback age material age*feedback age*material feedback*material age*feedback*material
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(feedback)
  /CONTRAST(feedback)=SPECIAL(1 -0.5 -0.5) /*feedback: none vs AVG(pos, neg)
  /EMMEANS = TABLES(age*material) COMPARE(material) ADJ(LSD)
  /CRITERIA=ALPHA(.05).



DATASET ACTIVATE words.
IF (feedback=1 and material=1 and age=1) treatment=1.
IF (feedback=1 and material=1 and age=2) treatment=2.
IF (feedback=1 and material=2 and age=1) treatment=3.
IF (feedback=1 and material=2 and age=2) treatment=4.
IF (feedback=1 and material=3 and age=1) treatment=5.
IF (feedback=1 and material=3 and age=2) treatment=6.
IF (feedback=2 and material=1 and age=1) treatment=7.
IF (feedback=2 and material=1 and age=2) treatment=8.
IF (feedback=2 and material=2 and age=1) treatment=9.
IF (feedback=2 and material=2 and age=2) treatment=10.
IF (feedback=2 and material=3 and age=1) treatment=11.
IF (feedback=2 and material=3 and age=2) treatment=12.
IF (feedback=3 and material=1 and age=1) treatment=13.
IF (feedback=3 and material=1 and age=2) treatment=14.
IF (feedback=3 and material=2 and age=1) treatment=15.
IF (feedback=3 and material=2 and age=2) treatment=16.
IF (feedback=3 and material=3 and age=1) treatment=17.
IF (feedback=3 and material=3 and age=2) treatment=18.
EXECUTE.

VALUE LABELS treatment
   1 'none - low freq low emotion - fifth grader' 
   2 'none - low freq low emotion - senior' 
   3 'none - high freq low emotion - fifth grader' 
   4 'none - high freq low emotion - senior' 
   5 'none - high freq high emotion - fifth grader' 
   6 'none - high freq high emotion - senior' 
   7 'positive - low freq low emotion - fifth grader' 
   8 'positive -  low freq low emotion - senior' 
   9 'positive - high freq low emotion - fifth grader' 
   10 'positive - high freq low emotion - senior' 
   11 'positive - high freq high emotion - fifth grader' 
   12 'positive - high freq high emotion -senior' 
   13 'negative - low freq low emotion - fifth grader' 
   14 'negative - low freq low emotion - senior' 
   15 'negative - high freq low emotion - fifth grader' 
   16 'negative - high freq low emotion - senior' 
   17 'negative -  high freq high emotion - fifth grader' 
   18 'negative - high freq high emotion - senior'.
EXECUTE.


/* First two LMATRIX are simple contrasts (AVG low emotion vs high emotion) for each age group, averaging across feedback 
/* Third call compares both high freqs, between age groups (interaction component)
/* Fourth call compares none vs feedback for b2 (high freq, low emotion) vs b3  (high freq, high emotion)
/* Fifth call looks at the difference to fourth call between age groups
/*
UNIANOVA words BY treatment
  /DESIGN= treatment
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /LMATRIX "Contrast 1a: low vs high emotion (fifth grader)" treatment -1/6 0 -1/6 0 1/3 0 -1/6 0 -1/6 0 1/3 0 -1/6 0 -1/6 0 1/3 0
  /LMATRIX "Contrast 1b: low vs high emotion (senior)" treatment  0 -1/6 0 -1/6 0 1/3 0 -1/6 0 -1/6 0 1/3 0 -1/6 0 -1/6 0 1/3
  /LMATRIX "Contrast 2: high freq, between age groups (interaction)" treatment  0 0 -1/6 1/6 1/6 -1/6 0 0 -1/6 1/6 1/6 -1/6 0 0 -1/6 1/6 1/6 -1/6
  /LMATRIX "Contrast 3: feedback (yes/none) for b2 vs b3" treatment  0 0 -2 -2 2 2 0 0 1 1 -1 -1 0 0 1 1 -1 -1
  /LMATRIX "Contrast 4: difference of C3 between age groups" treatment  0 0 -1 1 1 -1 0 0 1/2 -1/2 -1/2 1/2 0 0 1/2 -1/2 -1/2 1/2
  /LMATRIX "Contrast 5: feedback vs average" treatment 1 1 1 1 1 1 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2 -1/2
  /LMATRIX "Joint test of material for age c1 (fifth grader)" treatment  1 0 -1 0 0 0 1 0 -1 0 0 0 1 0 -1 0 0 0; treatment 1 0 0 0 -1 0 1 0 0 0 -1 0 1 0 0 0 -1 0 
  /LMATRIX "Joint test of material for age c2 (senior)" treatment  0 1 0 -1 0 0 0 1 0 -1 0 0 0 1 0 -1 0 0 ; treatment 0 1 0 0 0 -1 0 1 0 0 0 -1 0 1 0 0 0 -1 
  /CRITERIA=ALPHA(.05).

DATASET CLOSE words.
