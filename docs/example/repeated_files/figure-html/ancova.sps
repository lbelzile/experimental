/* One-way ANCOVA with BSJ92 data

GET 
  FILE='C:\Users\11187439\Documents\SPSS\BSJ92.sav'. 
DATASET NAME BSJ92 WINDOW=FRONT.

UNIANOVA posttest2 BY group WITH pretest2
  /EMMEANS=TABLES(group) WITH(pretest2=MEAN)
  /LMATRIX group 2 -1 -1
  /LMATRIX group 0 1 -1
  /CRITERIA=ALPHA(.05)
  /DESIGN=group pretest2.


DATASET CLOSE BSJ92.
