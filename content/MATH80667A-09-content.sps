GET 
  FILE='C:\Users\11187439\Desktop\SPSS\AVC02.sav'. 
DATASET NAME AVC02 WINDOW=FRONT. 
 
GLM prime debt profitability BY format 
  /METHOD=SSTYPE(3) 
  /PRINT=DESCRIPTIVE HOMOGENEITY 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=format.
  
 DATASET CLOSE AVC02.
 
GET FILE='C:\Users\11187439\Desktop\SPSS\AA21.sav'.
DATASET NAME AA21 WINDOW=FRONT. 
DATASET ACTIVATE AA21. 
 
DATASET DECLARE AA21m.
AGGREGATE 
  /OUTFILE='AA21m' 
  /BREAK=stimuli id 
  /latency=MEAN(latency). 
DATASET CLOSE AA21. 
DATASET ACTIVATE AA21m.

UNIANOVA latency BY stimuli id 
  /RANDOM=id 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /POSTHOC=stimuli(TUKEY) 
  /EMMEANS=TABLES(stimuli)
  /PRINT ETASQ
  /CRITERIA=ALPHA(.05) 
  /DESIGN=stimuli id.
