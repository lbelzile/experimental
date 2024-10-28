* Encoding: UTF-8.
GET 
  FILE='C:\Users\11187439\Documents\SPSS\AA21.sav'. 
DATASET NAME AA21 WINDOW=FRONT.

/* Aggregate to get one average response per subcondition and id */
  
DATASET DECLARE AA21m. 
AGGREGATE 
  /OUTFILE='AA21m' 
  /BREAK=id stimulus 
  /latency_mean=MEAN(latency).

/* Convert from wide to long format */
  
SORT CASES BY id stimulus. 
CASESTOVARS 
  /ID=id 
  /INDEX=stimulus 
  /GROUPBY=VARIABLE.
RENAME VARIABLES(latency_mean.1 latency_mean.2 latency_mean.3 = real GAN1 GAN2).

GLM real GAN1 GAN2
  /WSFACTOR=stimulus 3 
  /EMMEANS=TABLES(stimulus) COMPARE(stimulus)
  /PRINT=DESCRIPTIVE 
  /WSDESIGN=stimulus.
