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





GET FILE='C:\Users\11187439\Desktop\SPSS\HOSM22_E3.sav'. 
DATASET NAME HOSM22E3 WINDOW=FRONT. 
SORT CASES BY id ratingtype. 
CASESTOVARS 
  /ID=id 
  /INDEX=ratingtype 
  /GROUPBY=VARIABLE.
RENAME VARIABLES (=experience prediction)

GLM experience prediction BY waiting 
  /WSFACTOR=rating 2
  /PLOT=PROFILE(waiting*rating) TYPE=LINE ERRORBAR=CI MEANREFERENCE=YES YAXIS=AUTO 
  /EMMEANS=TABLES(rating) 
  /EMMEANS=TABLES(waiting) 
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05) 
  /WSDESIGN=rating 
  /DESIGN=waiting.
