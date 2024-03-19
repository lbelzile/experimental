
GET FILE='C:\Users\11187439\Desktop\SPSS\HOSM22_E3.sav'. 
DATASET NAME HOSM22E3 WINDOW=FRONT.

MIXED imscore BY waiting ratingtype 
  /CRITERIA=DFMETHOD(KENWARDROGER) 
  /FIXED=waiting ratingtype waiting*ratingtype | SSTYPE(3) 
  /METHOD=REML 
  /PRINT=CORB COVB  SOLUTION 
  /RANDOM=INTERCEPT | SUBJECT(id)  COVTYPE(VC) 
  /EMMEANS=TABLES(waiting) COMPARE ADJ(LSD) 
  /EMMEANS=TABLES(ratingtype) COMPARE ADJ(LSD) 
  /EMMEANS=TABLES(waiting*ratingtype).

DATASET CLOSE HOSM22E3.

GET FILE='C:\Users\11187439\Desktop\SPSS\HB22_S5.sav'. 
DATASET NAME HB22S5 WINDOW=FRONT.

MIXED likelihood BY curstate predout tempdist 
  /CRITERIA=DFMETHOD(SATTERTHWAITE) 
  /FIXED=curstate predout tempdist curstate*predout curstate*tempdist predout*tempdist 
    curstate*predout*tempdist | SSTYPE(3) 
  /METHOD=REML 
  /PRINT=SOLUTION 
  /RANDOM=INTERCEPT | SUBJECT(id).

DATASET CLOSE HB22S5.
