GET FILE='C:\Users\11187439\Desktop\SPSS\C22_E3.sav'. 
DATASET NAME C22 WINDOW=FRONT.

MIXED guilt BY anchor vignette verdictsyst WITH prior
  /CRITERIA=DFMETHOD(SATTERTHWAITE)
  /FIXED=anchor vignette verdictsyst anchor*vignette anchor*verdictsyst vignette*verdictsyst anchor*vignette*verdictsyst prior | SSTYPE(3) 
  /METHOD=REML 
  /PRINT=SOLUTION 
  /RANDOM=INTERCEPT | SUBJECT(id) COVTYPE(VC).


DATASET CLOSE C22.


GET FILE='C:\Users\11187439\Desktop\SPSS\chocolate.sav'. 
DATASET NAME chocolate WINDOW=FRONT.


MIXED y BY choc background 
  /CRITERIA=DFMETHOD(KENWARDROGER) 
  /FIXED=choc background choc*background | SSTYPE(3) 
  /METHOD=REML 
  /RANDOM=INTERCEPT choc | SUBJECT(rater*background) COVTYPE(VC).


DATASET CLOSE chocolate.