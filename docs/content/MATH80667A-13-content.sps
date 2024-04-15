GET 
  FILE='C:\Users\11187439\Desktop\SPSS\DA23_E2.sav'. 
DATASET NAME DA23E2 WINDOW=FRONT. 
CROSSTABS 
  /TABLES=format BY purchased 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ PHI 
  /CELLS=COUNT EXPECTED 
  /COUNT ROUND CELL.
 
DATASET CLOSE DA23E2.

GET FILE='C:\Users\11187439\Desktop\SPSS\MULTI21_D1.sav'. 
DATASET NAME ELLIOT21 WINDOW=FRONT. 

/* Aggregate results over all labs */
DATASET DECLARE contingency. 
AGGREGATE 
  /OUTFILE='contingency' 
  /BREAK=age frequency 
  /total=SUM(count).
DATASET ACTIVATE contingency. 

* Fit Poisson regression model.
GENLIN total BY age frequency (ORDER=ASCENDING) 
  /MODEL age frequency INTERCEPT=YES 
 DISTRIBUTION=POISSON LINK=LOG 
  /CRITERIA ANALYSISTYPE=3(LR) CILEVEL=95 CITYPE=PROFILE(.0001) LIKELIHOOD=FULL 
  /PRINT FIT. 
 *Only returns the Pearson X2 (score) and deviance (LRT) statistics.

WEIGHT BY total.  
CROSSTABS 
  /TABLES=frequency BY age 
  /FORMAT=AVALUE TABLES 
  /STATISTICS=CHISQ PHI 
  /CELLS=COUNT EXPECTED 
  /COUNT ASIS.
DATASET CLOSE contingency.
DATASET CLOSE ELLIOT21.

GET 
  FILE='C:\Users\11187439\Desktop\SPSS\BM04_T2.sav'. 
DATASET NAME BM04T2 WINDOW=FRONT.
* Create symmetry factor.
* Note that labels != numerical values, so "0" = 1, "1" = 2, "2" = 3.
STRING sym(A8).
DO IF (black=1 and white=1).
COMPUTE sym = "00".
ELSE IF (black=2 and white=2).
COMPUTE sym = "11".
ELSE IF (black=3 and white=3).
COMPUTE sym = "22".
ELSE IF (black=2 and white=1).
COMPUTE sym = "01".
ELSE IF (black=1 and white=2).
COMPUTE sym = "01".
ELSE IF (black=2 and white=3).
COMPUTE sym = "12".
ELSE IF (black=3 and white=2).
COMPUTE sym = "12".
ELSE.
COMPUTE sym="02".
END IF.
EXECUTE.

 /*Goodness-of-fit tables returns statistics and p-values, EMMEANS the expected counts  */
GENLIN count BY sym (ORDER=ASCENDING)
  /MODEL sym INTERCEPT=YES
 DISTRIBUTION=POISSON LINK=LOG
  /CRITERIA ANALYSISTYPE=3(LR) CILEVEL=95 CITYPE=PROFILE(.0001) LIKELIHOOD=FULL
  /EMMEANS TABLES=sym SCALE=ORIGINAL
  /PRINT FIT.

DATASET CLOSE BM04T2.


/*****************************************************************/
GET 
  FILE='C:\Users\11187439\Desktop\SPSS\BL22_E.sav'. 
DATASET NAME BL22E WINDOW=FRONT.

*Nonparametric Tests: Independent Samples. 
NPTESTS 
  /INDEPENDENT TEST (partner_time) GROUP (cond) MANN_WHITNEY HODGES_LEHMANN 
  /CRITERIA ALPHA=0.05  CILEVEL=95.
* Compare results with two-sample t-test.
T-TEST GROUPS=cond(1 2) 
  /VARIABLES=partner_time 
  /CRITERIA=CI(.95).
  
DATASET CLOSE  BL22E.
  
GET 
  FILE='C:\Users\11187439\Desktop\SPSS\BRLS21_T3.sav'. 
DATASET NAME BRLS21T3 WINDOW=FRONT. 

SORT CASES BY id task. 
CASESTOVARS 
  /ID=id 
  /INDEX=task 
  /GROUPBY=VARIABLE.

* Friedman test.
NPTESTS 
  /RELATED TEST(nviolation.1 nviolation.2 nviolation.3 nviolation.4) FRIEDMAN(COMPARE=PAIRWISE) 
  /MISSING SCOPE=ANALYSIS USERMISSING=EXCLUDE 
  /CRITERIA ALPHA=0.05  CILEVEL=95.
  
* Wilcoxon signed-rank test 
NPTESTS 
  /RELATED TEST(nviolation.1 nviolation.2) WILCOXON HODGES_LEHMAN 
  /CRITERIA ALPHA=0.05  CILEVEL=95.

NPTESTS 
  /RELATED TEST(nviolation.1 nviolation.3) WILCOXON HODGES_LEHMAN 
  /CRITERIA ALPHA=0.05  CILEVEL=95.

NPTESTS 
  /RELATED TEST(nviolation.1 nviolation.4) WILCOXON HODGES_LEHMAN 
  /CRITERIA ALPHA=0.05  CILEVEL=95.

NPTESTS 
  /RELATED TEST(nviolation.2 nviolation.3) WILCOXON HODGES_LEHMAN 
  /CRITERIA ALPHA=0.05  CILEVEL=95.

NPTESTS 
  /RELATED TEST(nviolation.2 nviolation.4) WILCOXON HODGES_LEHMAN 
  /CRITERIA ALPHA=0.05  CILEVEL=95.

NPTESTS 
  /RELATED TEST(nviolation.3 nviolation.4) WILCOXON HODGES_LEHMAN 
  /CRITERIA ALPHA=0.05  CILEVEL=95.

  DATASET CLOSE BBRLS21T3.