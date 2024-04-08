* Encoding: UTF-8.
GET FILE='C:\Users\11187439\Desktop\SPSS\BJF14_S1.sav'. 
DATASET NAME BJF14S1 WINDOW=FRONT.

RENAME VARIABLES (condition groupsize=cond size).

GLM threat BY cond WITH gender size age
/METHOD=SSTYPE(3) 
/INTERCEPT=INCLUDE 
/PRINT=PARAMETER 
/CRITERIA=ALPHA(.05) 
/DESIGN=cond gender size age.

GLM bonding BY cond WITH threat gender size age
/METHOD=SSTYPE(3) 
/INTERCEPT=INCLUDE 
/PRINT=PARAMETER 
/CRITERIA=ALPHA(.05) 
/DESIGN=cond threat gender size age.

PROCESS 
     y=bonding /
     x=cond /
     m=threat /
     cov=gender size age /
     model=4/
     boot=10000.

DATASET CLOSE BJF14S1.
