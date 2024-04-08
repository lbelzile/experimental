* Encoding: UTF-8.

/* Liu (2024), data L22_E4 - mediation analysis */

GET FILE='C:\Users\11187439\Downloads\L22_E4.sav'. 
DATASET NAME L22E4 WINDOW=FRONT.

/* Linear model for response 1 */

UNIANOVA cutfreq BY socialbin WITH enjoyamount enjoyother 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PRINT=PARAMETER
  /CRITERIA=ALPHA(.05) 
  /DESIGN=socialbin enjoyamount enjoyother.

/* Linear model for 'parallel mediators' 1 and 2*/
 
UNIANOVA enjoyamount BY socialbin
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PRINT=PARAMETER
  /CRITERIA=ALPHA(.05) 
  /DESIGN=socialbin.

UNIANOVA enjoyother BY socialbin
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /PRINT=PARAMETER
  /CRITERIA=ALPHA(.05) 
  /DESIGN=socialbin.

/* PROCESS doesn't support variables names larger 8 char. */

RENAME VARIABLES (socialbin enjoyamount enjoyother = socbin enjoyam enjoyoth).

PROCESS y=cutfreq / x=socbin / m=enjoyam,enjoyoth / model=4 / normal=1 / boot=10000 /conf=95.

DATASET CLOSE L22E4.

* Example 2.

/* Data GSBE10 */
 GET FILE='C:\Users\11187439\Downloads\GSBE10.sav'. 
DATASET NAME GSBE10 WINDOW=FRONT.
/* PROCESS doesn't support variables names larger 8 char. */
RENAME VARIABLES (likeability protestint = liking protint).

UNIANOVA respeval BY protint WITH sexism
/METHOD=SSTYPE(3)
/INTERCEPT=INCLUDE
/EMMEANS=TABLES(protint) WITH(sexism=MEAN) COMPARE ADJ(LSD)
/PRINT=PARAMETER
/CRITERIA=ALPHA(.05)
/DESIGN=protint sexism protint*sexism.

/* Moderation analysis */
PROCESS y=respeval/ x=protint / w=sexism / model=1 / conf=95 / moments=1 / plot=1 / JN=1.

/* Mediated moderation */
PROCESS y=liking / x=protint /w=sexism / m=respeval / model=8 / conf=95 / moments=1 / boot=10000.

DATASET CLOSE GSBE10.

 /* Leckfor et al. (2023), data LWSH23_S3 - moderation analysis */
 
GET FILE='C:\Users\11187439\Downloads\LWSH23_S3.sav'. 
DATASET NAME LWSH23S3 WINDOW=FRONT.

GLM needsatis BY cond WITH needclosure
/METHOD=SSTYPE(3)
/INTERCEPT=INCLUDE
/PRINT=PARAMETER
/CRITERIA=ALPHA(.05)
/DESIGN=needclosure cond needclosure*cond.

RENAME VARIABLES (needsatis needclosure = nsatisf nclose).
PROCESS y = nsatisf / x=nclose / w=condind / model=1 / plot=1 / mcw=1.

DATASET CLOSE LWSH23S3.
