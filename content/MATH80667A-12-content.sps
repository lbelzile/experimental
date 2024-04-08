/* Data GSBE10 */
 
UNIANOVA respeval BY protestint WITH sexism
/METHOD=SSTYPE(3)
/INTERCEPT=INCLUDE
/EMMEANS=TABLES(protest1) WITH(sexism=MEAN) COMPARE ADJ(LSD)
/PRINT=PARAMETER
/CRITERIA=ALPHA(.05)
/DESIGN=protestint sexism protestint*sexism.

/* Moderation analysis */

PROCESS y=respeval/ x=protestint / w=sexism / model=1 / conf=95 / moments=1 / plot=1 / JN=1.

/* Moderated mediation */
PROCESS y=likeability / x=protestind /w=sexism / m=respeval / model=8 / conf=95 / moments=1 / boot=10000.



 /* Leckfor et al. (2023), data LWSH23_S3 - moderation analysis */
 
GLM needsatis BY cond WITH needclosure
/METHOD=SSTYPE(3)
/INTERCEPT=INCLUDE
/PRINT=PARAMETER
/CRITERIA=ALPHA(.05)
/DESIGN=needclosure cond needclosure*cond.


PROCESS y = needsatis / x=needclosure / w=condind / model=1 / plot=1 / mcw=1.

/* Liu (2024), data L22_E4 - mediation analysis */

PROCESS y=cutfreq / x=socialbin / m=enjoyamount,enjoyother / model=4 / normal=1 / boot=10000 /conf=95.


