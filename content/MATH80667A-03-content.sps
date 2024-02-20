* Encoding: windows-1252.
** Analysis of the "arithmetic" data.

GET FILE='C:\Users\11187439\Documents\SPSS\arithmetic.sav'.

** Compute pairwise difference.
ONEWAY score BY group
  /STATISTICS HOMOGENEITY
  /CRITERIA=CILEVEL(0.95).
