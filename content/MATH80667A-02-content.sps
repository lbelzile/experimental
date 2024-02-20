* Encoding: windows-1252.
** Analysis of the "arithmetic" data.

GET FILE='C:\Users\11187439\Documents\SPSS\arithmetic.sav'.

** Summary statistics: mean and standard deviations.
MEANS TABLES=score BY group
  /CELLS=MEAN STDDEV .

** Create boxplot.
EXAMINE VARIABLES=score BY group 
  /PLOT=BOXPLOT 
  /STATISTICS=NONE 
  /NOTOTAL.

** Compute pairwise difference.
ONEWAY score BY group
  /POLYNOMIAL=1
  /CONTRAST=0 0 1 -1 0
  /CRITERIA=CILEVEL(0.95).
