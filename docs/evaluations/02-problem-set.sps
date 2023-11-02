GET FILE="BJF14_S1.sav".

T-TEST
        GROUPS=condition(1 2)
        /VARIABLES=pain, unpleasant, bonding
        /CRITERIA=CI(.95)

ONEWAY /VARIABLES= bonding BY condition
	/STATISTICS=DESCRIPTIVES .


