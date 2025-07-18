$TITLE SAM balancing program. Standard CGE modeling system, Version 1.00
$ontext
Version 1.00 Release date: May 02, 2001
Last update: 

This SAM-balancing program is intended for the removal of rounding
errors, not for estimating SAMs with significant imbalances. For an
approach to SAM estimation that is appropriate in such contexts, see:

Robinson, Sherman and Moataz El-Said. "GAMS Code for Estimating a Social
Accounting Matrix (SAM) Using Cross Entropy Methods (CE)" Discussion
Paper 64, Trade and Macroeconomics Division, IFPRI, December 2000.

Robinson, Sherman, Andrea Cattaneo, and Moataz El-Said. "Updating and
Estimating a Social Account Matrix Using Cross Entropy Methods."
Economic Systems Research, Vol. 13, No. 1, 2001, pp. 47-64.
Also available as Discussion Paper 58, Trade and Macroeconomics Division,
IFPRI, August 2000.

In this file:

--the user has the option of removing small cells (with absolute values
below a cutoff point).
--the row/column balance of the SAM is checked.
--a SAM-balancing program is activated if the account imbalances in the SAM
exceed a cut-off point.
--the objective function of the SAM-balancing program is a cross-entropy
distance from the initial SAM coefficients for the entire matrix rather
than column sums. The method approximates the RAS procedure except that
in this case the only restrictions are that column and row sums be equal.
We do not assume known column sums.
--Negative entries are fixed and excluded from the balance procedure.
--Rows/columns with negative sums are also excluded and balanced by a
simple column adjustment.

$offtext

*CHECKING SAM BALANCE AND REMOVING SMALL CELLS=========================

 SET
  SMLCELL(AC,ACP)       SAM cells with abs value < cutoff are removed
 ;

 PARAMETER
  TDIFF(AC)      column minus row total for account AC in SAM
 ;
 SCALAR
  CUTOFF   lower bound on absolute cell values  /0.0/
 ;
*If the user does not want to remove small absolute values, then CUTOFF
*should be set at zero.
*If the user wants to remove small absolute values then CUTOFF should
*be set at a low positive value (e.g. 0.005).

 SAM('TOTAL',ACP) = 0;
 SAM(AC,'TOTAL')  = 0;

*ACNT-ACNTP is an element in the set SMLCELL if
*(1) they have a non-zero SAM cell; and
*(2) the absolute value of this cell is less than cutoff
 SMLCELL(ACNT,ACNTP)$(SAM(ACNT,ACNTP)$(ABS(SAM(ACNT,ACNTP))LT cutoff))=YES;

*The cells corresponding to elements in SMLCELL are set at zero.
 SAM(ACNT,ACNTP)$SMLCELL(ACNT,ACNTP) = 0;

 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 TDIFF(ACNT)       = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');

DISPLAY "After excluding small cells", TDIFF, SMLCELL, SAM ;

*Negative cells are handled by fixing the cell coefficient and
*excluding them from the adjustment process. If the row/column sum is
* negative, the adjustment is handled by a one-column RAS.

SETS
 ICOEFF(AC,ACP)   Cells to include in adjustment process
 IROW2(AC)        Row or column to include in process
 ;

*initialize as yes, then exclude cells
 ICOEFF(ACNT,ACNTP)     = YES ;
 IROW2(ACNT)            = YES ;

*Exclude cells for which row/column sums are zero or less

LOOP(ACNT$(SAM(ACNT,'TOTAL') LE 0),

 IROW2(ACNT)            = NO ;
 ICOEFF(ACNT,ACNTP)     = NO ;
 ICOEFF(ACNTP,ACNT)     = NO ;

 SAM(ACNTP,ACNT)$SAM(ACNT,'TOTAL') =
      SAM(ACNTP,ACNT)*SAM('TOTAL',ACNT)/SAM(ACNT,'TOTAL') ;

*end loop statement
 );

 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 TDIFF(ACNT)       = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');

 DISPLAY "After excluding zero sum rows", TDIFF, SAM, IROW2 ;


*SAM BALANCING PROGRAM==========================================
*This program assures that the SAM is perfectly balanced. Put an
*asterisk before the solve statement if you do not want to run it.
*Currently it runs if the sum of the absolute values of the account
*imbalances exceed an upper limit close to zero.

PARAMETER
 SUMABSDEV  sum of absolute values of column-row imbalances;
 SUMABSDEV = SUM(ACNT, ABS(tdiff(ACNT)));
DISPLAY SUMABSDEV;

PARAMETER
 COEFF0(AC,ACP)   Initial SAM coefficients
 ;
SCALAR
 TOTWGHT   total of all SAM cells
 delta     epsilon value for cross-entropy minimand            /.00001 /
 delta2    lower bound for coefficients based on cutoff value
  ;

 TOTWGHT              = SUM((ACNT,ACNTP), SAM(ACNT,ACNTP)) ;
 COEFF0(ACNT,ACNTP)   = SAM(ACNT,ACNTP)/TOTWGHT ;

 delta2 = (CUTOFF/TOTWGHT) ;

*Exclude all cells which are zero or negative.
 ICOEFF(ACNT,ACNTP)$(COEFF0(ACNT,ACNTP) LE 0) = NO ;

VARIABLES
 CENTROPY      Cross entropy measure
 COEFF(AC,ACP) SAM coefficients which sum to one over entire matrix
 ;

*Initialize variables
 COEFF.L(ACNT, ACNTP) = COEFF0(ACNT,ACNTP);
 CENTROPY.L           = 0 ;

EQUATIONS
 ENTROPYEQ   Cross entropy objective function
 COLROWBAL   column total equals row total
 SUMCFEQ     Total sum  of coefficients
 ;

 ENTROPYEQ..
 CENTROPY =E= SUM((ACNT,ACNTP)$ICOEFF(acnt,acntp),
                 COEFF(acnt,acntp)*(log(COEFF(acnt,acntp) + delta)
                 - log(COEFF0(acnt,acntp) + delta))) ;

 COLROWBAL(ACNT)$IROW2(ACNT)..
  SUM(ACNTP, COEFF(ACNTP,ACNT)) =E= SUM(ACNTP, COEFF(ACNT,ACNTP));

 SUMCFEQ..
  SUM((ACNT,ACNTP), COEFF(ACNT,ACNTP)) =E= 1 ;


 MODEL
  SAMBAL SAM balancing program for removing rounding errors
  /
   ENTROPYEQ
   COLROWBAL
   SUMCFEQ
  /;


*Fix cells not to be changed.
*After balancing, no unfixed cell will have value less than cutoff.

 COEFF.LO(ACNT,ACNTP)$ICOEFF(ACNT,ACNTP)       = delta2 ;
 COEFF.FX(ACNT,ACNTP)$(NOT ICOEFF(ACNT,ACNTP)) = COEFF0(ACNT,ACNTP) ;

 OPTION LIMROW=0, LIMCOL=0;
 SAMBAL.HOLDFIXED = 1;
 OPTION NLP=MINOS5;
*OPTION NLP=CONOPT2;

IF(SUMABSDEV GT 0.000001,
  SOLVE SAMBAL MINIMIZING CENTROPY USING NLP;
);

*Reload new SAM

 SAM(ACNT,ACNTP)   = coeff.l(ACNT,ACNTP)*TOTWGHT ;

 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));
 TDIFF(ACNT)       = SAM('TOTAL',ACNT) - SAM(ACNT,'TOTAL');
*??
OPTION SAM:8;
 DISPLAY "Balanced SAM after SAMBAL", TDIFF, SAM ;
OPTION SAM:3;