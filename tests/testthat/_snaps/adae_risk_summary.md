# Standard Inputs work

    Code
      output
    Output
      # A tibble: 58 x 24
         DPTVAL    PVALUE ADJPVALUE  RISK RISKCIL RISKCIU RISK_CI TRTPAIR TRTVAR  FREQ
         <chr>      <dbl>     <dbl> <dbl>   <dbl>   <dbl> <chr>   <chr>   <fct>  <dbl>
       1 INJURY, ~ 0.591     0.591  0.672    0.16    2.9  0.672 ~ Placeb~ Place~     4
       2 INJURY, ~ 0.591     0.591  0.672    0.16    2.9  0.672 ~ Placeb~ Xanom~     3
       3 MUSCULOS~ 0.452     0.452  1.57     0.48    5.13 1.568 ~ Placeb~ Place~     4
       4 MUSCULOS~ 0.452     0.452  1.57     0.48    5.13 1.568 ~ Placeb~ Xanom~     7
       5 RENAL AN~ 0.331     0.331  0.448    0.08    2.37 0.448 ~ Placeb~ Place~     4
       6 RENAL AN~ 0.331     0.331  0.448    0.08    2.37 0.448 ~ Placeb~ Xanom~     2
       7 METABOLI~ 0.0707    0.0707 0.179    0.02    1.5  0.179 ~ Placeb~ Place~     5
       8 METABOLI~ 0.0707    0.0707 0.179    0.02    1.5  0.179 ~ Placeb~ Xanom~     1
       9 NERVOUS ~ 0.0521    0.0521 2.18     0.96    4.93 2.176 ~ Placeb~ Place~     7
      10 NERVOUS ~ 0.0521    0.0521 2.18     0.96    4.93 2.176 ~ Placeb~ Xanom~    17
      # i 48 more rows
      # i 14 more variables: PCT <dbl>, TOTAL_N <dbl>, HOVER_PCT <chr>,
      #   HOVER_RISK <chr>, HOVER_TEXT <chr>, DPTVAR <chr>, CN <chr>, DPTVARN <int>,
      #   DPTVALN <dbl>, SUBGRPVARX <chr>, SUBGRPVARXN <dbl>,
      #   `Risk Ratio (CI)` <chr>, `P-value` <dbl>, CVALUE <chr>

---

    Code
      out_table
    Output
      # A tibble: 29 x 8
         DPTVAL          DPTVARN DPTVALN CN    `Placebo_n (%) ` Xanomeline Low Dose_~1
         <chr>             <int>   <dbl> <chr> <chr>            <chr>                 
       1 "INJURY, POISO~       1       0 C     4 (5.8%)         3 (3.9%)              
       2 "MUSCULOSKELET~       2       0 C     4 (5.8%)         7 (9.09%)             
       3 "RENAL AND URI~       3       0 C     4 (5.8%)         2 (2.6%)              
       4 "METABOLISM AN~       4       0 C     5 (7.25%)        1 (1.3%)              
       5 "NERVOUS SYSTE~       5       0 C     7 (10.14%)       17 (22.08%)           
       6 "\t\t\tDizzine~       5       1 C     2 (2.9%)         6 (7.79%)             
       7 "RESPIRATORY, ~       6       0 C     7 (10.14%)       9 (11.69%)            
       8 "\t\t\tCough"         6       1 C     1 (1.45%)        5 (6.49%)             
       9 "INVESTIGATION~       7       0 C     8 (11.59%)       5 (6.49%)             
      10 "CARDIAC DISOR~       8       0 C     9 (13.04%)       10 (12.99%)           
      # i 19 more rows
      # i abbreviated name: 1: `Xanomeline Low Dose_n (%)  `
      # i 2 more variables: `Risk Ratio (CI)` <chr>, `P-value` <dbl>

