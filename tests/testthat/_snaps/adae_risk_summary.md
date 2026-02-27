# Test Case 1: adae_summary with standard inputs works

    Code
      ae_risk
    Output
      # A tibble: 48 x 30
         TRTVAR       DPTVAR DPTVAL CVALUE DENOMN  FREQ   PCT CPCT  XVAR  CN    PVALUE
         <ord>        <chr>  <chr>  <chr>   <dbl> <dbl> <dbl> <chr> <chr> <chr>  <dbl>
       1 Placebo      TIER   CARDI~ 1 ( 1~     62     1  1.61 " 1.~ CARD~ C     0.236 
       2 Placebo      TIER   RESPI~ 1 ( 1~     62     1  1.61 " 1.~ RESP~ C     0.141 
       3 Xanomeline ~ TIER   CARDI~ 4 ( 5~     73     4  5.48 " 5.~ CARD~ C     0.236 
       4 Xanomeline ~ TIER   RESPI~ 5 ( 6~     73     5  6.85 " 6.~ RESP~ C     0.141 
       5 Placebo      TIER   NERVO~ 2 ( 3~     62     2  3.23 " 3.~ NERV~ C     0.221 
       6 Xanomeline ~ TIER   NERVO~ 6 ( 8~     73     6  8.22 " 8.~ NERV~ C     0.221 
       7 Placebo      TIER   INFEC~ 8 (12~     62     8 12.9  "12.~ INFE~ C     0.235 
       8 Xanomeline ~ TIER   INFEC~ 5 ( 6~     73     5  6.85 " 6.~ INFE~ C     0.235 
       9 Placebo      TIER   GASTR~ 9 (14~     62     9 14.5  "14.~ GAST~ C     0.0342
      10 Xanomeline ~ TIER   GASTR~ 3 ( 4~     73     3  4.11 " 4.~ GAST~ C     0.0342
      # i 38 more rows
      # i 19 more variables: RISK <dbl>, RISKCIL <dbl>, RISKCIU <dbl>,
      #   ADJPVALUE <dbl>, RISK_CI <chr>, TRTPAIR <chr>, CTRL <chr>, ACTIVE <chr>,
      #   TOTAL_N <dbl>, DPTVARN <int>, DPTVALN <dbl>, SUBGRPVARX <chr>,
      #   SUBGRPVARXN <dbl>, `Risk Difference (CI)` <chr>, `Risk Difference` <dbl>,
      #   `P-value` <dbl>, `Lower Limit` <dbl>, `Upper Limit` <dbl>,
      #   `(Lower-Upper)` <chr>

# Test Case 2: adae_summary with summary row

    Code
      ae_risk
    Output
      # A tibble: 48 x 32
         TRTVAR DPTVAR DPTVAL CVALUE DENOMN  FREQ   PCT CPCT  XVAR  CN    PVALUE  RISK
         <ord>  <chr>  <chr>  <chr>   <dbl> <dbl> <dbl> <chr> <chr> <chr>  <dbl> <dbl>
       1 Place~ TIER   CARDI~ 1 ( 1~     62     1  1.61 " 1.~ CARD~ C     0.466  3.40 
       2 Xanom~ TIER   CARDI~ 4 ( 5~     73     4  5.48 " 5.~ CARD~ C     0.466  3.40 
       3 Place~ TIER   GASTR~ 9 (14~     62     9 14.5  "14.~ GAST~ C     0.0697 0.283
       4 Xanom~ TIER   GASTR~ 3 ( 4~     73     3  4.11 " 4.~ GAST~ C     0.0697 0.283
       5 Place~ TIER   GENER~ 16 (2~     62    16 25.8  "25.~ GENE~ C     0.0034 2.02 
       6 Xanom~ TIER   GENER~ 38 (5~     73    38 52.1  "52.~ GENE~ C     0.0034 2.02 
       7 Place~ TIER   INFEC~ 8 (12~     62     8 12.9  "12.~ INFE~ C     0.370  0.531
       8 Xanom~ TIER   INFEC~ 5 ( 6~     73     5  6.85 " 6.~ INFE~ C     0.370  0.531
       9 Place~ TIER   NERVO~ 2 ( 3~     62     2  3.23 " 3.~ NERV~ C     0.390  2.55 
      10 Xanom~ TIER   NERVO~ 6 ( 8~     73     6  8.22 " 8.~ NERV~ C     0.390  2.55 
      # i 38 more rows
      # i 20 more variables: RISKCIL <dbl>, RISKCIU <dbl>, ADJPVALUE <dbl>,
      #   RISK_CI <chr>, TRTPAIR <chr>, CTRL <chr>, ACTIVE <chr>, TOTAL_N <dbl>,
      #   DPTVARN <dbl>, DPTVALN <dbl>, CTRL_N <int>, CTRL_PCT <dbl>,
      #   SUBGRPVARX <chr>, SUBGRPVARXN <dbl>, `Risk Ratio (CI)` <chr>,
      #   `Risk Ratio` <dbl>, `P-value` <dbl>, `Lower Limit` <dbl>,
      #   `Upper Limit` <dbl>, `(Lower-Upper)` <chr>

