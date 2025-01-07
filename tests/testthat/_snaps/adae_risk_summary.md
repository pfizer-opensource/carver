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
      #   SUBGRPVARXN <dbl>, `Risk Ratio (CI)` <chr>, `Risk Ratio` <dbl>,
      #   `P-value` <dbl>, `Lower Limit` <dbl>, `Upper Limit` <dbl>,
      #   `(Lower-Upper)` <chr>

---

    Code
      output
    Output
      a flextable object.
      col_keys: `  `, `Placebo_n (%) `, `Xanomeline Low Dose_n (%)  `, `Risk Ratio (CI)` 
      header has 2 row(s) 
      body has 24 row(s) 
      original dataset sample: 
                                                        DPTVARN DPTVALN CN
      1                               CARDIAC DISORDERS       1       0  C
      2                         \t\t\tSINUS BRADYCARDIA       1       1  C
      3 RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS       2       0  C
      4                                     \t\t\tCOUGH       2       1  C
      5                        NERVOUS SYSTEM DISORDERS       3       0  C
        Placebo_n (%)  Xanomeline Low Dose_n (%)       Risk Ratio (CI)
      1      1 ( 1.6%)                   4 ( 5.5%)  0.039 (-0.02, 0.1)
      2      1 ( 1.6%)                   4 ( 5.5%)  0.039 (-0.02, 0.1)
      3      1 ( 1.6%)                   5 ( 6.8%) 0.052 (-0.01, 0.12)
      4      1 ( 1.6%)                   5 ( 6.8%) 0.052 (-0.01, 0.12)
      5      2 ( 3.2%)                   6 ( 8.2%)  0.05 (-0.03, 0.13)

# Test Case 2: adae_summary with summary row

    Code
      ae_risk
    Output
      # A tibble: 50 x 32
         TRTVAR       DPTVAR DPTVAL CVALUE DENOMN  FREQ   PCT CPCT  XVAR  CN    PVALUE
         <ord>        <chr>  <chr>  <chr>   <dbl> <dbl> <dbl> <chr> <chr> <chr>  <dbl>
       1 Placebo      TIER   CARDI~ 1 ( 1~     62     1  1.61 " 1.~ CARD~ C     0.236 
       2 Xanomeline ~ TIER   CARDI~ 4 ( 5~     73     4  5.48 " 5.~ CARD~ C     0.236 
       3 Placebo      TIER   GASTR~ 9 (14~     62     9 14.5  "14.~ GAST~ C     0.0342
       4 Xanomeline ~ TIER   GASTR~ 3 ( 4~     73     3  4.11 " 4.~ GAST~ C     0.0342
       5 Placebo      TIER   GENER~ 16 (2~     62    16 25.8  "25.~ GENE~ C     0.0019
       6 Xanomeline ~ TIER   GENER~ 38 (5~     73    38 52.1  "52.~ GENE~ C     0.0019
       7 Placebo      TIER   INFEC~ 8 (12~     62     8 12.9  "12.~ INFE~ C     0.235 
       8 Xanomeline ~ TIER   INFEC~ 5 ( 6~     73     5  6.85 " 6.~ INFE~ C     0.235 
       9 Placebo      TIER   NERVO~ 2 ( 3~     62     2  3.23 " 3.~ NERV~ C     0.221 
      10 Xanomeline ~ TIER   NERVO~ 6 ( 8~     73     6  8.22 " 8.~ NERV~ C     0.221 
      # i 40 more rows
      # i 21 more variables: RISK <dbl>, RISKCIL <dbl>, RISKCIU <dbl>,
      #   ADJPVALUE <dbl>, RISK_CI <chr>, TRTPAIR <chr>, CTRL <chr>, ACTIVE <chr>,
      #   TOTAL_N <dbl>, DPTVARN <dbl>, DPTVALN <dbl>, CTRL_N <int>, CTRL_PCT <dbl>,
      #   SUBGRPVARX <chr>, SUBGRPVARXN <dbl>, `Risk Ratio (CI)` <chr>,
      #   `Risk Ratio` <dbl>, `P-value` <dbl>, `Lower Limit` <dbl>,
      #   `Upper Limit` <dbl>, `(Lower-Upper)` <chr>

---

    Code
      output
    Output
      a flextable object.
      col_keys: `  `, `Placebo_n (%) `, `Xanomeline Low Dose_n (%)  `, `Risk Ratio (CI)` 
      header has 2 row(s) 
      body has 25 row(s) 
      original dataset sample: 
                                   DPTVARN DPTVALN CN Placebo_n (%) 
      1                     Any AE       0       0  C     42 (67.7%)
      2          CARDIAC DISORDERS       1       0  C      1 ( 1.6%)
      3    \t\t\tSINUS BRADYCARDIA       1       1  C      1 ( 1.6%)
      4 GASTROINTESTINAL DISORDERS       2       0  C      9 (14.5%)
      5            \t\t\tDIARRHOEA       2       1  C      9 (14.5%)
        Xanomeline Low Dose_n (%)        Risk Ratio (CI)
      1                  64 (87.7%)   0.199 (0.06, 0.34)
      2                   4 ( 5.5%)   0.039 (-0.02, 0.1)
      3                   4 ( 5.5%)   0.039 (-0.02, 0.1)
      4                   3 ( 4.1%) -0.104 (-0.2, -0.01)
      5                   3 ( 4.1%) -0.104 (-0.2, -0.01)

