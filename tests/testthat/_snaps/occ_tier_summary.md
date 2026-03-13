# occ_tier standard inputs works

    Code
      print(output, n = Inf, width = Inf)
    Output
      # A tibble: 12 x 14
         TRTVAR               DPTVAR
         <ord>                <chr> 
       1 Placebo              TIER  
       2 Placebo              TIER  
       3 Xanomeline Low Dose  TIER  
       4 Xanomeline Low Dose  TIER  
       5 Xanomeline High Dose TIER  
       6 Xanomeline High Dose TIER  
       7 Placebo              TIER  
       8 Placebo              TIER  
       9 Xanomeline Low Dose  TIER  
      10 Xanomeline Low Dose  TIER  
      11 Xanomeline High Dose TIER  
      12 Xanomeline High Dose TIER  
         DPTVAL                                                 CVALUE      DENOMN
         <chr>                                                  <chr>        <int>
       1 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 6 (17.14%)      35
       2 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               8 (22.86%)      35
       3 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 22 (32.35%)     68
       4 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               23 (33.82%)     68
       5 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 22 (31.88%)     69
       6 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               26 (37.68%)     69
       7 "\t\t\tAPPLICATION SITE PRURITUS"                      6 ( 8.70%)      69
       8 "\t\t\tPRURITUS"                                       8 (11.59%)      69
       9 "\t\t\tAPPLICATION SITE PRURITUS"                      22 (28.57%)     77
      10 "\t\t\tPRURITUS"                                       23 (29.87%)     77
      11 "\t\t\tAPPLICATION SITE PRURITUS"                      22 (27.85%)     79
      12 "\t\t\tPRURITUS"                                       26 (32.91%)     79
          FREQ   PCT CPCT    XVAR                                                
         <int> <dbl> <chr>   <chr>                                               
       1     6 17.1  "17.14" GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
       2     8 22.9  "22.86" SKIN AND SUBCUTANEOUS TISSUE DISORDERS              
       3    22 32.4  "32.35" GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
       4    23 33.8  "33.82" SKIN AND SUBCUTANEOUS TISSUE DISORDERS              
       5    22 31.9  "31.88" GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
       6    26 37.7  "37.68" SKIN AND SUBCUTANEOUS TISSUE DISORDERS              
       7     6  8.70 " 8.70" APPLICATION SITE PRURITUS                           
       8     8 11.6  "11.59" PRURITUS                                            
       9    22 28.6  "28.57" APPLICATION SITE PRURITUS                           
      10    23 29.9  "29.87" PRURITUS                                            
      11    22 27.8  "27.85" APPLICATION SITE PRURITUS                           
      12    26 32.9  "32.91" PRURITUS                                            
         CN    DPTVARN DPTVALN SUBGRPVARX SUBGRPVARXN
         <chr>   <int>   <dbl> <chr>            <dbl>
       1 C           1       0 "n (%) "             1
       2 C           2       0 "n (%) "             1
       3 C           1       0 "n (%)  "            1
       4 C           2       0 "n (%)  "            1
       5 C           1       0 "n (%)   "           1
       6 C           2       0 "n (%)   "           1
       7 C           1       1 "n (%) "             1
       8 C           2       1 "n (%) "             1
       9 C           1       1 "n (%)  "            1
      10 C           2       1 "n (%)  "            1
      11 C           1       1 "n (%)   "           1
      12 C           2       1 "n (%)   "           1

# occ_tier modified inputs works

    Code
      print(output, n = Inf, width = Inf)
    Output
      # A tibble: 21 x 14
         TRTVAR               DPTVAR
         <ord>                <chr> 
       1 Placebo              TIER  
       2 Xanomeline Low Dose  TIER  
       3 Xanomeline High Dose TIER  
       4 Placebo              TIER  
       5 Xanomeline Low Dose  TIER  
       6 Xanomeline High Dose TIER  
       7 Placebo              TIER  
       8 Xanomeline Low Dose  TIER  
       9 Xanomeline High Dose TIER  
      10 Placebo              TIER  
      11 Xanomeline Low Dose  TIER  
      12 Xanomeline High Dose TIER  
      13 Placebo              TIER  
      14 Xanomeline Low Dose  TIER  
      15 Xanomeline High Dose TIER  
      16 Placebo              TIER  
      17 Xanomeline Low Dose  TIER  
      18 Xanomeline High Dose TIER  
      19 Placebo              TIER  
      20 Xanomeline Low Dose  TIER  
      21 Xanomeline High Dose TIER  
         DPTVAL                                                 CVALUE         DENOMN
         <chr>                                                  <chr>           <int>
       1 "GASTROINTESTINAL DISORDERS"                           ""                 69
       2 "GASTROINTESTINAL DISORDERS"                           ""                 77
       3 "GASTROINTESTINAL DISORDERS"                           ""                 79
       4 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" ""                 69
       5 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" ""                 77
       6 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" ""                 79
       7 "NERVOUS SYSTEM DISORDERS"                             ""                 69
       8 "NERVOUS SYSTEM DISORDERS"                             ""                 77
       9 "NERVOUS SYSTEM DISORDERS"                             ""                 79
      10 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               ""                 69
      11 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               ""                 77
      12 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               ""                 79
      13 "\t\t\tAPPLICATION SITE PRURITUS"                      "6 ( 8.70%)"       69
      14 "\t\t\tAPPLICATION SITE PRURITUS"                      "22 (28.57%)"      77
      15 "\t\t\tAPPLICATION SITE PRURITUS"                      "22 (27.85%)"      79
      16 "\t\t\tPRURITUS"                                       "8 (11.59%)"       69
      17 "\t\t\tPRURITUS"                                       "23 (29.87%)"      77
      18 "\t\t\tPRURITUS"                                       "26 (32.91%)"      79
      19 "Any Adverse Event"                                    "69 (100.00%)"     69
      20 "Any Adverse Event"                                    "77 (100.00%)"     77
      21 "Any Adverse Event"                                    "79 (100.00%)"     79
          FREQ    PCT CPCT     XVAR                                                
         <int>  <dbl> <chr>    <chr>                                               
       1    17  24.6  "24.64"  GASTROINTESTINAL DISORDERS                          
       2    15  19.5  "19.48"  GASTROINTESTINAL DISORDERS                          
       3    21  26.6  "26.58"  GASTROINTESTINAL DISORDERS                          
       4    21  30.4  "30.43"  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
       5    47  61.0  "61.04"  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
       6    40  50.6  "50.63"  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
       7    12  17.4  "17.39"  NERVOUS SYSTEM DISORDERS                            
       8    20  26.0  "25.97"  NERVOUS SYSTEM DISORDERS                            
       9    27  34.2  "34.18"  NERVOUS SYSTEM DISORDERS                            
      10    21  30.4  "30.43"  SKIN AND SUBCUTANEOUS TISSUE DISORDERS              
      11    42  54.5  "54.55"  SKIN AND SUBCUTANEOUS TISSUE DISORDERS              
      12    42  53.2  "53.16"  SKIN AND SUBCUTANEOUS TISSUE DISORDERS              
      13     6   8.70 " 8.70"  APPLICATION SITE PRURITUS                           
      14    22  28.6  "28.57"  APPLICATION SITE PRURITUS                           
      15    22  27.8  "27.85"  APPLICATION SITE PRURITUS                           
      16     8  11.6  "11.59"  PRURITUS                                            
      17    23  29.9  "29.87"  PRURITUS                                            
      18    26  32.9  "32.91"  PRURITUS                                            
      19    69 100    "100.00" 1                                                   
      20    77 100    "100.00" 1                                                   
      21    79 100    "100.00" 1                                                   
         CN    DPTVARN DPTVALN SUBGRPVARX SUBGRPVARXN
         <chr>   <dbl>   <dbl> <chr>            <dbl>
       1 C           1       0 "n (%) "             1
       2 C           1       0 "n (%)  "            1
       3 C           1       0 "n (%)   "           1
       4 C           2       0 "n (%) "             1
       5 C           2       0 "n (%)  "            1
       6 C           2       0 "n (%)   "           1
       7 C           3       0 "n (%) "             1
       8 C           3       0 "n (%)  "            1
       9 C           3       0 "n (%)   "           1
      10 C           4       0 "n (%) "             1
      11 C           4       0 "n (%)  "            1
      12 C           4       0 "n (%)   "           1
      13 C           2       1 "n (%) "             1
      14 C           2       1 "n (%)  "            1
      15 C           2       1 "n (%)   "           1
      16 C           4       1 "n (%) "             1
      17 C           4       1 "n (%)  "            1
      18 C           4       1 "n (%)   "           1
      19 C           0       0 "n (%) "             1
      20 C           0       0 "n (%)  "            1
      21 C           0       0 "n (%)   "           1

# occ_tier standard inputs works max severity

    Code
      print(output, n = Inf, width = Inf)
    Output
      # A tibble: 153 x 16
          TRTVAR               SUBGRPVAR1 DPTVAR
          <ord>                <chr>      <chr> 
        1 Placebo              MILD       TIER  
        2 Placebo              MODERATE   TIER  
        3 Placebo              SEVERE     TIER  
        4 Xanomeline Low Dose  MILD       TIER  
        5 Xanomeline Low Dose  MODERATE   TIER  
        6 Xanomeline Low Dose  SEVERE     TIER  
        7 Xanomeline High Dose MILD       TIER  
        8 Xanomeline High Dose MODERATE   TIER  
        9 Xanomeline High Dose SEVERE     TIER  
       10 Placebo              MILD       TIER  
       11 Placebo              MODERATE   TIER  
       12 Placebo              SEVERE     TIER  
       13 Xanomeline Low Dose  MILD       TIER  
       14 Xanomeline Low Dose  MODERATE   TIER  
       15 Xanomeline Low Dose  SEVERE     TIER  
       16 Xanomeline High Dose MILD       TIER  
       17 Xanomeline High Dose MODERATE   TIER  
       18 Xanomeline High Dose SEVERE     TIER  
       19 Placebo              MILD       TIER  
       20 Placebo              MODERATE   TIER  
       21 Placebo              SEVERE     TIER  
       22 Xanomeline Low Dose  MILD       TIER  
       23 Xanomeline Low Dose  MODERATE   TIER  
       24 Xanomeline Low Dose  SEVERE     TIER  
       25 Xanomeline High Dose MILD       TIER  
       26 Xanomeline High Dose MODERATE   TIER  
       27 Xanomeline High Dose SEVERE     TIER  
       28 Placebo              MILD       TIER  
       29 Placebo              MODERATE   TIER  
       30 Placebo              SEVERE     TIER  
       31 Xanomeline Low Dose  MILD       TIER  
       32 Xanomeline Low Dose  MODERATE   TIER  
       33 Xanomeline Low Dose  SEVERE     TIER  
       34 Xanomeline High Dose MILD       TIER  
       35 Xanomeline High Dose MODERATE   TIER  
       36 Xanomeline High Dose SEVERE     TIER  
       37 Placebo              MILD       TIER  
       38 Placebo              MODERATE   TIER  
       39 Placebo              SEVERE     TIER  
       40 Xanomeline Low Dose  MILD       TIER  
       41 Xanomeline Low Dose  MODERATE   TIER  
       42 Xanomeline Low Dose  SEVERE     TIER  
       43 Xanomeline High Dose MILD       TIER  
       44 Xanomeline High Dose MODERATE   TIER  
       45 Xanomeline High Dose SEVERE     TIER  
       46 Placebo              MILD       TIER  
       47 Placebo              MODERATE   TIER  
       48 Placebo              SEVERE     TIER  
       49 Xanomeline Low Dose  MILD       TIER  
       50 Xanomeline Low Dose  MODERATE   TIER  
       51 Xanomeline Low Dose  SEVERE     TIER  
       52 Xanomeline High Dose MILD       TIER  
       53 Xanomeline High Dose MODERATE   TIER  
       54 Xanomeline High Dose SEVERE     TIER  
       55 Placebo              MILD       TIER  
       56 Placebo              MODERATE   TIER  
       57 Placebo              SEVERE     TIER  
       58 Xanomeline Low Dose  MILD       TIER  
       59 Xanomeline Low Dose  MODERATE   TIER  
       60 Xanomeline Low Dose  SEVERE     TIER  
       61 Xanomeline High Dose MILD       TIER  
       62 Xanomeline High Dose MODERATE   TIER  
       63 Xanomeline High Dose SEVERE     TIER  
       64 Placebo              MILD       TIER  
       65 Placebo              MODERATE   TIER  
       66 Placebo              SEVERE     TIER  
       67 Xanomeline Low Dose  MILD       TIER  
       68 Xanomeline Low Dose  MODERATE   TIER  
       69 Xanomeline Low Dose  SEVERE     TIER  
       70 Xanomeline High Dose MILD       TIER  
       71 Xanomeline High Dose MODERATE   TIER  
       72 Xanomeline High Dose SEVERE     TIER  
       73 Placebo              MILD       TIER  
       74 Placebo              MODERATE   TIER  
       75 Placebo              SEVERE     TIER  
       76 Xanomeline Low Dose  MILD       TIER  
       77 Xanomeline Low Dose  MODERATE   TIER  
       78 Xanomeline Low Dose  SEVERE     TIER  
       79 Xanomeline High Dose MILD       TIER  
       80 Xanomeline High Dose MODERATE   TIER  
       81 Xanomeline High Dose SEVERE     TIER  
       82 Placebo              MILD       TIER  
       83 Placebo              MODERATE   TIER  
       84 Placebo              SEVERE     TIER  
       85 Xanomeline Low Dose  MILD       TIER  
       86 Xanomeline Low Dose  MODERATE   TIER  
       87 Xanomeline Low Dose  SEVERE     TIER  
       88 Xanomeline High Dose MILD       TIER  
       89 Xanomeline High Dose MODERATE   TIER  
       90 Xanomeline High Dose SEVERE     TIER  
       91 Placebo              MILD       TIER  
       92 Placebo              MODERATE   TIER  
       93 Placebo              SEVERE     TIER  
       94 Xanomeline Low Dose  MILD       TIER  
       95 Xanomeline Low Dose  MODERATE   TIER  
       96 Xanomeline Low Dose  SEVERE     TIER  
       97 Xanomeline High Dose MILD       TIER  
       98 Xanomeline High Dose MODERATE   TIER  
       99 Xanomeline High Dose SEVERE     TIER  
      100 Placebo              MILD       TIER  
      101 Placebo              MODERATE   TIER  
      102 Placebo              SEVERE     TIER  
      103 Xanomeline Low Dose  MILD       TIER  
      104 Xanomeline Low Dose  MODERATE   TIER  
      105 Xanomeline Low Dose  SEVERE     TIER  
      106 Xanomeline High Dose MILD       TIER  
      107 Xanomeline High Dose MODERATE   TIER  
      108 Xanomeline High Dose SEVERE     TIER  
      109 Placebo              MILD       TIER  
      110 Placebo              MODERATE   TIER  
      111 Placebo              SEVERE     TIER  
      112 Xanomeline Low Dose  MILD       TIER  
      113 Xanomeline Low Dose  MODERATE   TIER  
      114 Xanomeline Low Dose  SEVERE     TIER  
      115 Xanomeline High Dose MILD       TIER  
      116 Xanomeline High Dose MODERATE   TIER  
      117 Xanomeline High Dose SEVERE     TIER  
      118 Placebo              MILD       TIER  
      119 Placebo              MODERATE   TIER  
      120 Placebo              SEVERE     TIER  
      121 Xanomeline Low Dose  MILD       TIER  
      122 Xanomeline Low Dose  MODERATE   TIER  
      123 Xanomeline Low Dose  SEVERE     TIER  
      124 Xanomeline High Dose MILD       TIER  
      125 Xanomeline High Dose MODERATE   TIER  
      126 Xanomeline High Dose SEVERE     TIER  
      127 Placebo              MILD       TIER  
      128 Placebo              MODERATE   TIER  
      129 Placebo              SEVERE     TIER  
      130 Xanomeline Low Dose  MILD       TIER  
      131 Xanomeline Low Dose  MODERATE   TIER  
      132 Xanomeline Low Dose  SEVERE     TIER  
      133 Xanomeline High Dose MILD       TIER  
      134 Xanomeline High Dose MODERATE   TIER  
      135 Xanomeline High Dose SEVERE     TIER  
      136 Placebo              MILD       TIER  
      137 Placebo              MODERATE   TIER  
      138 Placebo              SEVERE     TIER  
      139 Xanomeline Low Dose  MILD       TIER  
      140 Xanomeline Low Dose  MODERATE   TIER  
      141 Xanomeline Low Dose  SEVERE     TIER  
      142 Xanomeline High Dose MILD       TIER  
      143 Xanomeline High Dose MODERATE   TIER  
      144 Xanomeline High Dose SEVERE     TIER  
      145 Placebo              MILD       TIER  
      146 Placebo              MODERATE   TIER  
      147 Xanomeline Low Dose  MILD       TIER  
      148 Xanomeline Low Dose  MODERATE   TIER  
      149 Xanomeline Low Dose  SEVERE     TIER  
      150 Xanomeline High Dose MILD       TIER  
      151 Xanomeline High Dose MODERATE   TIER  
      152 Xanomeline High Dose SEVERE     TIER  
      153 Placebo              SEVERE     TIER  
          DPTVAL                                                 CVALUE      DENOMN
          <chr>                                                  <chr>        <int>
        1 "CARDIAC DISORDERS"                                    8 (15.09%)      53
        2 "CARDIAC DISORDERS"                                    2 ( 3.77%)      53
        3 "CARDIAC DISORDERS"                                    2 ( 3.77%)      53
        4 "CARDIAC DISORDERS"                                    8 (10.81%)      74
        5 "CARDIAC DISORDERS"                                    5 ( 6.76%)      74
        6 "CARDIAC DISORDERS"                                    0               74
        7 "CARDIAC DISORDERS"                                    9 (12.33%)      73
        8 "CARDIAC DISORDERS"                                    5 ( 6.85%)      73
        9 "CARDIAC DISORDERS"                                    1 ( 1.37%)      73
       10 "GASTROINTESTINAL DISORDERS"                           15 (28.30%)     53
       11 "GASTROINTESTINAL DISORDERS"                           2 ( 3.77%)      53
       12 "GASTROINTESTINAL DISORDERS"                           0               53
       13 "GASTROINTESTINAL DISORDERS"                           10 (13.51%)     74
       14 "GASTROINTESTINAL DISORDERS"                           4 ( 5.41%)      74
       15 "GASTROINTESTINAL DISORDERS"                           0               74
       16 "GASTROINTESTINAL DISORDERS"                           14 (19.18%)     73
       17 "GASTROINTESTINAL DISORDERS"                           4 ( 5.48%)      73
       18 "GASTROINTESTINAL DISORDERS"                           2 ( 2.74%)      73
       19 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 16 (30.19%)     53
       20 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 5 ( 9.43%)      53
       21 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 0               53
       22 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 19 (25.68%)     74
       23 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 21 (28.38%)     74
       24 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 7 ( 9.46%)      74
       25 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 19 (26.03%)     73
       26 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 21 (28.77%)     73
       27 "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" 0               73
       28 "NERVOUS SYSTEM DISORDERS"                             6 (11.32%)      53
       29 "NERVOUS SYSTEM DISORDERS"                             2 ( 3.77%)      53
       30 "NERVOUS SYSTEM DISORDERS"                             0               53
       31 "NERVOUS SYSTEM DISORDERS"                             10 (13.51%)     74
       32 "NERVOUS SYSTEM DISORDERS"                             7 ( 9.46%)      74
       33 "NERVOUS SYSTEM DISORDERS"                             3 ( 4.05%)      74
       34 "NERVOUS SYSTEM DISORDERS"                             13 (17.81%)     73
       35 "NERVOUS SYSTEM DISORDERS"                             8 (10.96%)      73
       36 "NERVOUS SYSTEM DISORDERS"                             4 ( 5.48%)      73
       37 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               12 (22.64%)     53
       38 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               8 (15.09%)      53
       39 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               0               53
       40 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               12 (16.22%)     74
       41 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               23 (31.08%)     74
       42 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               4 ( 5.41%)      74
       43 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               24 (32.88%)     73
       44 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               15 (20.55%)     73
       45 "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"               1 ( 1.37%)      73
       46 "\t\t\tSINUS BRADYCARDIA"                              1 ( 1.54%)      65
       47 "\t\t\tSINUS BRADYCARDIA"                              1 ( 1.54%)      65
       48 "\t\t\tSINUS BRADYCARDIA"                              0               65
       49 "\t\t\tSINUS BRADYCARDIA"                              6 ( 7.79%)      77
       50 "\t\t\tSINUS BRADYCARDIA"                              1 ( 1.30%)      77
       51 "\t\t\tSINUS BRADYCARDIA"                              0               77
       52 "\t\t\tSINUS BRADYCARDIA"                              4 ( 5.26%)      76
       53 "\t\t\tSINUS BRADYCARDIA"                              4 ( 5.26%)      76
       54 "\t\t\tSINUS BRADYCARDIA"                              0               76
       55 "\t\t\tDIARRHOEA"                                      9 (13.85%)      65
       56 "\t\t\tDIARRHOEA"                                      0               65
       57 "\t\t\tDIARRHOEA"                                      0               65
       58 "\t\t\tDIARRHOEA"                                      4 ( 5.19%)      77
       59 "\t\t\tDIARRHOEA"                                      0               77
       60 "\t\t\tDIARRHOEA"                                      0               77
       61 "\t\t\tDIARRHOEA"                                      2 ( 2.63%)      76
       62 "\t\t\tDIARRHOEA"                                      2 ( 2.63%)      76
       63 "\t\t\tDIARRHOEA"                                      0               76
       64 "\t\t\tAPPLICATION SITE ERYTHEMA"                      3 ( 4.62%)      65
       65 "\t\t\tAPPLICATION SITE ERYTHEMA"                      0               65
       66 "\t\t\tAPPLICATION SITE ERYTHEMA"                      0               65
       67 "\t\t\tAPPLICATION SITE ERYTHEMA"                      4 ( 5.19%)      77
       68 "\t\t\tAPPLICATION SITE ERYTHEMA"                      6 ( 7.79%)      77
       69 "\t\t\tAPPLICATION SITE ERYTHEMA"                      2 ( 2.60%)      77
       70 "\t\t\tAPPLICATION SITE ERYTHEMA"                      9 (11.84%)      76
       71 "\t\t\tAPPLICATION SITE ERYTHEMA"                      6 ( 7.89%)      76
       72 "\t\t\tAPPLICATION SITE ERYTHEMA"                      0               76
       73 "\t\t\tAPPLICATION SITE IRRITATION"                    1 ( 1.54%)      65
       74 "\t\t\tAPPLICATION SITE IRRITATION"                    2 ( 3.08%)      65
       75 "\t\t\tAPPLICATION SITE IRRITATION"                    0               65
       76 "\t\t\tAPPLICATION SITE IRRITATION"                    3 ( 3.90%)      77
       77 "\t\t\tAPPLICATION SITE IRRITATION"                    3 ( 3.90%)      77
       78 "\t\t\tAPPLICATION SITE IRRITATION"                    3 ( 3.90%)      77
       79 "\t\t\tAPPLICATION SITE IRRITATION"                    3 ( 3.95%)      76
       80 "\t\t\tAPPLICATION SITE IRRITATION"                    6 ( 7.89%)      76
       81 "\t\t\tAPPLICATION SITE IRRITATION"                    0               76
       82 "\t\t\tAPPLICATION SITE PRURITUS"                      5 ( 7.69%)      65
       83 "\t\t\tAPPLICATION SITE PRURITUS"                      1 ( 1.54%)      65
       84 "\t\t\tAPPLICATION SITE PRURITUS"                      0               65
       85 "\t\t\tAPPLICATION SITE PRURITUS"                      13 (16.88%)     77
       86 "\t\t\tAPPLICATION SITE PRURITUS"                      8 (10.39%)      77
       87 "\t\t\tAPPLICATION SITE PRURITUS"                      1 ( 1.30%)      77
       88 "\t\t\tAPPLICATION SITE PRURITUS"                      10 (13.16%)     76
       89 "\t\t\tAPPLICATION SITE PRURITUS"                      12 (15.79%)     76
       90 "\t\t\tAPPLICATION SITE PRURITUS"                      0               76
       91 "\t\t\tDIZZINESS"                                      2 ( 3.08%)      65
       92 "\t\t\tDIZZINESS"                                      0               65
       93 "\t\t\tDIZZINESS"                                      0               65
       94 "\t\t\tDIZZINESS"                                      5 ( 6.49%)      77
       95 "\t\t\tDIZZINESS"                                      3 ( 3.90%)      77
       96 "\t\t\tDIZZINESS"                                      0               77
       97 "\t\t\tDIZZINESS"                                      7 ( 9.21%)      76
       98 "\t\t\tDIZZINESS"                                      3 ( 3.95%)      76
       99 "\t\t\tDIZZINESS"                                      1 ( 1.32%)      76
      100 "\t\t\tERYTHEMA"                                       4 ( 6.15%)      65
      101 "\t\t\tERYTHEMA"                                       4 ( 6.15%)      65
      102 "\t\t\tERYTHEMA"                                       0               65
      103 "\t\t\tERYTHEMA"                                       6 ( 7.79%)      77
      104 "\t\t\tERYTHEMA"                                       8 (10.39%)      77
      105 "\t\t\tERYTHEMA"                                       0               77
      106 "\t\t\tERYTHEMA"                                       10 (13.16%)     76
      107 "\t\t\tERYTHEMA"                                       4 ( 5.26%)      76
      108 "\t\t\tERYTHEMA"                                       0               76
      109 "\t\t\tHYPERHIDROSIS"                                  2 ( 3.08%)      65
      110 "\t\t\tHYPERHIDROSIS"                                  0               65
      111 "\t\t\tHYPERHIDROSIS"                                  0               65
      112 "\t\t\tHYPERHIDROSIS"                                  1 ( 1.30%)      77
      113 "\t\t\tHYPERHIDROSIS"                                  3 ( 3.90%)      77
      114 "\t\t\tHYPERHIDROSIS"                                  0               77
      115 "\t\t\tHYPERHIDROSIS"                                  8 (10.53%)      76
      116 "\t\t\tHYPERHIDROSIS"                                  0               76
      117 "\t\t\tHYPERHIDROSIS"                                  0               76
      118 "\t\t\tPRURITUS"                                       7 (10.77%)      65
      119 "\t\t\tPRURITUS"                                       1 ( 1.54%)      65
      120 "\t\t\tPRURITUS"                                       0               65
      121 "\t\t\tPRURITUS"                                       9 (11.69%)      77
      122 "\t\t\tPRURITUS"                                       11 (14.29%)     77
      123 "\t\t\tPRURITUS"                                       1 ( 1.30%)      77
      124 "\t\t\tPRURITUS"                                       17 (22.37%)     76
      125 "\t\t\tPRURITUS"                                       9 (11.84%)      76
      126 "\t\t\tPRURITUS"                                       0               76
      127 "\t\t\tRASH"                                           2 ( 3.08%)      65
      128 "\t\t\tRASH"                                           3 ( 4.62%)      65
      129 "\t\t\tRASH"                                           0               65
      130 "\t\t\tRASH"                                           9 (11.69%)      77
      131 "\t\t\tRASH"                                           3 ( 3.90%)      77
      132 "\t\t\tRASH"                                           1 ( 1.30%)      77
      133 "\t\t\tRASH"                                           5 ( 6.58%)      76
      134 "\t\t\tRASH"                                           3 ( 3.95%)      76
      135 "\t\t\tRASH"                                           1 ( 1.32%)      76
      136 "Any Adverse Event"                                    21 (39.62%)     53
      137 "Any Adverse Event"                                    8 (15.09%)      53
      138 "Any Adverse Event"                                    0               53
      139 "Any Adverse Event"                                    16 (21.62%)     74
      140 "Any Adverse Event"                                    24 (32.43%)     74
      141 "Any Adverse Event"                                    6 ( 8.11%)      74
      142 "Any Adverse Event"                                    19 (26.03%)     73
      143 "Any Adverse Event"                                    32 (43.84%)     73
      144 "Any Adverse Event"                                    2 ( 2.74%)      73
      145 "Total preferred term events"                          36              NA
      146 "Total preferred term events"                          12              NA
      147 "Total preferred term events"                          60              NA
      148 "Total preferred term events"                          46              NA
      149 "Total preferred term events"                          8               NA
      150 "Total preferred term events"                          75              NA
      151 "Total preferred term events"                          49              NA
      152 "Total preferred term events"                          2               NA
      153 "Total preferred term events"                          0               NA
           FREQ SUBGRPVAR1N   PCT CPCT   
          <int>       <dbl> <dbl> <chr>  
        1     8           1 15.1  "15.09"
        2     2           2  3.77 " 3.77"
        3     2           3  3.77 " 3.77"
        4     8           1 10.8  "10.81"
        5     5           2  6.76 " 6.76"
        6     0           3  0    " 0.00"
        7     9           1 12.3  "12.33"
        8     5           2  6.85 " 6.85"
        9     1           3  1.37 " 1.37"
       10    15           1 28.3  "28.30"
       11     2           2  3.77 " 3.77"
       12     0           3  0    " 0.00"
       13    10           1 13.5  "13.51"
       14     4           2  5.41 " 5.41"
       15     0           3  0    " 0.00"
       16    14           1 19.2  "19.18"
       17     4           2  5.48 " 5.48"
       18     2           3  2.74 " 2.74"
       19    16           1 30.2  "30.19"
       20     5           2  9.43 " 9.43"
       21     0           3  0    " 0.00"
       22    19           1 25.7  "25.68"
       23    21           2 28.4  "28.38"
       24     7           3  9.46 " 9.46"
       25    19           1 26.0  "26.03"
       26    21           2 28.8  "28.77"
       27     0           3  0    " 0.00"
       28     6           1 11.3  "11.32"
       29     2           2  3.77 " 3.77"
       30     0           3  0    " 0.00"
       31    10           1 13.5  "13.51"
       32     7           2  9.46 " 9.46"
       33     3           3  4.05 " 4.05"
       34    13           1 17.8  "17.81"
       35     8           2 11.0  "10.96"
       36     4           3  5.48 " 5.48"
       37    12           1 22.6  "22.64"
       38     8           2 15.1  "15.09"
       39     0           3  0    " 0.00"
       40    12           1 16.2  "16.22"
       41    23           2 31.1  "31.08"
       42     4           3  5.41 " 5.41"
       43    24           1 32.9  "32.88"
       44    15           2 20.5  "20.55"
       45     1           3  1.37 " 1.37"
       46     1           1  1.54 " 1.54"
       47     1           2  1.54 " 1.54"
       48     0           3  0    " 0.00"
       49     6           1  7.79 " 7.79"
       50     1           2  1.30 " 1.30"
       51     0           3  0    " 0.00"
       52     4           1  5.26 " 5.26"
       53     4           2  5.26 " 5.26"
       54     0           3  0    " 0.00"
       55     9           1 13.8  "13.85"
       56     0           2  0    " 0.00"
       57     0           3  0    " 0.00"
       58     4           1  5.19 " 5.19"
       59     0           2  0    " 0.00"
       60     0           3  0    " 0.00"
       61     2           1  2.63 " 2.63"
       62     2           2  2.63 " 2.63"
       63     0           3  0    " 0.00"
       64     3           1  4.62 " 4.62"
       65     0           2  0    " 0.00"
       66     0           3  0    " 0.00"
       67     4           1  5.19 " 5.19"
       68     6           2  7.79 " 7.79"
       69     2           3  2.60 " 2.60"
       70     9           1 11.8  "11.84"
       71     6           2  7.89 " 7.89"
       72     0           3  0    " 0.00"
       73     1           1  1.54 " 1.54"
       74     2           2  3.08 " 3.08"
       75     0           3  0    " 0.00"
       76     3           1  3.90 " 3.90"
       77     3           2  3.90 " 3.90"
       78     3           3  3.90 " 3.90"
       79     3           1  3.95 " 3.95"
       80     6           2  7.89 " 7.89"
       81     0           3  0    " 0.00"
       82     5           1  7.69 " 7.69"
       83     1           2  1.54 " 1.54"
       84     0           3  0    " 0.00"
       85    13           1 16.9  "16.88"
       86     8           2 10.4  "10.39"
       87     1           3  1.30 " 1.30"
       88    10           1 13.2  "13.16"
       89    12           2 15.8  "15.79"
       90     0           3  0    " 0.00"
       91     2           1  3.08 " 3.08"
       92     0           2  0    " 0.00"
       93     0           3  0    " 0.00"
       94     5           1  6.49 " 6.49"
       95     3           2  3.90 " 3.90"
       96     0           3  0    " 0.00"
       97     7           1  9.21 " 9.21"
       98     3           2  3.95 " 3.95"
       99     1           3  1.32 " 1.32"
      100     4           1  6.15 " 6.15"
      101     4           2  6.15 " 6.15"
      102     0           3  0    " 0.00"
      103     6           1  7.79 " 7.79"
      104     8           2 10.4  "10.39"
      105     0           3  0    " 0.00"
      106    10           1 13.2  "13.16"
      107     4           2  5.26 " 5.26"
      108     0           3  0    " 0.00"
      109     2           1  3.08 " 3.08"
      110     0           2  0    " 0.00"
      111     0           3  0    " 0.00"
      112     1           1  1.30 " 1.30"
      113     3           2  3.90 " 3.90"
      114     0           3  0    " 0.00"
      115     8           1 10.5  "10.53"
      116     0           2  0    " 0.00"
      117     0           3  0    " 0.00"
      118     7           1 10.8  "10.77"
      119     1           2  1.54 " 1.54"
      120     0           3  0    " 0.00"
      121     9           1 11.7  "11.69"
      122    11           2 14.3  "14.29"
      123     1           3  1.30 " 1.30"
      124    17           1 22.4  "22.37"
      125     9           2 11.8  "11.84"
      126     0           3  0    " 0.00"
      127     2           1  3.08 " 3.08"
      128     3           2  4.62 " 4.62"
      129     0           3  0    " 0.00"
      130     9           1 11.7  "11.69"
      131     3           2  3.90 " 3.90"
      132     1           3  1.30 " 1.30"
      133     5           1  6.58 " 6.58"
      134     3           2  3.95 " 3.95"
      135     1           3  1.32 " 1.32"
      136    21           1 39.6  "39.62"
      137     8           2 15.1  "15.09"
      138     0           3  0    " 0.00"
      139    16           1 21.6  "21.62"
      140    24           2 32.4  "32.43"
      141     6           3  8.11 " 8.11"
      142    19           1 26.0  "26.03"
      143    32           2 43.8  "43.84"
      144     2           3  2.74 " 2.74"
      145    36           1 NA     <NA>  
      146    12           2 NA     <NA>  
      147    60           1 NA     <NA>  
      148    46           2 NA     <NA>  
      149     8           3 NA     <NA>  
      150    75           1 NA     <NA>  
      151    49           2 NA     <NA>  
      152     2           3 NA     <NA>  
      153     0           3 NA     <NA>  
          XVAR                                                 CN    DPTVARN DPTVALN
          <chr>                                                <chr>   <dbl>   <dbl>
        1 CARDIAC DISORDERS                                    C           1       0
        2 CARDIAC DISORDERS                                    C           1       0
        3 CARDIAC DISORDERS                                    C           1       0
        4 CARDIAC DISORDERS                                    C           1       0
        5 CARDIAC DISORDERS                                    C           1       0
        6 CARDIAC DISORDERS                                    C           1       0
        7 CARDIAC DISORDERS                                    C           1       0
        8 CARDIAC DISORDERS                                    C           1       0
        9 CARDIAC DISORDERS                                    C           1       0
       10 GASTROINTESTINAL DISORDERS                           C           2       0
       11 GASTROINTESTINAL DISORDERS                           C           2       0
       12 GASTROINTESTINAL DISORDERS                           C           2       0
       13 GASTROINTESTINAL DISORDERS                           C           2       0
       14 GASTROINTESTINAL DISORDERS                           C           2       0
       15 GASTROINTESTINAL DISORDERS                           C           2       0
       16 GASTROINTESTINAL DISORDERS                           C           2       0
       17 GASTROINTESTINAL DISORDERS                           C           2       0
       18 GASTROINTESTINAL DISORDERS                           C           2       0
       19 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       20 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       21 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       22 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       23 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       24 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       25 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       26 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       27 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS C           3       0
       28 NERVOUS SYSTEM DISORDERS                             C           4       0
       29 NERVOUS SYSTEM DISORDERS                             C           4       0
       30 NERVOUS SYSTEM DISORDERS                             C           4       0
       31 NERVOUS SYSTEM DISORDERS                             C           4       0
       32 NERVOUS SYSTEM DISORDERS                             C           4       0
       33 NERVOUS SYSTEM DISORDERS                             C           4       0
       34 NERVOUS SYSTEM DISORDERS                             C           4       0
       35 NERVOUS SYSTEM DISORDERS                             C           4       0
       36 NERVOUS SYSTEM DISORDERS                             C           4       0
       37 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       38 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       39 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       40 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       41 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       42 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       43 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       44 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       45 SKIN AND SUBCUTANEOUS TISSUE DISORDERS               C           5       0
       46 SINUS BRADYCARDIA                                    C           1       1
       47 SINUS BRADYCARDIA                                    C           1       1
       48 SINUS BRADYCARDIA                                    C           1       1
       49 SINUS BRADYCARDIA                                    C           1       1
       50 SINUS BRADYCARDIA                                    C           1       1
       51 SINUS BRADYCARDIA                                    C           1       1
       52 SINUS BRADYCARDIA                                    C           1       1
       53 SINUS BRADYCARDIA                                    C           1       1
       54 SINUS BRADYCARDIA                                    C           1       1
       55 DIARRHOEA                                            C           2       1
       56 DIARRHOEA                                            C           2       1
       57 DIARRHOEA                                            C           2       1
       58 DIARRHOEA                                            C           2       1
       59 DIARRHOEA                                            C           2       1
       60 DIARRHOEA                                            C           2       1
       61 DIARRHOEA                                            C           2       1
       62 DIARRHOEA                                            C           2       1
       63 DIARRHOEA                                            C           2       1
       64 APPLICATION SITE ERYTHEMA                            C           3       1
       65 APPLICATION SITE ERYTHEMA                            C           3       1
       66 APPLICATION SITE ERYTHEMA                            C           3       1
       67 APPLICATION SITE ERYTHEMA                            C           3       1
       68 APPLICATION SITE ERYTHEMA                            C           3       1
       69 APPLICATION SITE ERYTHEMA                            C           3       1
       70 APPLICATION SITE ERYTHEMA                            C           3       1
       71 APPLICATION SITE ERYTHEMA                            C           3       1
       72 APPLICATION SITE ERYTHEMA                            C           3       1
       73 APPLICATION SITE IRRITATION                          C           3       2
       74 APPLICATION SITE IRRITATION                          C           3       2
       75 APPLICATION SITE IRRITATION                          C           3       2
       76 APPLICATION SITE IRRITATION                          C           3       2
       77 APPLICATION SITE IRRITATION                          C           3       2
       78 APPLICATION SITE IRRITATION                          C           3       2
       79 APPLICATION SITE IRRITATION                          C           3       2
       80 APPLICATION SITE IRRITATION                          C           3       2
       81 APPLICATION SITE IRRITATION                          C           3       2
       82 APPLICATION SITE PRURITUS                            C           3       3
       83 APPLICATION SITE PRURITUS                            C           3       3
       84 APPLICATION SITE PRURITUS                            C           3       3
       85 APPLICATION SITE PRURITUS                            C           3       3
       86 APPLICATION SITE PRURITUS                            C           3       3
       87 APPLICATION SITE PRURITUS                            C           3       3
       88 APPLICATION SITE PRURITUS                            C           3       3
       89 APPLICATION SITE PRURITUS                            C           3       3
       90 APPLICATION SITE PRURITUS                            C           3       3
       91 DIZZINESS                                            C           4       1
       92 DIZZINESS                                            C           4       1
       93 DIZZINESS                                            C           4       1
       94 DIZZINESS                                            C           4       1
       95 DIZZINESS                                            C           4       1
       96 DIZZINESS                                            C           4       1
       97 DIZZINESS                                            C           4       1
       98 DIZZINESS                                            C           4       1
       99 DIZZINESS                                            C           4       1
      100 ERYTHEMA                                             C           5       1
      101 ERYTHEMA                                             C           5       1
      102 ERYTHEMA                                             C           5       1
      103 ERYTHEMA                                             C           5       1
      104 ERYTHEMA                                             C           5       1
      105 ERYTHEMA                                             C           5       1
      106 ERYTHEMA                                             C           5       1
      107 ERYTHEMA                                             C           5       1
      108 ERYTHEMA                                             C           5       1
      109 HYPERHIDROSIS                                        C           5       2
      110 HYPERHIDROSIS                                        C           5       2
      111 HYPERHIDROSIS                                        C           5       2
      112 HYPERHIDROSIS                                        C           5       2
      113 HYPERHIDROSIS                                        C           5       2
      114 HYPERHIDROSIS                                        C           5       2
      115 HYPERHIDROSIS                                        C           5       2
      116 HYPERHIDROSIS                                        C           5       2
      117 HYPERHIDROSIS                                        C           5       2
      118 PRURITUS                                             C           5       3
      119 PRURITUS                                             C           5       3
      120 PRURITUS                                             C           5       3
      121 PRURITUS                                             C           5       3
      122 PRURITUS                                             C           5       3
      123 PRURITUS                                             C           5       3
      124 PRURITUS                                             C           5       3
      125 PRURITUS                                             C           5       3
      126 PRURITUS                                             C           5       3
      127 RASH                                                 C           5       4
      128 RASH                                                 C           5       4
      129 RASH                                                 C           5       4
      130 RASH                                                 C           5       4
      131 RASH                                                 C           5       4
      132 RASH                                                 C           5       4
      133 RASH                                                 C           5       4
      134 RASH                                                 C           5       4
      135 RASH                                                 C           5       4
      136 1                                                    C           0       0
      137 1                                                    C           0       0
      138 1                                                    C           0       0
      139 1                                                    C           0       0
      140 1                                                    C           0       0
      141 1                                                    C           0       0
      142 1                                                    C           0       0
      143 1                                                    C           0       0
      144 1                                                    C           0       0
      145 1                                                    C           6       0
      146 1                                                    C           6       0
      147 1                                                    C           6       0
      148 1                                                    C           6       0
      149 1                                                    C           6       0
      150 1                                                    C           6       0
      151 1                                                    C           6       0
      152 1                                                    C           6       0
      153 1                                                    C           6       0
          SUBGRPVARX SUBGRPVARXN
          <chr>            <dbl>
        1 "n "                 1
        2 "n  "                1
        3 "n   "               1
        4 "n "                 1
        5 "n  "                1
        6 "n   "               1
        7 "n "                 1
        8 "n  "                1
        9 "n   "               1
       10 "n "                 1
       11 "n  "                1
       12 "n   "               1
       13 "n "                 1
       14 "n  "                1
       15 "n   "               1
       16 "n "                 1
       17 "n  "                1
       18 "n   "               1
       19 "n "                 1
       20 "n  "                1
       21 "n   "               1
       22 "n "                 1
       23 "n  "                1
       24 "n   "               1
       25 "n "                 1
       26 "n  "                1
       27 "n   "               1
       28 "n "                 1
       29 "n  "                1
       30 "n   "               1
       31 "n "                 1
       32 "n  "                1
       33 "n   "               1
       34 "n "                 1
       35 "n  "                1
       36 "n   "               1
       37 "n "                 1
       38 "n  "                1
       39 "n   "               1
       40 "n "                 1
       41 "n  "                1
       42 "n   "               1
       43 "n "                 1
       44 "n  "                1
       45 "n   "               1
       46 "n "                 1
       47 "n  "                1
       48 "n   "               1
       49 "n "                 1
       50 "n  "                1
       51 "n   "               1
       52 "n "                 1
       53 "n  "                1
       54 "n   "               1
       55 "n "                 1
       56 "n  "                1
       57 "n   "               1
       58 "n "                 1
       59 "n  "                1
       60 "n   "               1
       61 "n "                 1
       62 "n  "                1
       63 "n   "               1
       64 "n "                 1
       65 "n  "                1
       66 "n   "               1
       67 "n "                 1
       68 "n  "                1
       69 "n   "               1
       70 "n "                 1
       71 "n  "                1
       72 "n   "               1
       73 "n "                 1
       74 "n  "                1
       75 "n   "               1
       76 "n "                 1
       77 "n  "                1
       78 "n   "               1
       79 "n "                 1
       80 "n  "                1
       81 "n   "               1
       82 "n "                 1
       83 "n  "                1
       84 "n   "               1
       85 "n "                 1
       86 "n  "                1
       87 "n   "               1
       88 "n "                 1
       89 "n  "                1
       90 "n   "               1
       91 "n "                 1
       92 "n  "                1
       93 "n   "               1
       94 "n "                 1
       95 "n  "                1
       96 "n   "               1
       97 "n "                 1
       98 "n  "                1
       99 "n   "               1
      100 "n "                 1
      101 "n  "                1
      102 "n   "               1
      103 "n "                 1
      104 "n  "                1
      105 "n   "               1
      106 "n "                 1
      107 "n  "                1
      108 "n   "               1
      109 "n "                 1
      110 "n  "                1
      111 "n   "               1
      112 "n "                 1
      113 "n  "                1
      114 "n   "               1
      115 "n "                 1
      116 "n  "                1
      117 "n   "               1
      118 "n "                 1
      119 "n  "                1
      120 "n   "               1
      121 "n "                 1
      122 "n  "                1
      123 "n   "               1
      124 "n "                 1
      125 "n  "                1
      126 "n   "               1
      127 "n "                 1
      128 "n  "                1
      129 "n   "               1
      130 "n "                 1
      131 "n  "                1
      132 "n   "               1
      133 "n "                 1
      134 "n  "                1
      135 "n   "               1
      136 "n "                 1
      137 "n  "                1
      138 "n   "               1
      139 "n "                 1
      140 "n  "                1
      141 "n   "               1
      142 "n "                 1
      143 "n  "                1
      144 "n   "               1
      145 "n "                 1
      146 "n  "                1
      147 "n "                 1
      148 "n  "                1
      149 "n   "               1
      150 "n "                 1
      151 "n  "                1
      152 "n   "               1
      153 "n   "               1

