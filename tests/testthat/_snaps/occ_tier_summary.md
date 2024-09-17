# Standard inputs for occ_tier works

    Code
      output
    Output
      # A tibble: 84 x 13
         TRTVAR    DPTVAR DPTVAL CVALUE DENOMN  FREQ   PCT XVAR  CN    DPTVARN DPTVALN
         <ord>     <chr>  <chr>  <glue>  <int> <dbl> <dbl> <chr> <chr>   <int>   <dbl>
       1 Placebo   TIER   RESPI~ 1 ( 1~     52     1  1.92 RESP~ C           1       0
       2 Placebo   TIER   CARDI~ 2 ( 3~     52     2  3.85 CARD~ C           2       0
       3 Placebo   TIER   NERVO~ 3 ( 5~     52     3  5.77 NERV~ C           3       0
       4 Placebo   TIER   INFEC~ 8 (15~     52     8 15.4  INFE~ C           4       0
       5 Placebo   TIER   GASTR~ 12 (2~     52    12 23.1  GAST~ C           5       0
       6 Placebo   TIER   GENER~ 16 (3~     52    16 30.8  GENE~ C           6       0
       7 Placebo   TIER   SKIN ~ 17 (3~     52    17 32.7  SKIN~ C           7       0
       8 Xanomeli~ TIER   CARDI~ 5 ( 7~     69     5  7.25 CARD~ C           2       0
       9 Xanomeli~ TIER   GASTR~ 6 ( 8~     69     6  8.7  GAST~ C           5       0
      10 Xanomeli~ TIER   GENER~ 38 (5~     69    38 55.1  GENE~ C           6       0
      # i 74 more rows
      # i 2 more variables: SUBGRPVARX <chr>, SUBGRPVARXN <dbl>

---

    Code
      output
    Output
      # A tibble: 84 x 13
         TRTVAR    DPTVAR DPTVAL CVALUE DENOMN  FREQ   PCT XVAR  CN    DPTVARN DPTVALN
         <ord>     <chr>  <chr>  <glue>  <int> <dbl> <dbl> <chr> <chr>   <int>   <dbl>
       1 Placebo   TIER   RESPI~ 1 ( 1~     52     1  1.92 RESP~ C           1       0
       2 Placebo   TIER   CARDI~ 2 ( 3~     52     2  3.85 CARD~ C           2       0
       3 Placebo   TIER   NERVO~ 3 ( 5~     52     3  5.77 NERV~ C           3       0
       4 Placebo   TIER   INFEC~ 8 (15~     52     8 15.4  INFE~ C           4       0
       5 Placebo   TIER   GASTR~ 12 (2~     52    12 23.1  GAST~ C           5       0
       6 Placebo   TIER   GENER~ 16 (3~     52    16 30.8  GENE~ C           6       0
       7 Placebo   TIER   SKIN ~ 17 (3~     52    17 32.7  SKIN~ C           7       0
       8 Xanomeli~ TIER   CARDI~ 5 ( 7~     69     5  7.25 CARD~ C           2       0
       9 Xanomeli~ TIER   GASTR~ 6 ( 8~     69     6  8.7  GAST~ C           5       0
      10 Xanomeli~ TIER   GENER~ 38 (5~     69    38 55.1  GENE~ C           6       0
      # i 74 more rows
      # i 2 more variables: SUBGRPVARX <chr>, SUBGRPVARXN <dbl>

# Cut off applied for occ_tier works

    Code
      output
    Output
      # A tibble: 102 x 13
         TRTVAR    DPTVAR DPTVAL CVALUE DENOMN  FREQ   PCT XVAR  CN    DPTVARN DPTVALN
         <ord>     <chr>  <chr>  <glue>  <int> <dbl> <dbl> <chr> <chr>   <int>   <dbl>
       1 Placebo   TIER   CARDI~ 9 (13~     69     9  13.0 CARD~ C           1       0
       2 Xanomeli~ TIER   CARDI~ 10 (1~     77    10  13.0 CARD~ C           1       0
       3 Xanomeli~ TIER   CARDI~ 10 (1~     79    10  12.7 CARD~ C           1       0
       4 Placebo   TIER   GASTR~ 16 (2~     69    16  23.2 GAST~ C           2       0
       5 Xanomeli~ TIER   GASTR~ 13 (1~     77    13  16.9 GAST~ C           2       0
       6 Xanomeli~ TIER   GASTR~ 15 (1~     79    15  19.0 GAST~ C           2       0
       7 Placebo   TIER   GENER~ 21 (3~     69    21  30.4 GENE~ C           3       0
       8 Xanomeli~ TIER   GENER~ 45 (5~     77    45  58.4 GENE~ C           3       0
       9 Xanomeli~ TIER   GENER~ 35 (4~     79    35  44.3 GENE~ C           3       0
      10 Placebo   TIER   INFEC~ 16 (2~     69    16  23.2 INFE~ C           4       0
      # i 92 more rows
      # i 2 more variables: SUBGRPVARX <chr>, SUBGRPVARXN <dbl>

