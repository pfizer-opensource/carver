# tbl_processor works standard

    Code
      print(tbl_data, n = Inf, width = Inf)
    Output
      # A tibble: 26 x 9
         BYVAR1                 DPTVAL                                 DPTVARN DPTVALN
         <chr>                  <chr>                                    <int>   <dbl>
       1 HISPANIC OR LATINO     "Age (Years), n (%)"                         1       0
       2 HISPANIC OR LATINO     "\t\t<65"                                    1       1
       3 HISPANIC OR LATINO     "\t\t65-80"                                  1       2
       4 HISPANIC OR LATINO     "\t\t>80"                                    1       3
       5 HISPANIC OR LATINO     "NONE"                                       2       0
       6 HISPANIC OR LATINO     "\t\tn"                                      2       1
       7 HISPANIC OR LATINO     "\t\tMean (SD)"                              2       2
       8 HISPANIC OR LATINO     "Gender, n (%)"                              3       0
       9 HISPANIC OR LATINO     "\t\tF"                                      3       1
      10 HISPANIC OR LATINO     "\t\tM"                                      3       2
      11 HISPANIC OR LATINO     "Race, n (%)"                                4       0
      12 HISPANIC OR LATINO     "\t\tWHITE"                                  4       1
      13 NOT HISPANIC OR LATINO "Age (Years), n (%)"                         1       0
      14 NOT HISPANIC OR LATINO "\t\t<65"                                    1       1
      15 NOT HISPANIC OR LATINO "\t\t65-80"                                  1       2
      16 NOT HISPANIC OR LATINO "\t\t>80"                                    1       3
      17 NOT HISPANIC OR LATINO "NONE"                                       2       0
      18 NOT HISPANIC OR LATINO "\t\tn"                                      2       1
      19 NOT HISPANIC OR LATINO "\t\tMean (SD)"                              2       2
      20 NOT HISPANIC OR LATINO "Gender, n (%)"                              3       0
      21 NOT HISPANIC OR LATINO "\t\tF"                                      3       1
      22 NOT HISPANIC OR LATINO "\t\tM"                                      3       2
      23 NOT HISPANIC OR LATINO "Race, n (%)"                                4       0
      24 NOT HISPANIC OR LATINO "\t\tWHITE"                                  4       1
      25 NOT HISPANIC OR LATINO "\t\tBLACK OR AFRICAN AMERICAN"              4       2
      26 NOT HISPANIC OR LATINO "\t\tAMERICAN INDIAN OR ALASKA NATIVE"       4       6
         CN    `Placebo (N=86)` `Xanomeline Low Dose (N=84)`
         <chr> <chr>            <chr>                       
       1 <NA>  <NA>             <NA>                        
       2 C     2 ( 2.33%)       2 ( 2.38%)                  
       3 C     0                2 ( 2.38%)                  
       4 C     1 ( 1.16%)       2 ( 2.38%)                  
       5 <NA>  <NA>             <NA>                        
       6 N     3                6                           
       7 N     71.00 (13.00)    70.67 (12.63)               
       8 <NA>  <NA>             <NA>                        
       9 C     2 ( 2.33%)       4 ( 4.76%)                  
      10 C     1 ( 1.16%)       2 ( 2.38%)                  
      11 <NA>  <NA>             <NA>                        
      12 C     3 ( 3.49%)       6 ( 7.14%)                  
      13 <NA>  <NA>             <NA>                        
      14 C     12 (13.95%)      6 ( 7.14%)                  
      15 C     42 (48.84%)      45 (53.57%)                 
      16 C     29 (33.72%)      27 (32.14%)                 
      17 <NA>  <NA>             <NA>                        
      18 N     83               78                          
      19 N     75.36 (8.47)     76.05 (7.85)                
      20 <NA>  <NA>             <NA>                        
      21 C     51 (59.30%)      46 (54.76%)                 
      22 C     32 (37.21%)      32 (38.10%)                 
      23 <NA>  <NA>             <NA>                        
      24 C     75 (87.21%)      72 (85.71%)                 
      25 C     8 ( 9.30%)       6 ( 7.14%)                  
      26 C     0                0                           
         `Xanomeline High Dose (N=84)` `Total (N=254)`
         <chr>                         <chr>          
       1 <NA>                          <NA>           
       2 3 ( 3.57%)                    7 ( 2.76%)     
       3 0                             2 ( 0.79%)     
       4 0                             3 ( 1.18%)     
       5 <NA>                          <NA>           
       6 3                             12             
       7 58.33 (4.04)                  67.67 (11.74)  
       8 <NA>                          <NA>           
       9 1 ( 1.19%)                    7 ( 2.76%)     
      10 2 ( 2.38%)                    5 ( 1.97%)     
      11 <NA>                          <NA>           
      12 3 ( 3.57%)                    12 ( 4.72%)    
      13 <NA>                          <NA>           
      14 8 ( 9.52%)                    26 (10.24%)    
      15 55 (65.48%)                   142 (55.91%)   
      16 18 (21.43%)                   74 (29.13%)    
      17 <NA>                          <NA>           
      18 81                            242            
      19 74.98 (7.36)                  75.45 (7.89)   
      20 <NA>                          <NA>           
      21 39 (46.43%)                   136 (53.54%)   
      22 42 (50.00%)                   106 (41.73%)   
      23 <NA>                          <NA>           
      24 71 (84.52%)                   218 (85.83%)   
      25 9 (10.71%)                    23 ( 9.06%)    
      26 1 ( 1.19%)                    1 ( 0.39%)     

# tbl_processor works without trt/dpt

    Code
      print(tbl_data1, n = Inf, width = Inf)
    Output
      # A tibble: 2 x 5
        BYVAR1                 DPTVALN DPTVARN CN    `Participants, n (%) (N = 254)`
        <chr>                    <dbl>   <dbl> <chr> <chr>                          
      1 HISPANIC OR LATINO           1       1 C     12 ( 4.72%)                    
      2 NOT HISPANIC OR LATINO       1       1 C     242 (95.28%)                   

# Empty_tbl works

    Code
      tbl_empty
    Output
      a flextable object.
      col_keys: `X` 
      header has 1 row(s) 
      body has 1 row(s) 
      original dataset sample: 
      [1] "No participant meets the reporting criteria"

