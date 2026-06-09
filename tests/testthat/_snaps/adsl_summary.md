# adsl_summary works as expected

    Code
      print(tibble::as_tibble(dataf), n = Inf, width = Inf)
    Output
      # A tibble: 21 x 9
         BYVAR1 DPTVAR    DPTVAL                           DPTVARN DPTVALN CN   
         <chr>  <chr>     <chr>                              <int>   <dbl> <chr>
       1 F      Age Group <65                                    1       1 C    
       2 F      Age Group 65-80                                  1       2 C    
       3 F      Age Group >80                                    1       3 C    
       4 F      Age       N                                      2       1 N    
       5 F      Age       Range                                  2       2 N    
       6 F      Age       Mean (SD)                              2       3 N    
       7 F      Age       Median                                 2       4 N    
       8 F      Age       Interquartile Range                    2       5 N    
       9 F      Race      WHITE                                  3       1 C    
      10 F      Race      BLACK OR AFRICAN AMERICAN              3       2 C    
      11 M      Age Group <65                                    1       1 C    
      12 M      Age Group 65-80                                  1       2 C    
      13 M      Age Group >80                                    1       3 C    
      14 M      Age       N                                      2       1 N    
      15 M      Age       Range                                  2       2 N    
      16 M      Age       Mean (SD)                              2       3 N    
      17 M      Age       Median                                 2       4 N    
      18 M      Age       Interquartile Range                    2       5 N    
      19 M      Race      WHITE                                  3       1 C    
      20 M      Race      BLACK OR AFRICAN AMERICAN              3       2 C    
      21 M      Race      AMERICAN INDIAN OR ALASKA NATIVE       3       6 C    
         `Placebo (N=86)` `Xanomeline Low Dose (N=84)` `Xanomeline High Dose (N=84)`
         <chr>            <chr>                        <chr>                        
       1 9 (10.47%)       5 ( 5.95%)                   5 ( 5.95%)                   
       2 22 (25.58%)      28 (33.33%)                  28 (33.33%)                  
       3 22 (25.58%)      17 (20.24%)                  7 ( 8.33%)                   
       4 53               50                           40                           
       5 (59.00,89.00)    (54.00,87.00)                (56.00,88.00)                
       6 76.36 (8.73)     75.68 (8.09)                 74.67 (7.67)                 
       7 78.00            77.50                        76.00                        
       8 (70.00, 84.00)   (72.00, 81.00)               (72.00, 79.00)               
       9 48 (55.81%)      44 (52.38%)                  34 (40.48%)                  
      10 5 ( 5.81%)       6 ( 7.14%)                   6 ( 7.14%)                   
      11 5 ( 5.81%)       3 ( 3.57%)                   6 ( 7.14%)                   
      12 20 (23.26%)      19 (22.62%)                  27 (32.14%)                  
      13 8 ( 9.30%)       12 (14.29%)                  11 (13.10%)                  
      14 33               34                           44                           
      15 (52.00,85.00)    (51.00,88.00)                (56.00,86.00)                
      16 73.36 (8.15)     75.65 (8.69)                 74.11 (8.16)                 
      17 74.00            77.50                        77.00                        
      18 (69.00, 80.00)   (68.00, 82.00)               (69.00, 80.50)               
      19 30 (34.88%)      34 (40.48%)                  40 (47.62%)                  
      20 3 ( 3.49%)       0                            3 ( 3.57%)                   
      21 0                0                            1 ( 1.19%)                   

---

    Code
      print(tibble::as_tibble(dataf_), n = Inf, width = Inf)
    Output
      # A tibble: 74 x 9
         BYVAR1 DPTVAR    DPTVAL                           DPTVARN DPTVALN CN   
         <chr>  <chr>     <chr>                              <int>   <dbl> <chr>
       1 F      Age Group <65                                    1       1 C    
       2 F      Age Group 65-80                                  1       2 C    
       3 F      Age Group >80                                    1       3 C    
       4 F      Age       54                                     2       3 C    
       5 F      Age       56                                     2       4 C    
       6 F      Age       57                                     2       5 C    
       7 F      Age       59                                     2       6 C    
       8 F      Age       60                                     2       7 C    
       9 F      Age       61                                     2       8 C    
      10 F      Age       62                                     2       9 C    
      11 F      Age       63                                     2      10 C    
      12 F      Age       64                                     2      11 C    
      13 F      Age       66                                     2      13 C    
      14 F      Age       67                                     2      14 C    
      15 F      Age       68                                     2      15 C    
      16 F      Age       69                                     2      16 C    
      17 F      Age       70                                     2      17 C    
      18 F      Age       71                                     2      18 C    
      19 F      Age       72                                     2      19 C    
      20 F      Age       73                                     2      20 C    
      21 F      Age       74                                     2      21 C    
      22 F      Age       75                                     2      22 C    
      23 F      Age       76                                     2      23 C    
      24 F      Age       77                                     2      24 C    
      25 F      Age       78                                     2      25 C    
      26 F      Age       79                                     2      26 C    
      27 F      Age       80                                     2      27 C    
      28 F      Age       81                                     2      28 C    
      29 F      Age       82                                     2      29 C    
      30 F      Age       83                                     2      30 C    
      31 F      Age       84                                     2      31 C    
      32 F      Age       85                                     2      32 C    
      33 F      Age       86                                     2      33 C    
      34 F      Age       87                                     2      34 C    
      35 F      Age       88                                     2      35 C    
      36 F      Age       89                                     2      36 C    
      37 F      Race      WHITE                                  3       1 C    
      38 F      Race      BLACK OR AFRICAN AMERICAN              3       2 C    
      39 M      Age Group <65                                    1       1 C    
      40 M      Age Group 65-80                                  1       2 C    
      41 M      Age Group >80                                    1       3 C    
      42 M      Age       51                                     2       1 C    
      43 M      Age       52                                     2       2 C    
      44 M      Age       56                                     2       4 C    
      45 M      Age       57                                     2       5 C    
      46 M      Age       61                                     2       8 C    
      47 M      Age       62                                     2       9 C    
      48 M      Age       63                                     2      10 C    
      49 M      Age       64                                     2      11 C    
      50 M      Age       65                                     2      12 C    
      51 M      Age       67                                     2      14 C    
      52 M      Age       68                                     2      15 C    
      53 M      Age       69                                     2      16 C    
      54 M      Age       70                                     2      17 C    
      55 M      Age       71                                     2      18 C    
      56 M      Age       72                                     2      19 C    
      57 M      Age       73                                     2      20 C    
      58 M      Age       74                                     2      21 C    
      59 M      Age       75                                     2      22 C    
      60 M      Age       77                                     2      24 C    
      61 M      Age       78                                     2      25 C    
      62 M      Age       79                                     2      26 C    
      63 M      Age       80                                     2      27 C    
      64 M      Age       81                                     2      28 C    
      65 M      Age       82                                     2      29 C    
      66 M      Age       83                                     2      30 C    
      67 M      Age       84                                     2      31 C    
      68 M      Age       85                                     2      32 C    
      69 M      Age       86                                     2      33 C    
      70 M      Age       87                                     2      34 C    
      71 M      Age       88                                     2      35 C    
      72 M      Race      WHITE                                  3       1 C    
      73 M      Race      BLACK OR AFRICAN AMERICAN              3       2 C    
      74 M      Race      AMERICAN INDIAN OR ALASKA NATIVE       3       6 C    
         `Placebo (N=86)` `Xanomeline Low Dose (N=84)` `Xanomeline High Dose (N=84)`
         <chr>            <chr>                        <chr>                        
       1 9 (10.47%)       5 ( 5.95%)                   5 ( 5.95%)                   
       2 22 (25.58%)      28 (33.33%)                  28 (33.33%)                  
       3 22 (25.58%)      17 (20.24%)                  7 ( 8.33%)                   
       4 0                1 (1.19%)                    0                            
       5 0                2 (2.38%)                    2 (2.38%)                    
       6 0                1 (1.19%)                    0                            
       7 2 (2.33%)        0                            0                            
       8 1 (1.16%)        1 (1.19%)                    1 (1.19%)                    
       9 0                0                            1 (1.19%)                    
      10 1 (1.16%)        0                            0                            
      11 2 (2.33%)        0                            1 (1.19%)                    
      12 3 (3.49%)        0                            0                            
      13 1 (1.16%)        0                            0                            
      14 1 (1.16%)        1 (1.19%)                    2 (2.38%)                    
      15 1 (1.16%)        2 (2.38%)                    0                            
      16 1 (1.16%)        0                            1 (1.19%)                    
      17 1 (1.16%)        0                            0                            
      18 1 (1.16%)        3 (3.57%)                    1 (1.19%)                    
      19 1 (1.16%)        3 (3.57%)                    3 (3.57%)                    
      20 2 (2.33%)        1 (1.19%)                    2 (2.38%)                    
      21 2 (2.33%)        3 (3.57%)                    2 (2.38%)                    
      22 0                2 (2.38%)                    2 (2.38%)                    
      23 4 (4.65%)        4 (4.76%)                    4 (4.76%)                    
      24 1 (1.16%)        1 (1.19%)                    3 (3.57%)                    
      25 3 (3.49%)        2 (2.38%)                    3 (3.57%)                    
      26 1 (1.16%)        3 (3.57%)                    3 (3.57%)                    
      27 2 (2.33%)        3 (3.57%)                    2 (2.38%)                    
      28 6 (6.98%)        6 (7.14%)                    1 (1.19%)                    
      29 0                1 (1.19%)                    0                            
      30 2 (2.33%)        3 (3.57%)                    1 (1.19%)                    
      31 3 (3.49%)        5 (5.95%)                    2 (2.38%)                    
      32 2 (2.33%)        0                            1 (1.19%)                    
      33 3 (3.49%)        1 (1.19%)                    1 (1.19%)                    
      34 3 (3.49%)        1 (1.19%)                    0                            
      35 2 (2.33%)        0                            1 (1.19%)                    
      36 1 (1.16%)        0                            0                            
      37 48 (55.81%)      44 (52.38%)                  34 (40.48%)                  
      38 5 ( 5.81%)       6 ( 7.14%)                   6 ( 7.14%)                   
      39 5 ( 5.81%)       3 ( 3.57%)                   6 ( 7.14%)                   
      40 20 (23.26%)      19 (22.62%)                  27 (32.14%)                  
      41 8 ( 9.30%)       12 (14.29%)                  11 (13.10%)                  
      42 0                1 (1.19%)                    0                            
      43 1 (1.16%)        0                            0                            
      44 0                0                            2 (2.38%)                    
      45 1 (1.16%)        0                            1 (1.19%)                    
      46 1 (1.16%)        1 (1.19%)                    2 (2.38%)                    
      47 0                1 (1.19%)                    0                            
      48 0                0                            1 (1.19%)                    
      49 2 (2.33%)        0                            0                            
      50 1 (1.16%)        1 (1.19%)                    2 (2.38%)                    
      51 1 (1.16%)        1 (1.19%)                    2 (2.38%)                    
      52 0                4 (4.76%)                    0                            
      53 2 (2.33%)        1 (1.19%)                    2 (2.38%)                    
      54 3 (3.49%)        0                            1 (1.19%)                    
      55 1 (1.16%)        2 (2.38%)                    1 (1.19%)                    
      56 1 (1.16%)        0                            1 (1.19%)                    
      57 1 (1.16%)        0                            3 (3.57%)                    
      58 3 (3.49%)        1 (1.19%)                    2 (2.38%)                    
      59 2 (2.33%)        1 (1.19%)                    1 (1.19%)                    
      60 1 (1.16%)        3 (3.57%)                    5 (5.95%)                    
      61 2 (2.33%)        2 (2.38%)                    1 (1.19%)                    
      62 1 (1.16%)        2 (2.38%)                    4 (4.76%)                    
      63 1 (1.16%)        1 (1.19%)                    2 (2.38%)                    
      64 2 (2.33%)        1 (1.19%)                    3 (3.57%)                    
      65 2 (2.33%)        3 (3.57%)                    4 (4.76%)                    
      66 1 (1.16%)        1 (1.19%)                    0                            
      67 1 (1.16%)        3 (3.57%)                    2 (2.38%)                    
      68 2 (2.33%)        1 (1.19%)                    0                            
      69 0                0                            2 (2.38%)                    
      70 0                2 (2.38%)                    0                            
      71 0                1 (1.19%)                    0                            
      72 30 (34.88%)      34 (40.48%)                  40 (47.62%)                  
      73 3 ( 3.49%)       0                            3 ( 3.57%)                   
      74 0                0                            1 ( 1.19%)                   

# adsl_summary works with subsets

    Code
      print(actual, n = Inf, width = Inf)
    Output
      # A tibble: 19 x 9
         BYVAR1 DPTVAR    DPTVAL                           DPTVARN DPTVALN CN   
         <chr>  <chr>     <chr>                              <int>   <dbl> <chr>
       1 F      Age Group <65                                    1       1 C    
       2 F      Age       N                                      2       1 N    
       3 F      Age       Range                                  2       2 N    
       4 F      Age       Meansd                                 2       3 N    
       5 F      Age       Median                                 2       4 N    
       6 F      Age       IQR                                    2       5 N    
       7 F      Sex       F                                      3       1 C    
       8 F      Race      WHITE                                  4       1 C    
       9 F      Race      BLACK OR AFRICAN AMERICAN              4       2 C    
      10 M      Age Group <65                                    1       1 C    
      11 M      Age       N                                      2       1 N    
      12 M      Age       Range                                  2       2 N    
      13 M      Age       Meansd                                 2       3 N    
      14 M      Age       Median                                 2       4 N    
      15 M      Age       IQR                                    2       5 N    
      16 M      Sex       M                                      3       2 C    
      17 M      Race      WHITE                                  4       1 C    
      18 M      Race      BLACK OR AFRICAN AMERICAN              4       2 C    
      19 M      Race      AMERICAN INDIAN OR ALASKA NATIVE       4       6 C    
         `Placebo (N=86)` `Xanomeline Low Dose (N=84)` `Xanomeline High Dose (N=84)`
         <chr>            <chr>                        <chr>                        
       1 9 (10.47%)       5 ( 5.95%)                   5 ( 5.95%)                   
       2 22               17                           7                            
       3 (81.00,89.00)    (81.00,87.00)                (81.00,88.00)                
       4 84.45 (2.67)     82.94 (1.85)                 84.43 (2.23)                 
       5 84.50            83.00                        84.00                        
       6 (81.00, 87.00)   (81.00, 84.00)               (83.00, 86.00)               
       7 53 (61.63%)      50 (59.52%)                  40 (47.62%)                  
       8 48 (55.81%)      44 (52.38%)                  34 (40.48%)                  
       9 5 ( 5.81%)       6 ( 7.14%)                   6 ( 7.14%)                   
      10 5 ( 5.81%)       3 ( 3.57%)                   6 ( 7.14%)                   
      11 8                12                           11                           
      12 (81.00,85.00)    (81.00,88.00)                (81.00,86.00)                
      13 82.88 (1.64)     84.08 (2.27)                 82.82 (1.89)                 
      14 82.50            84.00                        82.00                        
      15 (81.50, 84.50)   (82.00, 86.00)               (81.00, 84.00)               
      16 33 (38.37%)      34 (40.48%)                  44 (52.38%)                  
      17 30 (34.88%)      34 (40.48%)                  40 (47.62%)                  
      18 3 ( 3.49%)       0                            3 ( 3.57%)                   
      19 0                0                            1 ( 1.19%)                   

---

    Code
      print(tibble::as_tibble(actual_), n = Inf, width = Inf)
    Output
      # A tibble: 23 x 9
         BYVAR1 DPTVAR    DPTVAL                           DPTVARN DPTVALN CN   
         <chr>  <chr>     <chr>                              <int>   <dbl> <chr>
       1 F      Age Group <65                                    1       1 C    
       2 F      Age Group 65-80                                  1       2 C    
       3 F      Age Group >80                                    1       3 C    
       4 F      Age       N                                      2       1 N    
       5 F      Age       Range                                  2       2 N    
       6 F      Age       Meansd                                 2       3 N    
       7 F      Age       Median                                 2       4 N    
       8 F      Age       IQR                                    2       5 N    
       9 F      Sex       F                                      3       1 C    
      10 F      Race      WHITE                                  4       1 C    
      11 F      Race      BLACK OR AFRICAN AMERICAN              4       2 C    
      12 M      Age Group <65                                    1       1 C    
      13 M      Age Group 65-80                                  1       2 C    
      14 M      Age Group >80                                    1       3 C    
      15 M      Age       N                                      2       1 N    
      16 M      Age       Range                                  2       2 N    
      17 M      Age       Meansd                                 2       3 N    
      18 M      Age       Median                                 2       4 N    
      19 M      Age       IQR                                    2       5 N    
      20 M      Sex       M                                      3       2 C    
      21 M      Race      WHITE                                  4       1 C    
      22 M      Race      BLACK OR AFRICAN AMERICAN              4       2 C    
      23 M      Race      AMERICAN INDIAN OR ALASKA NATIVE       4       6 C    
         `Placebo (N=86)` `Xanomeline Low Dose (N=84)` `Xanomeline High Dose (N=84)`
         <chr>            <chr>                        <chr>                        
       1 9 (11.54%)       5 ( 6.41%)                   5 ( 6.76%)                   
       2 22 (28.21%)      28 (35.90%)                  28 (37.84%)                  
       3 22 (28.21%)      17 (21.79%)                  7 ( 9.46%)                   
       4 53               50                           40                           
       5 (59.00,89.00)    (54.00,87.00)                (56.00,88.00)                
       6 76.36 (8.73)     75.68 (8.09)                 74.67 (7.67)                 
       7 78.00            77.50                        76.00                        
       8 (70.00, 84.00)   (72.00, 81.00)               (72.00, 79.00)               
       9 53 (61.63%)      50 (59.52%)                  40 (47.62%)                  
      10 48 (55.81%)      44 (52.38%)                  34 (40.48%)                  
      11 5 ( 5.81%)       6 ( 7.14%)                   6 ( 7.14%)                   
      12 5 ( 6.41%)       3 ( 3.85%)                   6 ( 8.11%)                   
      13 20 (25.64%)      19 (24.36%)                  27 (36.49%)                  
      14 8 (10.26%)       12 (15.38%)                  11 (14.86%)                  
      15 33               34                           44                           
      16 (52.00,85.00)    (51.00,88.00)                (56.00,86.00)                
      17 73.36 (8.15)     75.65 (8.69)                 74.11 (8.16)                 
      18 74.00            77.50                        77.00                        
      19 (69.00, 80.00)   (68.00, 82.00)               (69.00, 80.50)               
      20 33 (38.37%)      34 (40.48%)                  44 (52.38%)                  
      21 30 (34.88%)      34 (40.48%)                  40 (47.62%)                  
      22 3 ( 3.49%)       0                            3 ( 3.57%)                   
      23 0                0                            1 ( 1.19%)                   

