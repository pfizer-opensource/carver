# Test Case 1: process_event_analysis works with expected inputs

    Code
      print(tibble::as_tibble(x), n = Inf, width = Inf)
    Output
      # A tibble: 9 x 20
        BYVAR1                                 TRTVAR                  DPTVAR 
        <chr>                                  <fct>                   <chr>  
      1 Abdominal Pain/Narrow                  "Placebo"               AEDECOD
      2 Abdominal Pain/Narrow~~Dyspepsia/Broad "Placebo"               AEDECOD
      3 Abdominal Pain/Narrow                  "Placebo"               AEDECOD
      4 Abdominal Pain/Narrow                  "Xanomeline Low\nDose"  AEDECOD
      5 Abdominal Pain/Narrow~~Dyspepsia/Broad "Xanomeline Low\nDose"  AEDECOD
      6 Abdominal Pain/Narrow                  "Xanomeline Low\nDose"  AEDECOD
      7 Abdominal Pain/Narrow                  "Xanomeline High\nDose" AEDECOD
      8 Abdominal Pain/Narrow                  "Xanomeline High\nDose" AEDECOD
      9 Abdominal Pain/Narrow~~Dyspepsia/Broad "Xanomeline High\nDose" AEDECOD
        DPTVAL               CVALUE     DENOMN  FREQ DPTVALN BYVAR1N   PCT CPCT   
        <chr>                <chr>       <int> <int>   <dbl>   <dbl> <dbl> <chr>  
      1 ABDOMINAL PAIN       1 ( 0.70%)    142     1       2       1 0.704 " 0.70"
      2 ABDOMINAL DISCOMFORT 0             142     0       1       2 0     " 0.00"
      3 STOMACH DISCOMFORT   0             142     0     130       1 0     " 0.00"
      4 ABDOMINAL PAIN       3 ( 1.45%)    207     3       2       1 1.45  " 1.45"
      5 ABDOMINAL DISCOMFORT 0             207     0       1       2 0     " 0.00"
      6 STOMACH DISCOMFORT   0             207     0     130       1 0     " 0.00"
      7 ABDOMINAL PAIN       1 ( 0.45%)    222     1       2       1 0.450 " 0.45"
      8 STOMACH DISCOMFORT   1 ( 0.45%)    222     1     130       1 0.450 " 0.45"
      9 ABDOMINAL DISCOMFORT 1 ( 0.45%)    222     1       1       2 0.450 " 0.45"
        XVAR                 DPTVARN CN    HTERM   HVAL           LVAL                
        <chr>                  <dbl> <chr> <chr>   <chr>          <chr>               
      1 ABDOMINAL PAIN             1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      2 ABDOMINAL DISCOMFORT       1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      3 STOMACH DISCOMFORT         1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      4 ABDOMINAL PAIN             1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      5 ABDOMINAL DISCOMFORT       1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      6 STOMACH DISCOMFORT         1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      7 ABDOMINAL PAIN             1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      8 STOMACH DISCOMFORT         1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
      9 ABDOMINAL DISCOMFORT       1 C     FMQ_NAM ABDOMINAL PAIN ABDOMINAL DISCOMFORT
        Percent                                     PCT_N DECODh
        <chr>                                       <dbl>  <dbl>
      1 " 0.70 % \n Low Term: ABDOMINAL PAIN"       0.704    3  
      2 " 0.00 % \n Low Term: ABDOMINAL DISCOMFORT" 0     9999  
      3 " 0.00 % \n Low Term: STOMACH DISCOMFORT"   0        1.5
      4 " 1.45 % \n Low Term: ABDOMINAL PAIN"       1.45     3  
      5 " 0.00 % \n Low Term: ABDOMINAL DISCOMFORT" 0     9999  
      6 " 0.00 % \n Low Term: STOMACH DISCOMFORT"   0        1.5
      7 " 0.45 % \n Low Term: ABDOMINAL PAIN"       0.450    2  
      8 " 0.45 % \n Low Term: STOMACH DISCOMFORT"   0.450    2  
      9 " 0.45 % \n Low Term: ABDOMINAL DISCOMFORT" 0.450 9999  

---

    Code
      print(tibble::as_tibble(x), n = Inf, width = Inf)
    Output
      # A tibble: 3 x 16
        BYVAR1                                 TRTVAR                  DPTVAR 
        <chr>                                  <fct>                   <chr>  
      1 Abdominal Pain/Narrow~~Dyspepsia/Broad "Placebo"               AEDECOD
      2 Abdominal Pain/Narrow~~Dyspepsia/Broad "Xanomeline Low\nDose"  AEDECOD
      3 Abdominal Pain/Narrow~~Dyspepsia/Broad "Xanomeline High\nDose" AEDECOD
        DPTVAL               CVALUE     DENOMN  FREQ DPTVALN BYVAR1N   PCT CPCT   
        <chr>                <chr>       <int> <int>   <dbl>   <dbl> <dbl> <chr>  
      1 ABDOMINAL DISCOMFORT 0             142     0       1       2 0     " 0.00"
      2 ABDOMINAL DISCOMFORT 0             207     0       1       2 0     " 0.00"
      3 ABDOMINAL DISCOMFORT 1 ( 0.45%)    222     1       1       2 0.450 " 0.45"
        XVAR                 DPTVARN CN    PCT_N Percent  
        <chr>                  <dbl> <chr> <dbl> <chr>    
      1 ABDOMINAL DISCOMFORT       1 C     0     " 0.00 %"
      2 ABDOMINAL DISCOMFORT       1 C     0     " 0.00 %"
      3 ABDOMINAL DISCOMFORT       1 C     0.450 " 0.45 %"

# Test Case 2: event_analysis_plot works with expected inputs

    Code
      plot$x$data
    Output
      [[1]]
      [[1]]$orientation
      [1] "v"
      
      [[1]]$width
      [1] 0.4 0.4 0.4
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[1]]$base
      [1] 0 0 0
      attr(,"apiSrc")
      [1] TRUE
      
      [[1]]$x
      [1] 1 2 3
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[1]]$y
      [1] 0.0000000 0.0000000 0.4504505
      attr(,"apiSrc")
      [1] TRUE
      
      [[1]]$text
      [1] " 0.00 %" " 0.00 %" " 0.45 %"
      attr(,"apiSrc")
      [1] TRUE
      
      [[1]]$type
      [1] "bar"
      
      [[1]]$textposition
      [1] "none"
      
      [[1]]$marker
      [[1]]$marker$autocolorscale
      [1] FALSE
      
      [[1]]$marker$color
      [1] "rgba(58,95,205,1)"
      
      [[1]]$marker$line
      [[1]]$marker$line$width
      [1] 1.511811
      
      [[1]]$marker$line$color
      [1] "rgba(96,96,96,1)"
      
      
      
      [[1]]$showlegend
      [1] FALSE
      
      [[1]]$xaxis
      [1] "x"
      
      [[1]]$yaxis
      [1] "y"
      
      [[1]]$hoverinfo
      [1] "text"
      
      [[1]]$name
      [1] ""
      
      
      [[2]]
      [[2]]$orientation
      [1] "v"
      
      [[2]]$width
      [1] 0.4 0.4 0.4
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[2]]$base
      [1] 0 0 0
      attr(,"apiSrc")
      [1] TRUE
      
      [[2]]$x
      [1] 1 2 3
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[2]]$y
      [1] 0.0000000 0.0000000 0.4504505
      attr(,"apiSrc")
      [1] TRUE
      
      [[2]]$text
      [1] " 0.00 % <br /> Low Term: ABDOMINAL DISCOMFORT"
      [2] " 0.00 % <br /> Low Term: ABDOMINAL DISCOMFORT"
      [3] " 0.45 % <br /> Low Term: ABDOMINAL DISCOMFORT"
      attr(,"apiSrc")
      [1] TRUE
      
      [[2]]$type
      [1] "bar"
      
      [[2]]$textposition
      [1] "none"
      
      [[2]]$marker
      [[2]]$marker$autocolorscale
      [1] FALSE
      
      [[2]]$marker$color
      [1] "rgba(58,95,205,1)"
      
      [[2]]$marker$line
      [[2]]$marker$line$width
      [1] 1.511811
      
      [[2]]$marker$line$color
      [1] "rgba(96,96,96,1)"
      
      
      
      [[2]]$name
      [1] "ABDOMINAL DISCOMFORT"
      
      [[2]]$legendgroup
      [1] "ABDOMINAL DISCOMFORT"
      
      [[2]]$showlegend
      [1] TRUE
      
      [[2]]$xaxis
      [1] "x2"
      
      [[2]]$yaxis
      [1] "y2"
      
      [[2]]$hoverinfo
      [1] "text"
      
      
      [[3]]
      [[3]]$orientation
      [1] "v"
      
      [[3]]$width
      [1] 0.4 0.4 0.4
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[3]]$base
      [1] 0.0000000 0.0000000 0.4504505
      attr(,"apiSrc")
      [1] TRUE
      
      [[3]]$x
      [1] 1 2 3
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[3]]$y
      [1] 0.7042254 1.4492754 0.4504505
      attr(,"apiSrc")
      [1] TRUE
      
      [[3]]$text
      [1] " 0.70 % <br /> Low Term: ABDOMINAL PAIN"
      [2] " 1.45 % <br /> Low Term: ABDOMINAL PAIN"
      [3] " 0.45 % <br /> Low Term: ABDOMINAL PAIN"
      attr(,"apiSrc")
      [1] TRUE
      
      [[3]]$type
      [1] "bar"
      
      [[3]]$textposition
      [1] "none"
      
      [[3]]$marker
      [[3]]$marker$autocolorscale
      [1] FALSE
      
      [[3]]$marker$color
      [1] "rgba(248,118,109,1)"
      
      [[3]]$marker$line
      [[3]]$marker$line$width
      [1] 1.511811
      
      [[3]]$marker$line$color
      [1] "rgba(96,96,96,1)"
      
      
      
      [[3]]$name
      [1] "ABDOMINAL PAIN"
      
      [[3]]$legendgroup
      [1] "ABDOMINAL PAIN"
      
      [[3]]$showlegend
      [1] TRUE
      
      [[3]]$xaxis
      [1] "x2"
      
      [[3]]$yaxis
      [1] "y2"
      
      [[3]]$hoverinfo
      [1] "text"
      
      
      [[4]]
      [[4]]$orientation
      [1] "v"
      
      [[4]]$width
      [1] 0.4 0.4 0.4
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[4]]$base
      [1] 0.7042254 1.4492754 0.9009009
      attr(,"apiSrc")
      [1] TRUE
      
      [[4]]$x
      [1] 1 2 3
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      attr(,"apiSrc")
      [1] TRUE
      
      [[4]]$y
      [1] 0.0000000 0.0000000 0.4504505
      attr(,"apiSrc")
      [1] TRUE
      
      [[4]]$text
      [1] " 0.00 % <br /> Low Term: STOMACH DISCOMFORT"
      [2] " 0.00 % <br /> Low Term: STOMACH DISCOMFORT"
      [3] " 0.45 % <br /> Low Term: STOMACH DISCOMFORT"
      attr(,"apiSrc")
      [1] TRUE
      
      [[4]]$type
      [1] "bar"
      
      [[4]]$textposition
      [1] "none"
      
      [[4]]$marker
      [[4]]$marker$autocolorscale
      [1] FALSE
      
      [[4]]$marker$color
      [1] "rgba(0,191,196,1)"
      
      [[4]]$marker$line
      [[4]]$marker$line$width
      [1] 1.511811
      
      [[4]]$marker$line$color
      [1] "rgba(96,96,96,1)"
      
      
      
      [[4]]$name
      [1] "STOMACH DISCOMFORT"
      
      [[4]]$legendgroup
      [1] "STOMACH DISCOMFORT"
      
      [[4]]$showlegend
      [1] TRUE
      
      [[4]]$xaxis
      [1] "x2"
      
      [[4]]$yaxis
      [1] "y2"
      
      [[4]]$hoverinfo
      [1] "text"
      
      

