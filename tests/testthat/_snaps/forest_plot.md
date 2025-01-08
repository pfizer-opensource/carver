# Test case 1: Forest Plot Base Works with standard inputs

    Code
      fp[[x]]
    Output
      Aesthetic mapping: 
      * `x`      -> `.data[["RISK"]]`
      * `y`      -> `.data[["DPTVAL"]]`
      * `xmin`   -> `.data[["RISKCIL"]]`
      * `xmax`   -> `.data[["RISKCIU"]]`
      * `text`   -> `.data[["HOVER_RISK"]]`
      * `group`  -> `.data[["TRTPAIR"]]`
      * `colour` -> `.data[["TRTPAIR"]]`

---

    Code
      fp[[x]]
    Output
      [[1]]
      geom_errorbarh: na.rm = FALSE, width = 0.1
      stat_identity: na.rm = FALSE
      position_dodgev 
      
      [[2]]
      geom_point: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_dodgev 
      
      [[3]]
      mapping: xintercept = ~xintercept 
      geom_vline: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      
      [[4]]
      mapping: yintercept = ~yintercept 
      geom_hline: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      

---

    Code
      fp[[x]]
    Output
      $axis.title.x
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : num 8
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.title.y
       list()
       - attr(*, "class")= chr [1:2] "element_blank" "element"
      
      $axis.text.x
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : num 8
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.text.y
       list()
       - attr(*, "class")= chr [1:2] "element_blank" "element"
      
      $axis.ticks
       list()
       - attr(*, "class")= chr [1:2] "element_blank" "element"
      
      $axis.line
      List of 6
       $ colour       : chr "black"
       $ linewidth    : NULL
       $ linetype     : NULL
       $ lineend      : NULL
       $ arrow        : logi FALSE
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_line" "element"
      
      $legend.position
      [1] "bottom"
      
      $legend.direction
      [1] "horizontal"
      
      $panel.background
       list()
       - attr(*, "class")= chr [1:2] "element_blank" "element"
      
      $panel.border
      List of 5
       $ fill         : logi NA
       $ colour       : chr "black"
       $ linewidth    : num 1
       $ linetype     : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_rect" "element"
      
      $panel.grid.major
       list()
       - attr(*, "class")= chr [1:2] "element_blank" "element"
      
      $panel.grid.minor
       list()
       - attr(*, "class")= chr [1:2] "element_blank" "element"
      
      $plot.title
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : num 10
       $ hjust        : num 0.1
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $plot.margin
      [1] 0cm 0cm 0cm 0cm
      
      attr(,"complete")
      [1] FALSE
      attr(,"validate")
      [1] TRUE

# Test case 1: Forest Plot Scatter Works with standard inputs

    Code
      sp[[x]]
    Output
      Aesthetic mapping: 
      * `x`      -> `.data[["PCT"]]`
      * `y`      -> `.data[["DPTVAL"]]`
      * `colour` -> `.data[["TRTVAR"]]`
      * `shape`  -> `.data[["TRTVAR"]]`
      * `size`   -> `.data[["TRTVAR"]]`
      * `text`   -> `.data[["HOVER_PCT"]]`

---

    Code
      sp[[x]]
    Output
      [[1]]
      geom_point: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      
      [[2]]
      mapping: yintercept = ~yintercept 
      geom_hline: na.rm = FALSE
      stat_identity: na.rm = FALSE
      position_identity 
      

---

    Code
      sp[[x]]
    Output
      $axis.title.x
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : num 8
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.title.y
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : NULL
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.text.x
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : num 4
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $axis.text.y
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : NULL
       $ hjust        : NULL
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      $legend.background
      List of 5
       $ fill         : NULL
       $ colour       : chr "black"
       $ linewidth    : NULL
       $ linetype     : chr "solid"
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_rect" "element"
      
      $legend.position
      [1] "bottom"
      
      $legend.direction
      [1] "horizontal"
      
      $panel.background
      List of 5
       $ fill         : chr "white"
       $ colour       : chr "black"
       $ linewidth    : NULL
       $ linetype     : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_rect" "element"
      
      $panel.border
      List of 5
       $ fill         : logi NA
       $ colour       : chr "black"
       $ linewidth    : num 1
       $ linetype     : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_rect" "element"
      
      $plot.title
      List of 11
       $ family       : NULL
       $ face         : NULL
       $ colour       : NULL
       $ size         : NULL
       $ hjust        : num 0.5
       $ vjust        : NULL
       $ angle        : NULL
       $ lineheight   : NULL
       $ margin       : NULL
       $ debug        : NULL
       $ inherit.blank: logi FALSE
       - attr(*, "class")= chr [1:2] "element_text" "element"
      
      attr(,"complete")
      [1] FALSE
      attr(,"validate")
      [1] TRUE

# Test Case 1: forest_display interactive works correctly

    Code
      actual$x$layout
    Output
      $xaxis
      $xaxis$domain
      [1] 0.050 0.297
      
      $xaxis$automargin
      [1] TRUE
      
      $xaxis$type
      [1] "linear"
      
      $xaxis$autorange
      [1] FALSE
      
      $xaxis$range
      [1] 0.4 1.6
      
      $xaxis$tickmode
      [1] "array"
      
      $xaxis$ticktext
      [1] "HT"
      
      $xaxis$tickvals
      [1] 1
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      
      $xaxis$categoryorder
      [1] "array"
      
      $xaxis$categoryarray
      [1] "HT"
      
      $xaxis$nticks
      [1] NA
      
      $xaxis$ticks
      [1] ""
      
      $xaxis$tickcolor
      [1] NA
      
      $xaxis$ticklen
      [1] 3.652968
      
      $xaxis$tickwidth
      [1] 0
      
      $xaxis$showticklabels
      [1] FALSE
      
      $xaxis$tickfont
      $xaxis$tickfont$color
      [1] NA
      
      $xaxis$tickfont$family
      [1] NA
      
      $xaxis$tickfont$size
      [1] 0
      
      
      $xaxis$tickangle
      [1] 0
      
      $xaxis$showline
      [1] TRUE
      
      $xaxis$linecolor
      [1] "rgba(255,255,255,1)"
      
      $xaxis$linewidth
      [1] 0.664176
      
      $xaxis$showgrid
      [1] FALSE
      
      $xaxis$gridcolor
      [1] NA
      
      $xaxis$gridwidth
      [1] 0
      
      $xaxis$zeroline
      [1] FALSE
      
      $xaxis$anchor
      [1] "y"
      
      $xaxis$title
      $xaxis$title$text
      [1] ""
      
      $xaxis$title$font
      $xaxis$title$font$color
      [1] "rgba(0,0,0,1)"
      
      $xaxis$title$font$family
      [1] ""
      
      $xaxis$title$font$size
      [1] 10.62682
      
      
      
      $xaxis$hoverformat
      [1] ".2f"
      
      $xaxis$side
      [1] "top"
      
      
      $xaxis2
      $xaxis2$domain
      [1] 0.303 0.677
      
      $xaxis2$automargin
      [1] TRUE
      
      $xaxis2$type
      [1] "linear"
      
      $xaxis2$autorange
      [1] FALSE
      
      $xaxis2$range
      [1] -1.73913 36.52174
      
      $xaxis2$tickmode
      [1] "array"
      
      $xaxis2$ticktext
      [1] "0"  "10" "20" "30"
      
      $xaxis2$tickvals
      [1]  0 10 20 30
      
      $xaxis2$categoryorder
      [1] "array"
      
      $xaxis2$categoryarray
      [1] "0"  "10" "20" "30"
      
      $xaxis2$nticks
      [1] NA
      
      $xaxis2$ticks
      [1] "outside"
      
      $xaxis2$tickcolor
      [1] "rgba(51,51,51,1)"
      
      $xaxis2$ticklen
      [1] 3.652968
      
      $xaxis2$tickwidth
      [1] 0.664176
      
      $xaxis2$showticklabels
      [1] TRUE
      
      $xaxis2$tickfont
      $xaxis2$tickfont$color
      [1] "rgba(77,77,77,1)"
      
      $xaxis2$tickfont$family
      [1] ""
      
      $xaxis2$tickfont$size
      [1] 5.313408
      
      
      $xaxis2$tickangle
      [1] 0
      
      $xaxis2$showline
      [1] FALSE
      
      $xaxis2$linecolor
      [1] NA
      
      $xaxis2$linewidth
      [1] 0
      
      $xaxis2$showgrid
      [1] TRUE
      
      $xaxis2$gridcolor
      [1] "rgba(255,255,255,1)"
      
      $xaxis2$gridwidth
      [1] 0.664176
      
      $xaxis2$zeroline
      [1] FALSE
      
      $xaxis2$anchor
      [1] "y2"
      
      $xaxis2$title
      $xaxis2$title$text
      [1] "Percentage"
      
      $xaxis2$title$font
      $xaxis2$title$font$color
      [1] "rgba(0,0,0,1)"
      
      $xaxis2$title$font$family
      [1] ""
      
      $xaxis2$title$font$size
      [1] 10.62682
      
      
      
      $xaxis2$hoverformat
      [1] ".2f"
      
      $xaxis2$side
      [1] "top"
      
      
      $xaxis3
      $xaxis3$domain
      [1] 0.683 0.950
      
      $xaxis3$automargin
      [1] TRUE
      
      $xaxis3$type
      [1] "linear"
      
      $xaxis3$autorange
      [1] FALSE
      
      $xaxis3$range
      [1] -1.8705 39.2805
      
      $xaxis3$tickmode
      [1] "array"
      
      $xaxis3$ticktext
      [1] "0"  "10" "20" "30"
      
      $xaxis3$tickvals
      [1]  0 10 20 30
      
      $xaxis3$categoryorder
      [1] "array"
      
      $xaxis3$categoryarray
      [1] "0"  "10" "20" "30"
      
      $xaxis3$nticks
      [1] NA
      
      $xaxis3$ticks
      [1] ""
      
      $xaxis3$tickcolor
      [1] NA
      
      $xaxis3$ticklen
      [1] 3.652968
      
      $xaxis3$tickwidth
      [1] 0
      
      $xaxis3$showticklabels
      [1] TRUE
      
      $xaxis3$tickfont
      $xaxis3$tickfont$color
      [1] "rgba(77,77,77,1)"
      
      $xaxis3$tickfont$family
      [1] ""
      
      $xaxis3$tickfont$size
      [1] 10.62682
      
      
      $xaxis3$tickangle
      [1] 0
      
      $xaxis3$showline
      [1] TRUE
      
      $xaxis3$linecolor
      [1] "rgba(0,0,0,1)"
      
      $xaxis3$linewidth
      [1] 0.664176
      
      $xaxis3$showgrid
      [1] FALSE
      
      $xaxis3$gridcolor
      [1] NA
      
      $xaxis3$gridwidth
      [1] 0
      
      $xaxis3$zeroline
      [1] FALSE
      
      $xaxis3$anchor
      [1] "y3"
      
      $xaxis3$title
      $xaxis3$title$text
      [1] "Risk Ratio"
      
      $xaxis3$title$font
      $xaxis3$title$font$color
      [1] "rgba(0,0,0,1)"
      
      $xaxis3$title$font$family
      [1] ""
      
      $xaxis3$title$font$size
      [1] 10.62682
      
      
      
      $xaxis3$hoverformat
      [1] ".2f"
      
      $xaxis3$side
      [1] "top"
      
      
      $yaxis3
      $yaxis3$domain
      [1] 0 1
      
      $yaxis3$automargin
      [1] TRUE
      
      $yaxis3$type
      [1] "linear"
      
      $yaxis3$autorange
      [1] FALSE
      
      $yaxis3$range
      [1]  0.4 36.6
      
      $yaxis3$tickmode
      [1] "array"
      
      $yaxis3$ticktext
       [1] "AGITATION"                              
       [2] "APPLICATION SITE DERMATITIS"            
       [3] "APPLICATION SITE ERYTHEMA"              
       [4] "APPLICATION SITE IRRITATION"            
       [5] "APPLICATION SITE PRURITUS"              
       [6] "APPLICATION SITE VESICLES"              
       [7] "BACK PAIN"                              
       [8] "CONFUSIONAL STATE"                      
       [9] "CONTUSION"                              
      [10] "COUGH"                                  
      [11] "DIARRHOEA"                              
      [12] "DIZZINESS"                              
      [13] "EAR INFECTION"                          
      [14] "ELECTROCARDIOGRAM ST SEGMENT DEPRESSION"
      [15] "ELECTROCARDIOGRAM T WAVE INVERSION"     
      [16] "ERYTHEMA"                               
      [17] "EXCORIATION"                            
      [18] "FATIGUE"                                
      [19] "HEADACHE"                               
      [20] "HYPERHIDROSIS"                          
      [21] "HYPOTENSION"                            
      [22] "INSOMNIA"                               
      [23] "MYOCARDIAL INFARCTION"                  
      [24] "NASAL CONGESTION"                       
      [25] "NASOPHARYNGITIS"                        
      [26] "NAUSEA"                                 
      [27] "OEDEMA PERIPHERAL"                      
      [28] "PRURITUS"                               
      [29] "PYREXIA"                                
      [30] "RASH"                                   
      [31] "SINUS BRADYCARDIA"                      
      [32] "SKIN IRRITATION"                        
      [33] "SOMNOLENCE"                             
      [34] "UPPER RESPIRATORY TRACT INFECTION"      
      [35] "URINARY TRACT INFECTION"                
      [36] "VOMITING"                               
      
      $yaxis3$tickvals
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      
      $yaxis3$categoryorder
      [1] "array"
      
      $yaxis3$categoryarray
       [1] "AGITATION"                              
       [2] "APPLICATION SITE DERMATITIS"            
       [3] "APPLICATION SITE ERYTHEMA"              
       [4] "APPLICATION SITE IRRITATION"            
       [5] "APPLICATION SITE PRURITUS"              
       [6] "APPLICATION SITE VESICLES"              
       [7] "BACK PAIN"                              
       [8] "CONFUSIONAL STATE"                      
       [9] "CONTUSION"                              
      [10] "COUGH"                                  
      [11] "DIARRHOEA"                              
      [12] "DIZZINESS"                              
      [13] "EAR INFECTION"                          
      [14] "ELECTROCARDIOGRAM ST SEGMENT DEPRESSION"
      [15] "ELECTROCARDIOGRAM T WAVE INVERSION"     
      [16] "ERYTHEMA"                               
      [17] "EXCORIATION"                            
      [18] "FATIGUE"                                
      [19] "HEADACHE"                               
      [20] "HYPERHIDROSIS"                          
      [21] "HYPOTENSION"                            
      [22] "INSOMNIA"                               
      [23] "MYOCARDIAL INFARCTION"                  
      [24] "NASAL CONGESTION"                       
      [25] "NASOPHARYNGITIS"                        
      [26] "NAUSEA"                                 
      [27] "OEDEMA PERIPHERAL"                      
      [28] "PRURITUS"                               
      [29] "PYREXIA"                                
      [30] "RASH"                                   
      [31] "SINUS BRADYCARDIA"                      
      [32] "SKIN IRRITATION"                        
      [33] "SOMNOLENCE"                             
      [34] "UPPER RESPIRATORY TRACT INFECTION"      
      [35] "URINARY TRACT INFECTION"                
      [36] "VOMITING"                               
      
      $yaxis3$nticks
      [1] NA
      
      $yaxis3$ticks
      [1] ""
      
      $yaxis3$tickcolor
      [1] NA
      
      $yaxis3$ticklen
      [1] 3.652968
      
      $yaxis3$tickwidth
      [1] 0
      
      $yaxis3$showticklabels
      [1] FALSE
      
      $yaxis3$tickfont
      $yaxis3$tickfont$color
      [1] NA
      
      $yaxis3$tickfont$family
      [1] NA
      
      $yaxis3$tickfont$size
      [1] 0
      
      
      $yaxis3$tickangle
      [1] 0
      
      $yaxis3$showline
      [1] TRUE
      
      $yaxis3$linecolor
      [1] "rgba(0,0,0,1)"
      
      $yaxis3$linewidth
      [1] 0.664176
      
      $yaxis3$showgrid
      [1] FALSE
      
      $yaxis3$gridcolor
      [1] NA
      
      $yaxis3$gridwidth
      [1] 0
      
      $yaxis3$zeroline
      [1] FALSE
      
      $yaxis3$anchor
      [1] "x3"
      
      $yaxis3$hoverformat
      [1] ".2f"
      
      
      $yaxis2
      $yaxis2$domain
      [1] 0 1
      
      $yaxis2$automargin
      [1] TRUE
      
      $yaxis2$type
      [1] "linear"
      
      $yaxis2$autorange
      [1] FALSE
      
      $yaxis2$range
      [1]  0.4 36.6
      
      $yaxis2$tickmode
      [1] "array"
      
      $yaxis2$ticktext
       [1] "AGITATION"                              
       [2] "APPLICATION SITE DERMATITIS"            
       [3] "APPLICATION SITE ERYTHEMA"              
       [4] "APPLICATION SITE IRRITATION"            
       [5] "APPLICATION SITE PRURITUS"              
       [6] "APPLICATION SITE VESICLES"              
       [7] "BACK PAIN"                              
       [8] "CONFUSIONAL STATE"                      
       [9] "CONTUSION"                              
      [10] "COUGH"                                  
      [11] "DIARRHOEA"                              
      [12] "DIZZINESS"                              
      [13] "EAR INFECTION"                          
      [14] "ELECTROCARDIOGRAM ST SEGMENT DEPRESSION"
      [15] "ELECTROCARDIOGRAM T WAVE INVERSION"     
      [16] "ERYTHEMA"                               
      [17] "EXCORIATION"                            
      [18] "FATIGUE"                                
      [19] "HEADACHE"                               
      [20] "HYPERHIDROSIS"                          
      [21] "HYPOTENSION"                            
      [22] "INSOMNIA"                               
      [23] "MYOCARDIAL INFARCTION"                  
      [24] "NASAL CONGESTION"                       
      [25] "NASOPHARYNGITIS"                        
      [26] "NAUSEA"                                 
      [27] "OEDEMA PERIPHERAL"                      
      [28] "PRURITUS"                               
      [29] "PYREXIA"                                
      [30] "RASH"                                   
      [31] "SINUS BRADYCARDIA"                      
      [32] "SKIN IRRITATION"                        
      [33] "SOMNOLENCE"                             
      [34] "UPPER RESPIRATORY TRACT INFECTION"      
      [35] "URINARY TRACT INFECTION"                
      [36] "VOMITING"                               
      
      $yaxis2$tickvals
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      
      $yaxis2$categoryorder
      [1] "array"
      
      $yaxis2$categoryarray
       [1] "AGITATION"                              
       [2] "APPLICATION SITE DERMATITIS"            
       [3] "APPLICATION SITE ERYTHEMA"              
       [4] "APPLICATION SITE IRRITATION"            
       [5] "APPLICATION SITE PRURITUS"              
       [6] "APPLICATION SITE VESICLES"              
       [7] "BACK PAIN"                              
       [8] "CONFUSIONAL STATE"                      
       [9] "CONTUSION"                              
      [10] "COUGH"                                  
      [11] "DIARRHOEA"                              
      [12] "DIZZINESS"                              
      [13] "EAR INFECTION"                          
      [14] "ELECTROCARDIOGRAM ST SEGMENT DEPRESSION"
      [15] "ELECTROCARDIOGRAM T WAVE INVERSION"     
      [16] "ERYTHEMA"                               
      [17] "EXCORIATION"                            
      [18] "FATIGUE"                                
      [19] "HEADACHE"                               
      [20] "HYPERHIDROSIS"                          
      [21] "HYPOTENSION"                            
      [22] "INSOMNIA"                               
      [23] "MYOCARDIAL INFARCTION"                  
      [24] "NASAL CONGESTION"                       
      [25] "NASOPHARYNGITIS"                        
      [26] "NAUSEA"                                 
      [27] "OEDEMA PERIPHERAL"                      
      [28] "PRURITUS"                               
      [29] "PYREXIA"                                
      [30] "RASH"                                   
      [31] "SINUS BRADYCARDIA"                      
      [32] "SKIN IRRITATION"                        
      [33] "SOMNOLENCE"                             
      [34] "UPPER RESPIRATORY TRACT INFECTION"      
      [35] "URINARY TRACT INFECTION"                
      [36] "VOMITING"                               
      
      $yaxis2$nticks
      [1] NA
      
      $yaxis2$ticks
      [1] "outside"
      
      $yaxis2$tickcolor
      [1] "rgba(51,51,51,1)"
      
      $yaxis2$ticklen
      [1] 3.652968
      
      $yaxis2$tickwidth
      [1] 0.664176
      
      $yaxis2$showticklabels
      [1] TRUE
      
      $yaxis2$tickfont
      $yaxis2$tickfont$color
      [1] "rgba(77,77,77,1)"
      
      $yaxis2$tickfont$family
      [1] ""
      
      $yaxis2$tickfont$size
      [1] 11.6895
      
      
      $yaxis2$tickangle
      [1] 0
      
      $yaxis2$showline
      [1] FALSE
      
      $yaxis2$linecolor
      [1] NA
      
      $yaxis2$linewidth
      [1] 0
      
      $yaxis2$showgrid
      [1] TRUE
      
      $yaxis2$gridcolor
      [1] "rgba(255,255,255,1)"
      
      $yaxis2$gridwidth
      [1] 0.664176
      
      $yaxis2$zeroline
      [1] FALSE
      
      $yaxis2$anchor
      [1] "x2"
      
      $yaxis2$hoverformat
      [1] ".2f"
      
      
      $yaxis
      $yaxis$domain
      [1] 0 1
      
      $yaxis$automargin
      [1] TRUE
      
      $yaxis$type
      [1] "linear"
      
      $yaxis$autorange
      [1] FALSE
      
      $yaxis$range
      [1]  0.4 36.6
      
      $yaxis$tickmode
      [1] "array"
      
      $yaxis$ticktext
       [1] "AGITATION"                              
       [2] "APPLICATION SITE DERMATITIS"            
       [3] "APPLICATION SITE ERYTHEMA"              
       [4] "APPLICATION SITE IRRITATION"            
       [5] "APPLICATION SITE PRURITUS"              
       [6] "APPLICATION SITE VESICLES"              
       [7] "BACK PAIN"                              
       [8] "CONFUSIONAL STATE"                      
       [9] "CONTUSION"                              
      [10] "COUGH"                                  
      [11] "DIARRHOEA"                              
      [12] "DIZZINESS"                              
      [13] "EAR INFECTION"                          
      [14] "ELECTROCARDIOGRAM ST SEGMENT DEPRESSION"
      [15] "ELECTROCARDIOGRAM T WAVE INVERSION"     
      [16] "ERYTHEMA"                               
      [17] "EXCORIATION"                            
      [18] "FATIGUE"                                
      [19] "HEADACHE"                               
      [20] "HYPERHIDROSIS"                          
      [21] "HYPOTENSION"                            
      [22] "INSOMNIA"                               
      [23] "MYOCARDIAL INFARCTION"                  
      [24] "NASAL CONGESTION"                       
      [25] "NASOPHARYNGITIS"                        
      [26] "NAUSEA"                                 
      [27] "OEDEMA PERIPHERAL"                      
      [28] "PRURITUS"                               
      [29] "PYREXIA"                                
      [30] "RASH"                                   
      [31] "SINUS BRADYCARDIA"                      
      [32] "SKIN IRRITATION"                        
      [33] "SOMNOLENCE"                             
      [34] "UPPER RESPIRATORY TRACT INFECTION"      
      [35] "URINARY TRACT INFECTION"                
      [36] "VOMITING"                               
      
      $yaxis$tickvals
       [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
      [26] 26 27 28 29 30 31 32 33 34 35 36
      attr(,"class")
      [1] "mapped_discrete" "numeric"        
      
      $yaxis$categoryorder
      [1] "array"
      
      $yaxis$categoryarray
       [1] "AGITATION"                              
       [2] "APPLICATION SITE DERMATITIS"            
       [3] "APPLICATION SITE ERYTHEMA"              
       [4] "APPLICATION SITE IRRITATION"            
       [5] "APPLICATION SITE PRURITUS"              
       [6] "APPLICATION SITE VESICLES"              
       [7] "BACK PAIN"                              
       [8] "CONFUSIONAL STATE"                      
       [9] "CONTUSION"                              
      [10] "COUGH"                                  
      [11] "DIARRHOEA"                              
      [12] "DIZZINESS"                              
      [13] "EAR INFECTION"                          
      [14] "ELECTROCARDIOGRAM ST SEGMENT DEPRESSION"
      [15] "ELECTROCARDIOGRAM T WAVE INVERSION"     
      [16] "ERYTHEMA"                               
      [17] "EXCORIATION"                            
      [18] "FATIGUE"                                
      [19] "HEADACHE"                               
      [20] "HYPERHIDROSIS"                          
      [21] "HYPOTENSION"                            
      [22] "INSOMNIA"                               
      [23] "MYOCARDIAL INFARCTION"                  
      [24] "NASAL CONGESTION"                       
      [25] "NASOPHARYNGITIS"                        
      [26] "NAUSEA"                                 
      [27] "OEDEMA PERIPHERAL"                      
      [28] "PRURITUS"                               
      [29] "PYREXIA"                                
      [30] "RASH"                                   
      [31] "SINUS BRADYCARDIA"                      
      [32] "SKIN IRRITATION"                        
      [33] "SOMNOLENCE"                             
      [34] "UPPER RESPIRATORY TRACT INFECTION"      
      [35] "URINARY TRACT INFECTION"                
      [36] "VOMITING"                               
      
      $yaxis$nticks
      [1] NA
      
      $yaxis$ticks
      [1] ""
      
      $yaxis$tickcolor
      [1] NA
      
      $yaxis$ticklen
      [1] 3.652968
      
      $yaxis$tickwidth
      [1] 0
      
      $yaxis$showticklabels
      [1] FALSE
      
      $yaxis$tickfont
      $yaxis$tickfont$color
      [1] NA
      
      $yaxis$tickfont$family
      [1] NA
      
      $yaxis$tickfont$size
      [1] 0
      
      
      $yaxis$tickangle
      [1] 0
      
      $yaxis$showline
      [1] TRUE
      
      $yaxis$linecolor
      [1] "rgba(255,255,255,1)"
      
      $yaxis$linewidth
      [1] 0.664176
      
      $yaxis$showgrid
      [1] FALSE
      
      $yaxis$gridcolor
      [1] NA
      
      $yaxis$gridwidth
      [1] 0
      
      $yaxis$zeroline
      [1] FALSE
      
      $yaxis$anchor
      [1] "x"
      
      $yaxis$hoverformat
      [1] ".2f"
      
      
      $annotations
      list()
      
      $shapes
      $shapes[[1]]
      $shapes[[1]]$type
      [1] "rect"
      
      $shapes[[1]]$fillcolor
      [1] "transparent"
      
      $shapes[[1]]$line
      $shapes[[1]]$line$color
      [1] "rgba(255,255,255,1)"
      
      $shapes[[1]]$line$width
      [1] 1.328352
      
      $shapes[[1]]$line$linetype
      [1] "solid"
      
      
      $shapes[[1]]$yref
      [1] "paper"
      
      $shapes[[1]]$xref
      [1] "paper"
      
      $shapes[[1]]$x0
      [1] 0.05
      
      $shapes[[1]]$x1
      [1] 0.297
      
      $shapes[[1]]$y0
      [1] 0
      
      $shapes[[1]]$y1
      [1] 1
      
      
      $shapes[[2]]
      $shapes[[2]]$type
      [1] "rect"
      
      $shapes[[2]]$fillcolor
      [1] "transparent"
      
      $shapes[[2]]$line
      $shapes[[2]]$line$color
      [1] "rgba(0,0,0,1)"
      
      $shapes[[2]]$line$width
      [1] 1.328352
      
      $shapes[[2]]$line$linetype
      [1] "solid"
      
      
      $shapes[[2]]$yref
      [1] "paper"
      
      $shapes[[2]]$xref
      [1] "paper"
      
      $shapes[[2]]$x0
      [1] 0.303
      
      $shapes[[2]]$x1
      [1] 0.677
      
      $shapes[[2]]$y0
      [1] 0
      
      $shapes[[2]]$y1
      [1] 1
      
      
      $shapes[[3]]
      $shapes[[3]]$type
      [1] "rect"
      
      $shapes[[3]]$fillcolor
      [1] "transparent"
      
      $shapes[[3]]$line
      $shapes[[3]]$line$color
      [1] "rgba(0,0,0,1)"
      
      $shapes[[3]]$line$width
      [1] 1.328352
      
      $shapes[[3]]$line$linetype
      [1] "solid"
      
      
      $shapes[[3]]$yref
      [1] "paper"
      
      $shapes[[3]]$xref
      [1] "paper"
      
      $shapes[[3]]$x0
      [1] 0.683
      
      $shapes[[3]]$x1
      [1] 0.95
      
      $shapes[[3]]$y0
      [1] 0
      
      $shapes[[3]]$y1
      [1] 1
      
      
      
      $images
      list()
      
      $margin
      $margin$t
      [1] 16
      
      $margin$r
      [1] 0
      
      $margin$b
      [1] 24.9066
      
      $margin$l
      [1] 3.652968
      
      
      $paper_bgcolor
      [1] "rgba(255,255,255,1)"
      
      $font
      $font$color
      [1] "rgba(0,0,0,1)"
      
      $font$family
      [1] ""
      
      $font$size
      [1] 14.61187
      
      
      $showlegend
      [1] TRUE
      
      $legend
      $legend$bgcolor
      [1] "rgba(255,255,255,1)"
      
      $legend$bordercolor
      [1] "transparent"
      
      $legend$borderwidth
      [1] 1.889764
      
      $legend$font
      $legend$font$color
      [1] "rgba(0,0,0,1)"
      
      $legend$font$family
      [1] ""
      
      $legend$font$size
      [1] 8
      
      
      $legend$orientation
      [1] "h"
      
      $legend$x
      [1] 0.3
      
      $legend$y
      [1] -0.2
      
      $legend$size
      [1] 8
      
      $legend$xanchor
      [1] "left"
      
      $legend$yanchor
      [1] "top"
      
      
      $hovermode
      [1] "closest"
      
      $height
      [1] 800
      
      $barmode
      [1] "relative"
      
      $plot_bgcolor
      [1] "rgba(255,255,255,1)"
      

