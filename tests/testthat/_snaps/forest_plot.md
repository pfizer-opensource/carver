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

