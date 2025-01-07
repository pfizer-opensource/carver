# empty_plot works as expected

    Code
      exp_ptly_obj
    Output
      [[1]]
      [[1]]$x
      [1] 1
      
      [[1]]$y
      [1] 1
      
      [[1]]$text
      [1] "No data available for these values"
      
      [[1]]$hovertext
      [1] "x: 1<br />y: 1"
      
      [[1]]$textfont
      [[1]]$textfont$size
      [1] 30.23622
      
      [[1]]$textfont$color
      [1] "rgba(0,0,0,1)"
      
      
      [[1]]$type
      [1] "scatter"
      
      [[1]]$mode
      [1] "text"
      
      [[1]]$hoveron
      [1] "points"
      
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
      
      

# theme_cleany works as expected

    Code
      actual
    Output
      List of 14
       $ axis.title.x    :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y    : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.text.x     :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 6
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y     : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.ticks      : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line       :List of 6
        ..$ colour       : chr "black"
        ..$ linewidth    : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ legend.position : chr "bottom"
       $ legend.direction: chr "horizontal"
       $ panel.background: list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.border    :List of 5
        ..$ fill         : logi NA
        ..$ colour       : chr "black"
        ..$ linewidth    : num 1
        ..$ linetype     : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.grid.major: list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.grid.minor: list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ plot.title      :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 10
        ..$ hjust        : num 0.1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.margin     : 'simpleUnit' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi FALSE
       - attr(*, "validate")= logi TRUE

# tbl_to_plot works as expected

    Code
      fig[[x]]
    Output
      Aesthetic mapping: 
      * `x`      -> `.data[["CYL"]]`
      * `y`      -> `.data[["manufacturer"]]`
      * `label`  -> `.data[["HWY"]]`
      * `colour` -> `.data[["CYL"]]`

---

    Code
      fig[[x]]
    Output
      $x
      [1] "CYL"
      
      $y
      [1] "manufacturer"
      
      $label
      [1] "HWY"
      
      $colour
      [1] "CYL"
      

