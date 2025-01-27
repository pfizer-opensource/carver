# Test Case 1: bar_plot works with expected inputs

    Code
      bar_out[[x]]
    Output
      Aesthetic mapping: 
      * `x`     -> `.data[["XVAR"]]`
      * `y`     -> `.data[["YVAR"]]`
      * `fill`  -> `.data[["TRTVAR"]]`
      * `group` -> `.data[["TRTVAR"]]`

---

    Code
      bar_out[[x]]
    Output
      $x
      [1] ""
      
      $y
      [1] ""
      
      $title
      NULL
      
      $fill
      [1] "TRTVAR"
      
      $group
      [1] "TRTVAR"
      

