# Test Case 1: bar_plot works with expected inputs

    Code
      bar_out[["mapping"]]
    Output
      Aesthetic mapping: 
      * `x`     -> `.data[["XVAR"]]`
      * `y`     -> `.data[["YVAR"]]`
      * `fill`  -> `.data[["TRTVAR"]]`
      * `group` -> `.data[["TRTVAR"]]`

# Test Case 2: bar_plot works with modified inputs

    Code
      bar_out[["mapping"]]
    Output
      Aesthetic mapping: 
      * `colour` -> `.data[["TRTVAR"]]`
      * `x`      -> `.data[["XVAR"]]`
      * `y`      -> `.data[["YVAR"]]`
      * `fill`   -> `.data[["TRTVAR"]]`
      * `group`  -> `.data[["TRTVAR"]]`

