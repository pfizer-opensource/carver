# Test case 1: Forest Plot Base Works with standard inputs

    Code
      fp[["mapping"]]
    Output
      Aesthetic mapping: 
      * `x`      -> `.data[["RISK"]]`
      * `y`      -> `.data[["DPTVAL"]]`
      * `xmin`   -> `.data[["RISKCIL"]]`
      * `xmax`   -> `.data[["RISKCIU"]]`
      * `text`   -> `.data[["HOVER_RISK"]]`
      * `group`  -> `.data[["TRTPAIR"]]`
      * `colour` -> `.data[["TRTPAIR"]]`
      * `key`    -> `.data[["key"]]`

# Test case 1: Forest Plot Scatter Works with standard inputs

    Code
      sp[["mapping"]]
    Output
      Aesthetic mapping: 
      * `x`      -> `.data[["PCT"]]`
      * `y`      -> `.data[["DPTVAL"]]`
      * `colour` -> `.data[["TRTVAR"]]`
      * `shape`  -> `.data[["TRTVAR"]]`
      * `size`   -> `.data[["TRTVAR"]]`
      * `text`   -> `.data[["HOVER_PCT"]]`
      * `key`    -> `.data[["key"]]`

