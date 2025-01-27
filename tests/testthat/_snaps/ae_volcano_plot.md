# Test 1: Volcano plot with standard inputs

    Code
      volcano_test[["mapping"]]
    Output
      Aesthetic mapping: 
      * `x`    -> `.data[["RISK"]]`
      * `y`    -> `.data[["PVALUE"]]`
      * `text` -> `.data[["HOVER_TEXT"]]`
      * `fill` -> `.data[["BYVAR1"]]`
      * `key`  -> `.data[["key"]]`

---

    Code
      volcano_test[["labels"]]
    Output
      $x
      [1] "RISK"
      
      $y
      [1] "PVALUE"
      
      $text
      [1] "HOVER_TEXT"
      
      $fill
      [1] "BYVAR1"
      
      $key
      [1] "key"
      
      $size
      [1] "CTRL_N"
      
      $yintercept
      [1] "yintercept"
      
      $xintercept
      [1] "xintercept"
      

---

    Code
      x$aes_params
    Output
      $shape
      [1] 21
      
      $alpha
      [1] 0.5
      

---

    Code
      x$aes_params
    Output
      $colour
      [1] "grey30"
      
      $linetype
      [1] "dashed"
      

---

    Code
      x$aes_params
    Output
      $colour
      [1] "grey30"
      
      $linetype
      [1] "dashed"
      

---

    Code
      x$aes_params
    Output
      $colour
      [1] "grey30"
      
      $linetype
      [1] "dotted"
      

