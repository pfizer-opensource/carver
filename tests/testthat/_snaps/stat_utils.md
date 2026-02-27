# custom_cox_ph works as expected

    Code
      print(tibble::as_tibble(cp), n = Inf)
    Output
      # A tibble: 7 x 2
        out$xx   $zy $pred.x  $yhat   $yup  $ylow  pval
         <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>
      1     12  3.26    12     3.26   7.73  -1.21 0.878
      2     12  3.26    39.2 -22.4    1.96 -46.8  0.878
      3     61 -4.13    66.3   9.14  21.7   -3.38 0.878
      4     61 -4.13    93.5 104.   219.   -10.5  0.878
      5     64  2.90   121.  156.   324.   -13.4  0.878
      6    175  2.16   148.  106.   220.    -7.92 0.878
      7    175  2.16   175     2.16   6.62  -2.31 0.878

