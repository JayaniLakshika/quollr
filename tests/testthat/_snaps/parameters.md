# compute_mean_density_hex() works

    Code
      compute_mean_density_hex(model_2d = scurve_model_obj$model_2d, b1 = 4)
    Condition
      Warning in `compute_mean_density_hex()`:
      There are hexagonal bins that don't have any neighbouring bins.
    Output
      # A tibble: 26 x 2
         hb_id mean_density
         <int>        <dbl>
       1    42     NaN     
       2    49       0.012 
       3    52       0.012 
       4    53       0.0113
       5    56       0.0115
       6    57       0.0117
       7    67     NaN     
       8    69       0.011 
       9    70       0.014 
      10    86     NaN     
      # i 16 more rows

# find_low_dens_hex() works

    Code
      find_low_dens_hex(model_2d = scurve_model_obj$model_2d, b1 = 4,
      benchmark_mean_dens = 0.05)
    Condition
      Warning in `compute_mean_density_hex()`:
      There are hexagonal bins that don't have any neighbouring bins.
    Output
       [1]  49  52  53  56  57  69  70 197 200 202 205 222 227 232 258 259

---

    Code
      find_low_dens_hex(model_2d = scurve_model_obj$model_2d, b1 = 4,
      benchmark_mean_dens = 0.1)
    Condition
      Warning in `compute_mean_density_hex()`:
      There are hexagonal bins that don't have any neighbouring bins.
    Output
       [1]  49  52  53  56  57  69  70 197 200 202 205 222 227 232 258 259

