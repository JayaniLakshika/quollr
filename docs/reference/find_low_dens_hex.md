# Find low-density Hexagons

This function identifies hexagons with low density based on the mean
density of their neighboring hexagons.

## Usage

``` r
find_low_dens_hex(model_2d, b1 = 30, md_thresh = 0.05)
```

## Arguments

- model_2d:

  The tibble that contains all hexagonal bin centroids.

- b1:

  Number of bins along the x-axis for hexagon binning.

- md_thresh:

  A numeric value that contains the threshold for mean density.

## Value

A vector containing the IDs of hexagons to be removed after
investigating their neighboring bins.

## Examples

``` r
find_low_dens_hex(model_2d = scurve_model_obj$model_2d, b1 = 30,
md_thresh = 0.05)
#>   [1]  34  35  36  37  38  41  42  49  50  51  52  53  54  55  56  57  64  65
#>  [19]  66  67  68  69  70  71  72  73  78  79  80  81  82  83  84  85  86  87
#>  [37]  88  94  95  96  97 100 101 102 103 104 109 110 111 115 116 117 118 130
#>  [55] 131 132 133 134 145 146 147 148 149 160 161 162 163 164 173 174 175 176
#>  [73] 177 178 184 185 186 187 188 189 190 191 192 193 197 198 199 200 201 202
#>  [91] 203 204 205 206 207 212 213 214 215 216 217 218 219 220 221 222 227 228
#> [109] 229 230 231 232 233 234 235 236 242 243 244 245 246 247 248 249 250 257
#> [127] 258 259 260 261 262 263 273 274 275 276 277
```
