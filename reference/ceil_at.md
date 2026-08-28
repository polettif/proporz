# Rounding with predefined thresholds

Round `x` up to `ceiling(x)` if `x-floor(x) >= threshold`, otherwise
round down to `floor(x)`. Used for all divisor methods.

## Usage

``` r
ceil_at(x, threshold)
```

## Arguments

- x:

  Numeric atomic vector or matrix with values \>= 0 (`NaN` is not
  supported)

- threshold:

  Numeric threshold value in \[0,1\] or "harmonic"/"geometric" to use
  harmonic or geometric mean thresholds

## Value

Rounded vector or matrix

## Examples

``` r
ceil_at(c(0.5, 1.5, 2.49, 2.5, 2.51), 0.5)
#> [1] 1 2 2 3 3
# compare to
round(c(0.5, 1.5, 2.49, 2.5, 2.51))
#> [1] 0 2 2 2 3

ceil_at(c(1.45, 2.45, 3.45), 0) # like floor()
#> [1] 2 3 4
ceil_at(c(1.45, 2.45, 3.45, 0.2), "geometric")
#> [1] 2 3 3 1
```
