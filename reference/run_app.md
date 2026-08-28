# Use biproportional apportionment interactively in a shiny app

Use biproportional apportionment interactively in a shiny app

## Usage

``` r
run_app(votes_matrix = NULL, district_seats = NULL)
```

## Arguments

- votes_matrix:

  Optional `votes_matrix` to load upon start

- district_seats:

  Optional `district_seats` to load upon start

## Value

Calling the function starts the shiny app

## Examples

``` r
if(interactive()) {
    # You need to have the packages 'shiny' and
    # shinyMatrix' installed to run the app
    run_app()

    # It's possible to load data with the app
    run_app(uri2020$votes_matrix, uri2020$seats_vector)
}
```
