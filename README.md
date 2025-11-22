
# sparsevector

<figure>
<img
src="https://github.com/anthonyangg/sparsevector/actions/workflows/R-CMD-check.yaml/badge.svg"
alt="R-CMD-check" />
<figcaption aria-hidden="true">R-CMD-check</figcaption>
</figure>

`sparsevector` provides a **sparse numeric vector class
(`sparse_numeric`)** and methods for efficient computation with
mostly-zero vectors. Operations include addition, subtraction,
multiplication, crossproduct, sum, norm, and standardization.

This package is especially useful for handling **high-dimensional
vectors** with many zeros while saving memory and computation time.

------------------------------------------------------------------------

## Installation

You can install the development version directly from GitHub:

``` r
# install devtools if not already installed
install.packages("devtools")

# install sparsevector from GitHub
devtools::install_github("anthonyangg/sparsevector")
```
