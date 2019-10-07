# Repository of R code for the Causal Inference Book Examples by Hernán and Robins

The R code by Joy Shi and Sean McGrath available [here](https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/Rcode_CIpart2.zip) of the [Causal Inference Book](https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/) by Hernán MA and Robins JM rendered using [bookdown](https://bookdown.org/).

## Install dependencies
If you have downloaded/forked this repository you can install the dependencies that are on CRAN with (assuming your working directory is at the top level of the repo):
```r
# install.packages("devtools") # uncomment if devtools not installed
devtools::install_deps()
```

## Building the book

- If you need to install LaTeX, install the R tinytex package (which is included in the dependencies) and run
``` r
tinytex::install_tinytex()
```

- Render all formats
```r
rmarkdown::render_site(encoding = 'UTF-8')
```

- Render the PDF book
``` r
rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')
```

- Render the HTML book
``` r
rmarkdown::render_site(output_format = 'bookdown::gitbook_book', encoding = 'UTF-8')
```

- Preview a specific chapter
``` r
bookdown::preview_chapter("chapter-filename.Rmd")
```