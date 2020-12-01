# Repository of R and Stata code for the exercises in Causal Inference: What If by Hernán and Robins

- The R code by Joy Shi and Sean McGrath is available [here](https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/Rcode_CIpart2.zip)
- The Stata code by Eleanor Murray and Roger Logan is available [here](https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/2019/11/stata_part2.zip) 
- The book by by Hernán MA and Robins JM is available [here](https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/) 
- These files are rendered using [bookdown](https://bookdown.org/).

## Install dependencies
- If you have downloaded/forked this repository you can install the R dependencies with (assuming your working directory is at the top level of the repo):
    ```r
    # install.packages("devtools") # uncomment if devtools not installed
    devtools::install_dev_deps()
    ```
- You can install the Stata dependencies, in Stata, with
    ``` stata
    do depdendency
    ```

## Building the book

- If you need to install LaTeX, in R install the tinytex package (which is included in the dependencies) and run
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

- Render the EPUB book
    ```r
    rmarkdown::render_site(output_format = 'bookdown::epub_book', encoding = 'UTF-8')
    ```

- Preview a specific chapter
    ``` r
    bookdown::preview_chapter("chapter-filename.Rmd")
    ```

- To build the chapters containing Stata code:
  - You need Stata installed on your machine.
  - Running `devtools::install_dev_deps()` should install the [Statamarkdown](https://github.com/Hemken/Statamarkdown) package by Doug Hemken.
  - The Statamarkdown package needs to be able to find your installed Stata.
  - In Stata, install the Stata dependencies, in Stata, with: `do dependency`.