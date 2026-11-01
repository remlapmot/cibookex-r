depstata:
    stata-mp -b "do dependency"
depr:
    Rscript -e "source('dependency.R')"
deps: depr depstata
render:
    Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"
