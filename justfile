depstata:
    stata-mp -b "do dependency"
depr:
    R -e "source('dependency.R')"
render:
    R -e "rmarkdown::render_site(encoding = 'UTF-8')"
