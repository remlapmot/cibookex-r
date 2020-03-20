.PHONY: clean all

all: html pdf epub

html: ./docs/index.html

./docs/index.html:
	Rscript dependency.R && \
	Rscript -e 'rmarkdown::render_site(output_format = "bookdown::gitbook_book", encoding = "UTF-8")'

pdf: ./docs/cibookex-r.pdf

./docs/cibookex-r.pdf:
	Rscript dependency.R && \
	Rscript -e 'rmarkdown::render_site(output_format = "bookdown::pdf_book", encoding = "UTF-8")'

epub: ./docs/cibookex-r.epub

./docs/cibookex-r.epub:
	Rscript dependency.R && \
	Rscript -e 'rmarkdown::render_site(output_format = "bookdown::epub_book", encoding = "UTF-8")'

clean:
	rm ./docs/*
