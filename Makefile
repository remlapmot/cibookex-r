.PHONY: clean

all: rdeps statadeps ./docs/index.html ./docs/cibookex-r.pdf ./docs/cibookex-r.epub
	Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"

html: rdeps statadeps ./docs/index.html
	Rscript -e "rmarkdown::render_site(output_format = 'bookdown::gitbook_book', encoding = 'UTF-8')"

pdf: rdeps statadeps ./docs/cibookex-r.pdf
	Rscript -e "rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')"

epub: rdeps statadeps ./docs/cibookex-r.epub
	Rscipt -e "rmarkdown::render_site(output_format = 'bookdown::epub_book', encoding = 'UTF-8')"

rdeps:
	Rscript -e "options(pkgType = 'binary'); devtools::install_dev_deps()"
	Rscript -e "if (!tinytex:::is_tinytex()) tinytex::install_tinytex()"

clean:
	rm ./docs/*