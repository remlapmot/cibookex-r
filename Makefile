.PHONY: clean

all: rdeps statadeps
	Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"

html: rdeps statadeps ./docs/index.html
	Rscript -e "rmarkdown::render_site(output_format = 'bookdown::gitbook_book', encoding = 'UTF-8')"

pdf: rdeps statadeps ./docs/cibookex-r.pdf
	Rscript -e "rmarkdown::render_site(output_format = 'bookdown::pdf_book', encoding = 'UTF-8')"

epub: rdeps statadeps ./docs/cibookex-r.epub
	Rscipt -e "rmarkdown::render_site(output_format = 'bookdown::epub_book', encoding = 'UTF-8')"

rdeps:
	Rscript -e "install.packages('devtools'); devtools::install_dev_deps()"
	Rscript -e "if !tinytex::is_tinytex() tinytex::install_tinytex()"

statadeps:
	ifeq ($(OS),Windows_NT)     # is Windows_NT on XP, 2000, 7, Vista, 10...
		StataSE-64 /e do dependency.do
	else
		stata-se -b do dependency.do
	endif

clean:
	rm ./docs/*