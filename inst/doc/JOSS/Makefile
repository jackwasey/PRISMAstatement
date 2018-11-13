# can't believe it has come to this...

paper.utf8.md: paper.Rmd
	Rscript -e 'rmarkdown::render("paper.Rmd", clean = FALSE, run_pandoc = FALSE, output_format = "github_document")'

paper.md: paper.utf8.md
	cat paper.yaml paper.utf8.md > paper.md

paper.pdf: paper.md
	./paper.sh

.PHONY: pdftest
pdftest:
	pandoc paper.md --filter pandoc-citeproc --bibliography paper.bib --bibliography r.bib -o paper.pdf

#paper.tex: paper.md
#	pandoc -o paper.tex paper.md

.PHONY: clean
clean:
	rm -rf paper.utf8.md paper.knitr.md paper.md paper_files
