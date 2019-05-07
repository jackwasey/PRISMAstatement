#!/bin/bash

if false
then
echo "Doing full pandoc for JOSS"
pandoc \
  --verbose \
  -V repository="repo" \
  -V archive_doi="doi" \
  -V paper_url="purl" \
  -V formatted_doi="fdoi" \
  -V review_issue_url="revisurl" \
  -V graphics="true" \
  -V issue="issue" \
  -V volume="volume" \
  -V page="page" \
  -V joss_logo_path="/usr/local/src/git/whedon/resources/joss-logo.png" \
  -V year="year" \
  -V submitted="sub" \
  -V published="pub" \
  -V citation_author="jackwasey" \
  -V paper_title="papertitle" \
  -V geometry:margin=1in \
  -o paper.pdf \
  --pdf-engine=xelatex \
  --filter pandoc-citeproc paper.md \
  --template "/usr/local/src/git/whedon/resources/latex.template"
else
  echo "Doing test pandoc for JOSS"
  pandoc \
    --verbose \
    -V geometry:margin=1in \
    -V joss_logo_path="/usr/local/src/git/whedon/resources/joss-logo.png" \
    -o paper.pdf \
    --pdf-engine=xelatex \
    --filter pandoc-citeproc paper.md \
    --template "/usr/local/src/git/whedon/resources/latex.template"
fi

