---
title: 'Plot Flow Charts According to the PRISMA Statement'
tags:
  - evidence-based medicine
  - systematic reviews
  - reproducible research
  - graphics
  - healthcare research
authors:
 - name: Jack O Wasey
   orcid: 0000-0003-3738-4637
   affiliation: "1"
affiliations:
 - name: Children's Hospital of Philadelphia
   index: 1
date: 16 August 2018
bibliography:
 - paper.bib
 - r.bib
---

# Summary

The Preferred Reporting Items for Systematic Reviews and Meta-Analyses (PRISMA) Statement [@moher_preferred_2009] defines an evidence-based, minimal set of items for reporting in systematic reviews and meta-analyses. PRISMA focuses on the reporting of studies evaluating randomized clinical trials, but can also be used as a basis for reporting systematic reviews of other types of research, particularly evaluations of interventions.

Meta-analysis is a critical tool in evidence-based medicine, forming the basis of international recommendations and guiding local medical practice. Unfortunately, a large amount of published medical research is likely to be false [@ioannidis_why_2005]. For this reason, an effort has been made to raise the standard of work accepted in major medical journals. For example, for randomized controlled trials, we have the CONSORT statement [@Schulz_CONSORT2010Statement_2010]. PRISMA is the analogue for meta-analysis and systematic reviews.

The importance of these efforts to raise standards is so great that the PRISMA Statement has been published in multiple medical journals, and following its guidelines has become a requirement for authors wishing to publish meta-analyses and systematic reviews. [@moher_preferred_2009; @Moher_PreferredReportingItems_2009; @Moher_Preferredreportingitems_2009; @Moher_PreferredReportingItems_2009a; @Moher_Preferredreportingitems_2010; @Moher_Preferredreportingitems_2009a; @Liberati_PRISMAstatementreporting_2009; @Liberati_PRISMAStatementReporting_2009; @Liberati_PRISMAStatementReporting_2009a]

Given these important goals, the `PRISMAstatement` package for the R statistical software [@R] enables construction of a correct PRISMA flow diagram. R packages such as `ggplot2`, `rmarkdown`, `rticles` give researchers the tools to produce manuscripts incorporating their analysis, figures and text, entirely within the R environment, for example, using R Markdown (ref). However, but plotting flow charts is not straightforward, and PRISMA has specific requirements to meet. Most researchers are likely to resort to producing their PRISMA flow diagram in another application then import the resulting figure. This is a barrier to reproducible research which is addressed by `PRISMAstatement`: it is now possible to keep working within R to produce an up-to-date PRISMA flow diagram of publication quality. The updates may be incremental as the review takes place, or in an update when new articles are included in the systematic review.

Plot [PRISMA](http://prisma-statement.org) flow charts describing the identification, screening, eligibility and inclusion or studies in systematic reviews. 

# References
