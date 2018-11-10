#' Generate a PRISMA statement flow chart
#'
#' Generate PRISMA statement flow chart for use in retrospective medical
#' research. Almost all arguments are mandatory, as they are in the recommended
#' PRISMA statement.
#' @param found Records found through database searching
#' @param found_other Additional records identified through other sources
#' @param no_dupes Records after duplicates removed
#' @param screened Records screened
#' @param screen_exclusions Records excluded
#' @param full_text Full-text articles assessed for eligibility
#' @param full_text_exclusions Full-text articles excluded with reasons
#' @param qualitative Studies included in qualitative analysis
#' @param quantitative Studies included in quantitative synthesis
#'   (meta-analysis)
#' @param labels \code{NULL} is the default, but if a named list of character
#'   strings, the box matching each name will get the corresponding label. See
#'   examples.
#' @param extra_dupes_box Single logical value, default is \code{FALSE} which
#'   corresponds to the example 2009 PRISMA Statement Flow Chart. If
#'   \code{TRUE}, then an additional box will be presented indicating the number
#'   of duplicates removed, calculated from the other numbers.
#' @param ... Further arguments are passed to \code{grViz}
#' @param dpi Dots per inch, 72 is the default here, and in \code{DiagrammeR}
#'   itself it claims to be 96. Varying the DPI (which is done in the DOT file)
#'   unfortunately does not get detected by the downstream processing by the
#'   'htmlwidgets' package. To overcome this, the user can add `height` and
#'   `width` arguments which are passed through. It is easy to for scaled graphs
#'   to fall off the canvas, or be crushed into the top-left corner, and
#'   unfortunately this requires trial and error. Increasing DPI over 72 with
#'   this setting tends to truncate the graph. On the other hand, leaving the
#'   DPI at 72 and increasing both height and width appears to consistently give
#'   higher resolution images.
#' @param font_size integer font size in points, default is 10. `DiagrammeR` via
#'   `htmlwidgets` should scale the boxes to include the text no matter what
#'   size font is used. However, the heuristics are not perfect, so tweaking the
#'   font size here may help prepare for publication.
#' @source \url{http://prisma-statement.org/PRISMAStatement/FlowDiagram}
#' @examples
#' prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107)
#' prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107,
#'        labels = list(found = "FOUND"))
#' prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107, dpi = 24)
#' prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107, extra_dupes_box = TRUE)
#' # vary the font size
#' prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107, font_size = 6)
#' prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107, font_size = 60)
#' # giving impossible numbers should cause an error
#' \donttest{
#'   prisma(1, 2, 3, 4, 5, 6, 7, 8, 9)
#' # giving unlikely numbers should cause a warning
#'   prisma(1000, 20, 270, 270, 10, 260, 19, 240, 107)
#'   prisma(1000, 20, 270, 270, 269, 260, 20, 240, 107)
#' }
#' @md
#' @export
prisma <- function(found,
                   found_other,
                   no_dupes,
                   screened,
                   screen_exclusions,
                   full_text,
                   full_text_exclusions,
                   qualitative,
                   quantitative = NULL,
                   labels = NULL,
                   extra_dupes_box = FALSE,
                   ...,
                   dpi = 72,
                   font_size = 10) {
  stopifnot(length(found) == 1)
  stopifnot(length(found_other) == 1)
  stopifnot(length(no_dupes) == 1)
  stopifnot(length(screened) == 1)
  stopifnot(length(screen_exclusions) == 1)
  stopifnot(length(full_text) == 1)
  stopifnot(length(full_text_exclusions) == 1)
  stopifnot(length(qualitative) == 1)
  stopifnot(is.null(quantitative) || length(quantitative) == 1)
  # each number should be a non-negative integer (but may be 'numeric' type)
  stopifnot(found == floor(found))
  stopifnot(found_other == floor(found_other))
  stopifnot(no_dupes == floor(no_dupes))
  stopifnot(screened == floor(screened))
  stopifnot(screen_exclusions == floor(screen_exclusions))
  stopifnot(full_text == floor(full_text))
  stopifnot(full_text_exclusions == floor(full_text_exclusions))
  stopifnot(qualitative == floor(qualitative))
  stopifnot(is.null(quantitative) || quantitative == floor(quantitative))
  stopifnot(found >= 0)
  stopifnot(found_other >= 0)
  stopifnot(no_dupes >= 0)
  stopifnot(screened >= 0)
  stopifnot(screen_exclusions >= 0)
  stopifnot(full_text >= 0)
  stopifnot(full_text_exclusions >= 0)
  stopifnot(qualitative >= 0)
  stopifnot(is.null(quantitative) || quantitative >= 0)
  # can't have more articles at any stage
  stopifnot(no_dupes <= found + found_other)
  stopifnot(screened <= no_dupes)
  stopifnot(full_text <= screened)
  stopifnot(qualitative <= full_text)
  stopifnot(quantitative <= qualitative)
  # exclusions can't be greater than what they excluded from
  stopifnot(screen_exclusions <= screened)
  stopifnot(full_text_exclusions <= full_text)
  if (screened - screen_exclusions != full_text)
    warning("After screening exclusions, a different number of remaining ",
            "full-text articles is stated.")
  if (full_text - full_text_exclusions != qualitative)
    warning("After full-text exclusions, a different number of remaining ",
            "articles for qualitative synthesis is stated.")
  dupes <- found + found_other - no_dupes
  labels_orig <- list(
    found = pnl("Records identified through",
                "database searching",
                paren(found)),
    found_other = pnl("Additional records identified",
                      "through other sources",
                      paren(found_other)),
    no_dupes = pnl("Records after duplicates removed", paren(no_dupes)),
    dupes = pnl("Duplicates excluded", paren(dupes)),
    screened = pnl("Records screened", paren(screened)),
    screen_exclusions = pnl("Records excluded", paren(screen_exclusions)),
    full_text = pnl("Full-text articles assessed",
                    "for eligibility",
                    paren(full_text)),
    full_text_exclusions =
      pnl("Full-text articles excluded,",
          "with reasons",
          paren(full_text_exclusions)),
    qualitative = pnl("Studies included in qualitative synthesis",
                      paren(qualitative)),
    quantitative = pnl("Studies included in",
                       "quantitative synthesis",
                       "(meta-analysis)",
                       paren(quantitative))
  )
  for (l in names(labels))
    labels_orig[[l]] <- labels[[l]]
  labels <- labels_orig
  dupes_box <- sprintf(
    'nodups -> incex;
    nodups [label="%s"];',
    labels$no_dupes)
  if (extra_dupes_box)
    dupes_box <- sprintf(
      'nodups -> {incex; dups};
       nodups [label="%s"];
       dups [label="%s"]; {rank=same; nodups dups}',
      labels$no_dupes, labels$dupes)

  dot_template <- 'digraph prisma {
    node [shape="box", fontsize = %d];
    graph [splines=ortho, nodesep=1, dpi = %d]
    a -> nodups;
    b -> nodups;
    a [label="%s"];
    b [label="%s"]
    %s
    incex -> {ex; ft}
    incex [label="%s"];
    ex [label="%s"];
    {rank=same; incex ex}
    ft -> {qual; ftex};
    ft [label="%s"];
    {rank=same; ft ftex}
    ftex [label="%s"];
    qual -> quant
    qual [label="%s"];
    quant [label="%s"];
  }'

  DiagrammeR::grViz(
    sprintf(dot_template,
            font_size,
            dpi,
            labels$found,
            labels$found_other,
            dupes_box,
            labels$screened,
            labels$screen_exclusions,
            labels$full_text,
            labels$full_text_exclusions,
            labels$qualitative,
            labels$quantitative),
    ...)
}

paren <- function(n)
  sprintf("(n = %d)", n)

pnl <- function(...)
  paste(..., sep = "\n")

#' Make PDF of the plot
#'
#' This makes a PDF file which can be included by knitr Sweave.
#' @param x output of call to \code{prisma}
#' @param filename path of output file
#' @importFrom utils capture.output
#' @examples
#' \dontrun{
#' g <- prisma(9, 8, 7, 6, 5, 4, 3, 2, 1)
#' prisma_pdf(g, "test.pdf")
#' knitr::include_graphics("test.pdf")
#' }
#' @keywords internal
prisma_pdf <- function(x, filename = "prisma.pdf") {
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE) ||
      !requireNamespace("rsvg", quietly = TRUE)) {
    stop("DiagrammeRsvg and rsvg are both required for this prisma_pdf")
  }
  utils::capture.output({
    rsvg::rsvg_pdf(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                   file = filename)
  })
  invisible()
}

#' Generic exclusions flow chart (beta)
#'
#' Generate the diagraph DOT description for a generic series of inclusions and exclusions
#' @examples
#' PRISMAstatement:::exclusions_digraph(c(1000, 300, 150, 75, 38))
#' dot <-
#'   PRISMAstatement:::exclusions_digraph(
#'     incl_counts = c(972, 132, 77, 14),
#'     total_label = "Total Screened",
#'     incl_labels = c("Consented", "Completed Study", "BMI <= 30"),
#'     excl_labels = c("Declined Consent", "Failed to Complete", "BMI > 30")
#'     )
#' DiagrammeR::grViz(dot)
#' @param ... arguments passed to `scales::percent`
#' @importFrom scales percent
#' @export
#' @md
#' @keywords internal
exclusions_digraph <- function(
  incl_counts,
  total_label = "Total",
  incl_labels = LETTERS[seq_len(length(incl_counts) - 1)],
  excl_labels = paste("Not ", incl_labels),
  show_count = TRUE,
  percent_of_total = FALSE,
  percent_of_prev = FALSE,
  font_size = 10, dpi = 72,
  ...)
{
  incl_counts <- as.integer(incl_counts)
  excl_counts <- incl_counts[-length(incl_counts)] - incl_counts[-1]
  total_count <- incl_counts[1]
  stopifnot(length(total_label) == 1)
  stopifnot(length(incl_counts) == length(incl_labels) + 1,
            length(incl_counts) == length(excl_labels) + 1)
  stopifnot(is.numeric(font_size), is.numeric(dpi))
  stopifnot(!(percent_of_total && percent_of_prev))

  fixchar <- function(y) gsub(pattern = "[[:punct:][:space:]]",
                              replacement = "", x = y, perl = TRUE)
  total_nm <- fixchar(total_label)
  incl_nm <- fixchar(incl_labels)
  excl_nm <- fixchar(excl_labels)
  # to avoid duplicates being created accidentlly, add unique id to each
  incl_nm <- paste0(incl_nm, "_", LETTERS[seq_along(incl_nm)])
  excl_nm <- paste0(excl_nm, "_", letters[seq_along(excl_nm)])
  stopifnot(!anyDuplicated(c(total_nm, incl_nm, excl_nm)))
  if (any(c(show_count, percent_of_total, percent_of_prev))) {
    total_label %<>% paste0("\n")
    incl_labels %<>% paste0("\n")
    excl_labels %<>% paste0("\n")
    if (show_count) {
      total_label %<>% paste0(total_count)
      incl_labels %<>% paste0(incl_counts[-1])
      excl_labels %<>% paste0(excl_counts)
    }
    if (percent_of_total || percent_of_prev) {
      if (percent_of_total) {
        incl_frac <- incl_counts[-1] / total_count
        excl_frac <- excl_counts / total_count
      } else {
        incl_frac <- incl_counts[-1] / incl_counts[-length(incl_counts)]
        excl_frac <- excl_counts / incl_counts[-length(incl_counts)]
      }
      incl_perc <- scales::percent(incl_frac, ...)
      excl_perc <- scales::percent(excl_frac, ...)
      gap = ifelse(show_count, " ", "")
      total_label %<>% paste0(gap, "(100%)")
      incl_labels %<>% paste0(gap, "(", incl_perc, ")")
      excl_labels %<>% paste0(gap, "(", excl_perc, ")")
    }
    # end any percentage
  }
  # end any numbers at all
  head_template <- '
    digraph prisma {
    node [shape="box", fontsize = %d];
    graph [splines=ortho, nodesep=1, dpi = %d]
    '
  dot_head <- sprintf(head_template, font_size, dpi)

  label_template <- '%s [label="%s"];'
  arrow_template <- '%s -> {%s; %s};'
  rank_template <- '{rank=same; %s %s}'
  dot_labels <- paste(sprintf(label_template,
                              c(total_nm, incl_nm, excl_nm),
                              c(total_label, incl_labels, excl_labels)),
                      collapse = "\n")
  dot_arrows <- paste(sprintf(arrow_template,
                              c(total_nm, incl_nm[-length(incl_nm)]),
                              incl_nm,
                              excl_nm),
                      collapse = "\n")
  dot_rank <- paste(sprintf(rank_template,
                            c(total_nm, incl_nm[-length(incl_nm)]),
                            excl_nm),
                    collapse = "\n")
  paste(dot_head, dot_labels, dot_arrows, dot_rank, "}", sep = "\n")
}

