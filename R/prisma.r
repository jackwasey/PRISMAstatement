#' Generate a PRISMA statement flow chart
#'
#' Generate PRISMA statement flow chart for use in literature
#' research and reviews. Almost all arguments are mandatory, as they are in the recommended
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
#' \dontrun{
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
  DiagrammeR::grViz(
    prisma_graph(found = found,
                 found_other = found_other,
                 no_dupes = no_dupes,
                 screened = screened,
                 screen_exclusions = screen_exclusions,
                 full_text = full_text,
                 full_text_exclusions = full_text_exclusions,
                 qualitative = qualitative,
                 quantitative = quantitative,
                 labels = labels,
                 extra_dupes_box = extra_dupes_box,
                 dpi = dpi,
                 font_size = font_size,
                 ...)
  )
}

#' @describeIn prisma Generate the `dot` graph text
#' @export
prisma_graph <- function(found,
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
  # apostrophes need to be replaced for grViz
  labels <- lapply(labels, gsub, pattern = "'", replace = "&rsquo;")
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
            labels$quantitative)
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

#' @describeIn prisma_pdf Export using any conversion function offered by `rsvg`
#' @param rsvg_fun Function from `rsvg` default being `rsvg::rsvg_png`
#' @param ... Passed to `rsvg_fun`
#' @examples
#' \dontrun{
#' g_dot <- prisma_graph(9, 8, 7, 6, 1, 5, 1, 4, 1)
#' prisma_export(g_dot, "test.png", rsvg_fun = rsvg::rsvg_png)
#' }
#' @export
prisma_export <- function(x,
                          filename = "prisma.png",
                          rsvg_fun = rsvg::rsvg_png,
                          ...) {
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE) ||
      !requireNamespace("rsvg", quietly = TRUE)) {
    stop("DiagrammeRsvg and rsvg are both required for exporting PRISMA charts")
  }
  DiagrammeR::export_graph(x, file_name = filename, )
  utils::capture.output({
    rsvg_fun(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
             file = filename)
  })
  invisible()
}
