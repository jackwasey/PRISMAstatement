#' generate PRISMA statement flow chart
#'
#' generate PRISMA statement flow chart for use in retrospective medical
#' research
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
#' @param dpi Dots per inch, 72 is the default here, and in \code{DiagrammeR}
#'   itself it claims to be 96. Varying the DPI (which is done in the DOT file)
#'   unfortunately does not get detected by the downstream processing by the
#'   `htmlwidgets` package. To overcome this, the user can add `height` and
#'   `width` arguments which are passed through. It is easy to for scaled graphs
#'   to fall off the canvas, or be crushed into the top-left corner, and
#'   unfortunately this requires trial and error. Increasing DPI over 72 with
#'   this setting tends to truncate the graph. On the other hand, leaving the
#'   DPI at 72 and increasing both height and width appears to consitently give
#'   higher resolution images.
#' @param ... Further arguments are passed to \code{grViz}
#' @source
#' \url{http://www.prisma-statement.org/PRISMAStatement/FlowDiagram.aspx}
#' @examples
#' prisma(1, 2, 3, 4, 5, 6, 7, 8, 9)
#' @export
prisma <- function(found, found_other,
                   no_dupes,
                   screened, screen_exclusions,
                   full_text, full_text_exclusions,
                   qualitative,
                   quantitative, ..., dpi = 72) {

  stopifnot(length(found) == 1)
  stopifnot(length(found_other) == 1)
  stopifnot(length(no_dupes) == 1)
  stopifnot(length(screened) == 1)
  stopifnot(length(screen_exclusions) == 1)
  stopifnot(length(full_text) == 1)
  stopifnot(length(full_text_exclusions) == 1)
  stopifnot(length(qualitative) == 1)
  stopifnot(length(quantitative) == 1)

  stopifnot(found == floor(found))
  stopifnot(found_other == floor(found_other))
  stopifnot(no_dupes == floor(no_dupes))
  stopifnot(screened == floor(screened))
  stopifnot(screen_exclusions == floor(screen_exclusions))
  stopifnot(full_text == floor(full_text))
  stopifnot(full_text_exclusions == floor(full_text_exclusions))
  stopifnot(qualitative == floor(qualitative))
  stopifnot(quantitative == floor(quantitative))

  stopifnot(found >= 0)
  stopifnot(found_other >= 0)
  stopifnot(no_dupes >= 0)
  stopifnot(screened >= 0)
  stopifnot(screen_exclusions >= 0)
  stopifnot(full_text >= 0)
  stopifnot(full_text_exclusions >= 0)
  stopifnot(qualitative >= 0)
  stopifnot(quantitative >= 0)

  dot_template <- 'digraph prisma {

    node [shape="box"];
    graph [splines=ortho, nodesep=1, dpi = %d]

    a -> nodups;
    b -> nodups;
    a [label="Records identified through\ndatabase searching\n(n = %d)"];
    b [label="Additional records identified\nthrough other sources\n(n = %d)"]

    nodups -> incex;
    nodups [label="Records after duplicates removed\n(n = %d)"];

    incex -> {ex; ft}
    incex [label="Records screened\n(n = %d)"];

    ex [label="Records excluded\n(n = %d)"];
    {rank=same; incex ex}

    ft -> {qual; ftex};
    ft [label="Full-text articles assessed\nfor eligibility\n(n = %d)"];
    {rank=same; ft ftex}
    ftex [label="Full-text articles excluded,\nwith reasons\n(n = %d)"];

    qual -> quant
    qual [label="Studies included in qualitative synthesis\n(n = %d)"];

    quant [label="Studies included in\nquantitative synthesis\n(meta-analysis)\n(n = %d)"];
  }'

  DiagrammeR::grViz(
    sprintf(dot_template,
            dpi,
            found, found_other,
            no_dupes,
            screened, screen_exclusions,
            full_text, full_text_exclusions,
            qualitative, quantitative),
    ...)
}
