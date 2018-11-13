#' Generic exclusions flow chart (beta)
#'
#' Generate the diagraph DOT description for a generic series of inclusions and
#' exclusions
#' @examples
#' flow_exclusions(c(1000, 300, 150, 75, 38))
#' dot <-
#'   flow_exclusions_dot(
#'     incl_counts = c(972, 132, 77, 14),
#'     total_label = "Total Screened",
#'     incl_labels = c("Consented", "Completed Study", "BMI <= 30"),
#'     excl_labels = c("Declined Consent", "Failed to Complete", "BMI > 30")
#'     )
#' DiagrammeR::grViz(dot)
#' flow_exclusions(c(1000, 300, 150, 75, 38),
#'                 percent_of_total = TRUE,
#'                 show_count = FALSE)
#' flow_exclusions(c(100000, 3000, 1666, 411, 38),
#'                 percent_of_prev = TRUE,
#'                 percent_args = list(decimal.mark = ","),
#'                 format_args = list(scientific = TRUE),
#'                 show_count = TRUE)
#' @param percent_args `list` of arguments to pass to `scales::percent` for
#'   formatting percentages. Default is an empty list.
#' @param format_args `list` of arguments to pass to `base::format` for
#'   formatting counts. Default is an empty list.
#' @param ... arguments passed to `flow_exclusions_dot`
#' @export
#' @md
#' @keywords internal
flow_exclusions <- function(
  incl_counts,
  total_label = "Total",
  incl_labels = LETTERS[seq_len(length(incl_counts) - 1)],
  excl_labels = paste("Not ", incl_labels),
  show_count = TRUE,
  percent_of_total = FALSE,
  percent_of_prev = FALSE,
  percent_args = list(),
  format_args = list(),
  font_size = NULL, dpi = NULL,
  ...)
DiagrammeR::grViz(
  flow_exclusions_dot(
    incl_counts = incl_counts,
    total_label = total_label,
    incl_labels = incl_labels,
    excl_labels = excl_labels,
    show_count = show_count,
    percent_of_total = percent_of_total,
    percent_of_prev = percent_of_prev,
    percent_args = percent_args,
    format_args = format_args,
    font_size = font_size, dpi = dpi, ...)
)

#' @describeIn flow_exclusions Just generate the DOT text
#' @export
#' @keywords internal
flow_exclusions_dot <- function(
  incl_counts,
  total_label = "Total",
  incl_labels = LETTERS[seq_len(length(incl_counts) - 1)],
  excl_labels = paste("Not ", incl_labels),
  show_count = TRUE,
  percent_of_total = FALSE,
  percent_of_prev = FALSE,
  percent_args = list(),
  format_args = list(),
  font_size = NULL, dpi = NULL,
  ...) {
  is.wholenumber <-
    function(x) abs(x - round(x)) < .Machine$double.eps
  if (any(!is.wholenumber(incl_counts))) {
    warning("Some counts are not whole numbers, rounding them")
    incl_counts <- round(incl_counts)
  }
  excl_counts <- incl_counts[-length(incl_counts)] - incl_counts[-1]
  total_count <- incl_counts[1]
  stopifnot(length(total_label) == 1)
  stopifnot(length(incl_counts) == length(incl_labels) + 1,
            length(incl_counts) == length(excl_labels) + 1)
  stopifnot((is.numeric(font_size) && length(font_size) == 1) ||
              is.null(font_size))
  stopifnot((is.numeric(dpi) && length(dpi) == 1) ||
              is.null(dpi))
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
    total_label <- paste0(total_label, "\n")
    incl_labels <- paste0(incl_labels, "\n")
    excl_labels <- paste0(excl_labels, "\n")
    if (show_count) {
      total_label <- paste0(total_label,
                            do.call(format, c(list(total_count), format_args)))
      incl_labels <- paste0(incl_labels,
                            do.call(format, c(list(incl_counts[-1]), format_args)))
      excl_labels <- paste0(excl_labels,
                            do.call(format, c(list(excl_counts), format_args)))
    }
    if (percent_of_total || percent_of_prev) {
      if (percent_of_total) {
        incl_frac <- incl_counts[-1] / total_count
        excl_frac <- excl_counts / total_count
      } else {
        incl_frac <- incl_counts[-1] / incl_counts[-length(incl_counts)]
        excl_frac <- excl_counts / incl_counts[-length(incl_counts)]
      }
      hndr_perc <- do.call(scales::percent, c(list(1), percent_args))
      incl_perc <- do.call(scales::percent, c(list(incl_frac), percent_args))
      excl_perc <- do.call(scales::percent, c(list(excl_frac), percent_args))
      gap <- ifelse(show_count, " ", "")
      total_label <- paste0(total_label, gap, "(", hndr_perc, ")")
      incl_labels <- paste0(incl_labels, gap, "(", incl_perc, ")")
      excl_labels <- paste0(excl_labels, gap, "(", excl_perc, ")")
    }
    # end any percentage
  }
  # end any numbers at all
  head_template <- '
    digraph prisma {
    node [shape="box" %s];
    graph [splines=ortho, nodesep=1 %s]
    '
  dot_head <- sprintf(
    head_template,
    ifelse(is.null(font_size), "", sprintf(", fontsize = %d", font_size)),
    ifelse(is.null(dpi), "", sprintf(", dpi = %d", dpi))
  )
  label_template <- "%s [label=\"%s\"];"
  arrow_template <- "%s -> {%s; %s};"
  rank_template <- "{rank=same; %s %s}"
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
