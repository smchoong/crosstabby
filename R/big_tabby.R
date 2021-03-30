#' Create large-scale crosstab
#'
#' @param df A \code{data.frame}
#' @param row.vars A \code{character} or {character} vector
#' @param col.vars A \code{character} or {character} vector
#' @param wts A \code{character} or {character} vector
#' @param data.frame TRUE/FALSE: Should output be returned as a data.frame? Defaults to \code{FALSE}
#' @param format A \code{character} indicating "percent" or "decimal" format for the totals of the variables tabulated. Only available when data.frame=TRUE.
#' @param split.vars TRUE/FALSE: Should col.vars be treated as combined or crossed variables? Defaults to \code{FALSE}
#'
#' @return A \code{data.frame} with a "Question" column for the row variable names, a "Response" column for the row variable levels, columns displaying
#' percentages by total and grouped by column variables supplied by user.
#'
#' @import tidyverse pewmethods
#' @importFrom purrr map
#' @importFrom foreach foreach
#' @export
#'
#' @examples
big_tabby <-
  function(df,
           row.vars,
           col.vars = NULL,
           wts = NULL,
           data.frame = FALSE,
           format = FALSE,
           split.vars = FALSE) {

    # INITIAL ERROR HANDLING

    if (split.vars &
        length(col.vars) <= 1)
      stop("To implement split.vars, the col.vars argument must be a character vector")
    # if (!is.null(format) & !(data.frame))
    #   stop("The data.frame argument must be set to TRUE to use the format argument")
    # if (!is.null(format) & !(format %in% c("percent", "decimal")))
    #   stop("The format argument requires character input of either 'percent' or 'decimal'")


    if (!(is.null(row.vars) & is.null(col.vars))) {
      problem_vars <-
        suppressWarnings(get_input_lengths(df, row.vars, col.vars))
      problem_vars_rw <-
        suppressWarnings(get_input_lengths(df, input1 = row.vars))
      problem_vars_cl <-
        suppressWarnings(get_input_lengths(df, input1 = col.vars))

    }

    if (length(which(problem_vars)) > 0) {
      message(
        paste(
          "Variable input(s)",
          paste(str_to_upper(names(
            which(problem_vars)
          )), collapse = ", "),
          "exceeds the recommended number of response levels (<=10)."
        )
      )

      prompt1 <-
        readline(
          "Would you like to remove the problematic variables and proceed with the operation? (Y/N)"
        )
      if (regexpr(prompt1, 'y', ignore.case = TRUE) == 1) {
        pbv_r <- names(which(problem_vars_rw))
        row.vars.clean <-
          as.data.frame(row.vars) %>% filter(!row.vars %in% pbv_r)
        row.vars <- row.vars.clean[['row.vars']]

        pbv_c <- names(which(problem_vars_cl))
        col.vars.clean <-
          as.data.frame(col.vars) %>% filter(!col.vars %in% pbv_c)
        col.vars <- col.vars.clean[['col.vars']]

        continue = TRUE
      } else if (regexpr(prompt1, 'n', ignore.case = TRUE) == 1) {
        message("Operation cancelled by user.")
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
        continue = FALSE
      } else {
        message("Operation cancelled by user.")
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
        continue = FALSE
      }

    }


    # MAIN INTERNAL FUNCTION
    tabby <- function(df,
                      row.vars,
                      col.vars = NULL,
                      wts = NULL) {
      out <-
        purrr::map(
          row.vars,
          ~ pewmethods::get_totals(
            .x,
            df,
            wt = wts,
            by = col.vars,
            by_total = T,
            percent = T,
            digits = 0,
            include_unw = FALSE,
            complete = TRUE,
            na.rm = TRUE
          )
        )

    }

    if (is.null(col.vars) & length(row.vars) > 1 & data.frame) {
      out <- tabby(df, row.vars, col.vars, wts)
      out <- suppressWarnings(stacktab(out, row.vars)) %>%
        relocate(c("Question", "Response"), .before = everything())

    }

    if (split.vars) {
      out <- tabby(df, row.vars, col.vars, wts)
    }

    else if (length(col.vars) > 1) {
      out <- foreach::foreach(b = col.vars, .combine = 'cbind') %do% {
        tabby(df,
              row.vars,
              col.vars = b,
              wts)
      }
    }

    if (split.vars & data.frame) {
      out <- tabby(df, row.vars, col.vars, wts)
      out <- suppressWarnings(stacktab(out, row.vars)) %>%
        relocate(c("Question", "Response", "Total"), .before = everything()) %>%
        select(-weight_name)
    }

    else if (length(col.vars) > 1 & data.frame) {
      out <- foreach::foreach(b = col.vars, .combine = 'cbind') %do% {
        tabby(df = df,
              row.vars,
              col.vars = b,
              wts = "weights")
      }
      out <- suppressWarnings(stacktab(out, row.vars))

      fct_df <- df_to_factor(df)
      fct_info <- get_fct_info(fct_df, row.vars)

      out <- aggregate(
        x = out[, !(names(out) %in% c("Question", "Response"))],
        by = list(
          Question = out$Question,
          Response = out$Response
        ),
        min,
        na.rm = TRUE
      ) %>%
        arrange(match(Question, fct_info[[1]]), match(Response, fct_info[[2]])) %>%
        relocate(c("Question", "Response", "Total"), .before = everything()) %>%
        select(-weight_name)

    }

    if (data.frame & format == "percent") {
      out <- out %>% mutate(across(where(is.numeric), ~ paste0(.x, "%")))
    }

    if (data.frame & format == "decimal") {
      out <- out %>% mutate(across(where(is.numeric), ~ .x * 0.01))
    }

    return(out)
  }
