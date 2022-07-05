#' Create large-scale crosstab
#'
#' @param df A \code{data.frame}
#' @param row.vars A \code{character} or {character} vector
#' @param col.vars A \code{character} or {character} vector
#' @param wts A \code{character} or {character} vector
#' @param split.vars TRUE/FALSE: Should col.vars be treated as combined or crossed variables? Defaults to \code{FALSE}
#' @param percent TRUE/FALSE: Should output format be percent? Defaults to \code{FALSE}
#' @param decimal TRUE/FALSE: Should output format be decimal? Defaults to \code{FALSE}
#' @param digits A \code{integer} How many decimal places to round to? Defaults to \code{0}
#'
#' @return A \code{data.frame} with a "Question" column for the row variable names, a "Response" column for the row variable levels, columns displaying
#' percentages by total and grouped by column variables supplied by user.
#'
#' @import tidyverse pewmethods foreach
#' @importFrom purrr map
#' @export
#'
#' @examples
big_tabby <-
  function(df,
           row.vars,
           col.vars = NULL,
           wts = NULL,
           split.vars = FALSE,
           percent = FALSE,
           decimal = FALSE,
           digits = 0) {

    # INITIAL ERROR HANDLING

    if (split.vars &
        length(col.vars) <= 1)
      stop("To implement split.vars, the col.vars argument must be a character vector")

    # Detect presence of variables with excessive number of levels in row.vars and col.vars arguments
    if (!(is.null(row.vars) & is.null(col.vars))) {
      problem_vars <-
        suppressWarnings(get_input_lengths(df, row.vars, col.vars))
      problem_vars_rw <-
        suppressWarnings(get_input_lengths(df, input1 = row.vars))
      problem_vars_cl <-
        suppressWarnings(get_input_lengths(df, input1 = col.vars))

    }

    # If variables with too many levels exist, ask user whether they want the variables removed or the operation cancelled
    if (length(which(problem_vars)) > 0) {
      message(
        paste(
          "Variable input(s)",
          paste(str_to_upper(names(
            which(problem_vars)
          )), collapse = ", "),
          "exceeds the recommended number of response levels (<=50)."
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
                      wts = NULL,
                      places = digits) {
      out <-
        purrr::map(
          row.vars,
          ~ pewmethods::get_totals(
            .x,
            df,
            wt = wts,
            by = col.vars,
            by_total = T,
            percent = F,
            digits = places,
            include_unw = FALSE,
            complete = TRUE,
            na.rm = TRUE
          )
        )

    }

    if (is.null(col.vars) & length(row.vars)==1) {
      out <- tabby(df, row.vars, col.vars, wts) %>% as.data.frame()
    }

    else if (is.null(col.vars) & length(row.vars) > 1) {
      out <- tabby(df, row.vars, col.vars, wts)
      out <- suppressWarnings(stacktab(out, row.vars)) %>%
        relocate(c("Question", "Response"), .before = everything())

    }


    if (split.vars) {
      out <- tabby(df, row.vars, col.vars, wts)
      out <- suppressWarnings(stacktab(out, row.vars)) %>%
        relocate(c("Question", "Response", "Total"), .before = everything()) %>%
        select(-weight_name)
    }

    else if (length(col.vars) > 1) {
      out <- foreach::foreach(b = col.vars, .combine = 'cbind') %do% {
        tabby(df = df,
              row.vars,
              col.vars = b,
              wts = wts)
      }

      out <- suppressWarnings(stacktab(out, row.vars))

      fct_df <- df_to_factor(df)
      fct_info_rw <- get_fct_info(fct_df, row.vars)
      fct_info_cl <-get_fct_info(fct_df, col.vars)

      row_levels <- unname(unlist(fct_info_rw[[2]]))

      col_levels <- unname(unlist(fct_info_cl[[2]]))
      column_order<-c("Question", "Response", "Total", col_levels)
      output_levels <- out %>%
        select(-c(Question, Response, weight_name)) %>%
        names()
      Missing <- setdiff(col_levels, output_levels)

      if (length(Missing)>0) {
        out[Missing] <- 0
      }

      out <- aggregate(
        x = out[, !(names(out) %in% c("Question", "Response"))],
        by = list(
          Question = out$Question,
          Response = out$Response
        ),
        min,
        na.rm = TRUE
      ) %>%
        arrange(match(Question, fct_info_rw[[1]]), match(Response, row_levels)) %>%
        relocate(c("Question", "Response", "Total"), .before = everything()) %>%
        select(-weight_name)

      out<-out[,column_order]

      if (length(which(sapply(out, is.infinite)))>0) {
        out[sapply(out, is.infinite)] <- 0
      }


    }

    else {
      out <- tabby(df, row.vars, col.vars, wts)
      out <- suppressWarnings(stacktab(out, row.vars)) %>%
        relocate(c("Question", "Response"), .before = everything())
    }

    if (length(which(grepl("Total", names(out))))>0) {

      out <- out %>% relocate("Total", .after = Response)

    }

    if (length(which(grepl("weight_name", names(out))))>0) {

      out <- out %>% select(-weight_name)

    }

    if (percent) {
      out <- out %>%
        group_by(Question) %>%
        mutate(across(where(is.numeric), ~ round(.x * 100 / sum(.x),0)))

      is.nan.data.frame <- function(x)
        do.call(cbind, lapply(x, is.nan))

      out[is.nan(out)] <- 0

      out <- out %>%
        group_by(Question) %>%
        mutate(across(where(is.numeric), ~ paste0(.x,"%")))
    }

    if (decimal) {
      out <- out %>%
        group_by(Question) %>%
        mutate(across(where(is.numeric), ~ round(.x / sum(.x),2)))

      is.nan.data.frame <- function(x)
        do.call(cbind, lapply(x, is.nan))

      out[is.nan(out)] <- 0
    }

    return(out)
  }
