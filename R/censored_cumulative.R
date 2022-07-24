#' Censored cumulative logistic family.
#'
#' Use as:
#' brm(min | vint(max)  ~ predictors , family = censored_cumulative_logit(), stanvars = censored_cumulative_logit_stanvars(), ...)
#' Non-censored observations mean min == max. See `vignette("censored_cumulative_usage")` for more details.
#' @export
#' @importFrom brms custom_family
censored_cumulative_logit <- function() {
  custom_family(
    "censored_ordered_logistic", dpars = c("mu"),
    links = c("identity"), lb = 1,
    vars = "vint1[n]",
    type = "int", threshold = "flexible", specials = c("ordinal", "ordered_thres", "thres_minus_eta"))
}

#' @export
#' @rdname censored_cumulative_logit
#' @importFrom brms stanvar
censored_cumulative_logit_stanvars <- function() {
  stanvar(block = "functions", scode = '
    real censored_ordered_logistic_lpmf(int min, real mu, vector Intercept, int max) {
      if(min > max) {
        reject("min > max");
      }
      if(max > size(Intercept) + 1) {
        reject("max > number of categories");
      }
      if(min == max) {
        return(ordered_logistic_lpmf(min | mu, Intercept));
      } else if(min == 1 && max == size(Intercept) + 1) {
        // The range covers all possibilities
        return 0;
      } else {
        vector[max - min + 1] vals;
        for(i in min:max) {
          vals[i - min + 1] = ordered_logistic_lpmf(i | mu, Intercept);
        }
        return(log_sum_exp(vals));
      }
    }
  ' )
}
