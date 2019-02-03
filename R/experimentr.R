#' Perform experiment
#'
#' @param entrants integer - the total number of experiment entrnats (assuming 50/50 to each arm)
#' @param a_conv numeric - conversion rate of group a
#' @param b_conv numeric - conversion rate of group b
#' @param entrants_to_peek_after integer - number of entrants to peek after repeatedly
#' @param prior_a length two numeric - (alpha, beta) prior beliefs of each conversion rate
#' @param prior_b numberic - (alpha, beta) prior beliefs of each conversion rate
#'
#' @return belief of chances group a better at each peek
#' @export
#'
#' @examples
perform_experiment <- function(entrants, a_conv, b_conv,
                               entrants_to_peek_after = 1,
                               prior_a = c(1, 1),
                               prior_b = c(1, 1)) {
  # Step 1: perform experiment for each
  tibble::tibble(
    group_a = rbinom(entrants/2, 1, prob = a_conv),
    group_b = rbinom(entrants/2, 1, prob = b_conv)
  ) %>%
    # https://en.wikipedia.org/wiki/Conjugate_prior#Discrete_distributions
    # see the formula for posterior hyperparameters.
    mutate(group_a_conv_rate_beliefs = map2(cumsum(group_a), 1:n(),
                                            function(x, y) {
                                              rbeta(1e3, x + prior_a[[1]], y - x + prior_b[[2]])
                                            })) %>%
    mutate(group_b_conv_rate_beliefs = map2(cumsum(group_b), 1:n(),
                                            function(x, y) {
                                              rbeta(1e3, x + prior_b[[1]], y - x + prior_b[[2]])
                                            })) %>%
    mutate(belief_a_better = map2_dbl(group_a_conv_rate_beliefs, group_b_conv_rate_beliefs,
                                      function(x, y) {
                                        mean(x > y)
                                      })) %>%
    pull(belief_a_better) -> beliefs_a_better

  beliefs_a_better[
    seq_len(length(beliefs_a_better)) %%
      entrants_to_peek_after == 0]
}
