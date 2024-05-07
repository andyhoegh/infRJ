#' Combine Nimble output
#'
#' @param Y data
#' @param sims_in list of mcmc sims generated from run_nimble()
#' @param like_call_in list of likelihood calls associated with each model
#' @param priors_in vector of prior probabilities
#' @param sigma_in known sigma value
#'
#' @return list of posterior probabilities
#' @export
#'
#' @examples
#' \dontrun{
#' combine_nimble()
#' }
combine_nimble <- function(Y, sims_in, like_call_in, priors_in, sigma_in = 1){
  num_models <- length(sims_in)
  total_sims <- nrow(sims_in[[1]])
  N <- length(Y)
  Like_Stack <- array(0,dim = c(num_models, num_models, total_sims, N))
  # first two dims (i,j) are simulations generated under model i
  # to transition from the specified model j
  # the third dimension is each iteration from the RJMCMC
  # the fourth dimension is the length of the data

  for (i in 1:num_models){
    for (j in 1:N){
      for (k in 1:num_models){
        Like_Stack[i,k,,j] <- eval(parse(text = like_call_in[[k]]))
      }
     }
  }

  likes <- apply(Like_Stack, c(1,2,3), prod)

  transition_matrix <- matrix(0,num_models, num_models)

  for (i in 1:num_models){
    denom <- colSums(likes[i,,] * matrix(priors_in, nrow = num_models, ncol = total_sims))
    denom_matrix <- matrix(denom, num_models, total_sims, byrow = T)
    transition_matrix[i,] <- rowMeans(likes[i,,] * matrix(priors_in, nrow = num_models, ncol = total_sims) / denom_matrix)
  }

  sim_post_probs <- (abs(eigen(t(transition_matrix))$vectors[,1] )) /
    sum(sqrt((abs(eigen(t(transition_matrix))$vectors[,1]))^2 ))

  for (i in 1:num_models){
    denom <- colSums(likes[i,,])
    denom_matrix <- matrix(denom, num_models, total_sims, byrow = T)
    transition_matrix[i,] <- rowMeans(likes[i,,] / denom_matrix)
  }

  sim_post_probs_unif <- (abs(eigen(t(transition_matrix))$vectors[,1] )) /
    sum(sqrt((abs(eigen(t(transition_matrix))$vectors[,1]))^2 ))
  return(list(post_probs = sim_post_probs, post_probs_unif = sim_post_probs_unif))
}
