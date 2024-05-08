#' run_nimble
#'
#' @param Y response vector
#' @param X_mat matrix of covariates
#' @param code_in list with nimble code
#' @param constants_in list with constants for nimble code
#' @param data_in list with data for nimble code
#' @param tau_in prior variance on betas
#' @param sigma_in fixed value of sigma
#' @param num_sims number of MCMC sims
#' @param num_burn number of iterations for burn in
#'
#' @return list with mcmc simulations generated in nimble
#' @export
#'
#' @examples
#' \dontrun{
#' run_nimble()
#' }
run_nimble <- function(Y, X_mat, code_in, constants_in, data_in, tau_in, sigma_in = 1, num_sims = 6000, num_burn = 1000){
  p <- ncol(X_mat) - 1
  num_models <- length(code_in)
  N <- nrow(X_mat)

  num_cores <- parallel::detectCores() - 1
  cluster <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cl = cluster)

  sims_out <- foreach::foreach(
    i = 1:num_models,
    .packages = c('nimble')
  ) %dopar% {
    nimble_model <- nimble::nimbleModel(code_in[[i]],
                                        constants = constants_in[[i]],
                                        data = data_in[[i]],
                                        inits = list(beta0 = 0, beta = rep(0,p)))
    sims <- nimble::nimbleMCMC(model = nimble_model, niter = num_sims, nchains = 2,
                               nburnin = num_burn, progressBar = F)
    rbind(sims$chain1, sims$chain2)
  }
  parallel::stopCluster(cl = cluster)

  return(sims_out)
}
