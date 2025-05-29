#' @import mvtnorm

NULL
# -------------functions -------------------------------------------------------
#'Dependent uniform generator
#'
#' Generates a sequence of dependent uniformly distributed variables between 0 and 1 
#' with an exponentially decaying correlation structure.
#'
#' @param n Integer. Number of observations to generate.
#' @param rho Numeric. Base correlation coefficient between adjacent observations (0 ≤ \eqn{\rho} ≤ 1). 
#'            Correlation decays exponentially. Usually not larger than 0.33.
#'
#' @return Numeric vector of length n containing dependent uniform variables.
#' 
#' @section Correlation Structure:
#' The correlation matrix \eqn{S_t} is:
#' \deqn{S_1=\begin{bmatrix}
#' 1 & \rho &\rho^2 & \rho^3&...  \\
#' \rho & 1 & \rho & \rho^2&...\\
#' \rho^2 & \rho & 1 & \rho&...\\
#' \rho^3 & \rho^2 & \rho & 1&...\\
#' ... & ... & ... & ...&...\\
#' \end{bmatrix}, St=[ 2 \times \sin(s \times \pi/6)]_{\forall s\in S_1}.}
#' When the n and \eqn{\rho} is small, the sine transformation will make the matrix positive definite.
#' @seealso \code{\link{first_order_uniform}} for generating dependent uniform variables with given correlation on first order.
#' \code{\link{second_order_uniform}}for generating dependent uniform variables with given correlation on second order.
#' @examples
#' # Generate 100 correlated uniform variables
#' set.seed(123)
#' x=dependent_uniform(100, rho = 0.3)
#' x
#' acf(x)
#' @export
dependent_uniform<-function(n, rho=0.31) {
  if(n==1)return(runif(1))
  S <-diag(n)
  for (i in 1:n) {
    for (j in 2:n) {
      S[i, j] <- rho^(abs(i-j))
    }
  }
  diag(S)<-1 
  St = 2 * sin(S * pi/6) # must be positive definite
  U = c(pnorm(rmvnorm(1, sigma = St)))
  U
}

#' Dependent uniform generator with correlation on only first order
#'
#' Generates a sequence of dependent uniformly distributed variables between 0 and 1 
#' with only first order correlation.
#'
#' @param n Integer. Number of observations to generate.
#' @param rho Numeric. Correlation coefficient between adjacent observations (0 ≤  \eqn{\rho} ≤ 1). 
#' @return A numeric vector of length \eqn{n} containing dependent uniform variables.
#' @seealso \code{\link{dependent_uniform}} for generating dependent uniform variables with a general correlation structure.
#' 
#' \code{\link{second_order_uniform}}for generating dependent uniform variables with given correlation on second order.
#' @section Correlation Structure: We use the nearPD function in the Matrix package to find the nearest correlation matrix.
#'  The matrix is transformed from:
#' \deqn{S_2=\begin{bmatrix}
#'    1 & \rho &0 & 0&...  \\
#'    \rho & 1 & \rho & 0&...\\
#'    0 & \rho & 1 & \rho&...\\
#'    0 & 0 & \rho & 1&...\\
#'    ... & ... & ... & ...&...\\
#'    \end{bmatrix},
#' }
#' @examples
#' # Generate 100 correlated uniform variables
#' set.seed(123)
#' x=first_order_uniform(100, rho = 0.3)
#' x
#' acf(x)
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @references 
#'   Nicholas J Higham. 
#'   {Computing the nearest correlation matrix—a problem from finance}. 
#'   \emph{IMA Journal of Numerical Analysis}, 22(3):329–343, 2002. 
#'   \url{https://doi.org/10.1093/imanum/22.3.329}
#' @export
#'
first_order_uniform<-function(n, rho = 0.31) {
  S <- diag(n)
  if (n > 1) {
    for (i in 1:n) {
      if (i - 1 > 0) S[i, (i - 1)] <- rho
      if (i + 1 <= n) S[i, (i + 1)] <- rho
    }
    diag(S) <- 1
    eigen_values <- eigen(S, symmetric = TRUE)$values
    if (!all(eigen_values >= 0)) {
      S <- as.matrix(Matrix::nearPD(S, cor = TRUE)$mat)
    }
    U <- c(pnorm(rmvnorm(1, sigma = S)))
  } else {
    U <- runif(1)
  }
  U
}
#' Dependent uniform generator with correlation on only second order
#'
#' Generates a sequence of dependent uniformly distributed variables between 0 and 1 
#' with only second order correlation.
#'
#' @param n Integer. Number of observations to generate.
#' @param rho Numeric. Correlation coefficient between adjacent observations (0 ≤  \eqn{\rho} ≤ 1). 
#' @return A numeric vector of length \eqn{n} containing dependent uniform variables.
#' @seealso \code{\link{dependent_uniform}} for generating dependent uniform variables with a general correlation structure.
#' 
#' \code{\link{first_order_uniform}} for generating dependent uniform variables with given correlation on first order.
#' @section Correlation Structure: We use the nearPD function in the Matrix package to find the nearest correlation matrix.
#'  The matrix is transformed from:
#' \deqn{S_3=\begin{bmatrix}
#' 1 & 0 &\rho & 0&...  \\
#' 0 & 1 & 0 & \rho&...\\
#' \rho & 0 & 1 & 0&...\\
#' 0 & \rho & 0 & 1&...\\
#'... & ... & ... & ...&...\\\end{bmatrix}
#' }
#' @examples
#' # Generate 100 correlated uniform variables
#' set.seed(123)
#' x=second_order_uniform(100, rho = 0.3)
#' x
#' acf(x)
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @references 
#'   Nicholas J Higham. 
#'   {Computing the nearest correlation matrix—a problem from finance}. 
#'   \emph{IMA Journal of Numerical Analysis}, 22(3):329–343, 2002. 
#'   \url{https://doi.org/10.1093/imanum/22.3.329}
#' @export
#'
second_order_uniform<-function(n, rho = 0.31) {
  S <- diag(n)
  if (n > 2) {
    for (i in 1:n) {
      if (i - 2 > 0) S[i, (i - 2)] <- rho
      if (i + 2 <= n) S[i, (i + 2)] <- rho
    }
    S[1, 3] <- rho
    S[3, 1] <- rho
    diag(S) <- 1
    eigen_values <- eigen(S, symmetric = TRUE)$values
    if (!all(eigen_values >= 0)) {
      S <- as.matrix(Matrix::nearPD(S, cor = TRUE)$mat)
    }
    U <- c(pnorm(rmvnorm(1, sigma = S)))
  } else {
    U <- runif(n)
  }
  U
}