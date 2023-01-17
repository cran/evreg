#' evreg: A package for evidential regression
#'
#' The evreg package implements ENNreg  (Denoeux 2022, 2023b), a neural network model for
#' regression in which prediction uncertainty is quantified by Gaussian random fuzzy numbers
#' (GRFNs), a newly introduced family of random fuzzy subsets of the real line that
#' generalizes both Gaussian random variables and Gaussian possibility distributions
#' (Denoeux, 2023a). The evreg package contains functions for training the ENNreg model,
#' tuning hyperparameters by cross-validation or the hold-out method, and making predictions.
#' It also contains utilities for making calculations with GRFNs (such as, e.g., computing
#' the degrees of belief and plausibility of an interval, or combining GRFNs). The package
#' consists in the following main functions:
#' \describe{
#' \item{ENNreg}{Training of the ENNreg model}
#' \item{ENNreg_init}{Initializing a ENNreg model}
#' \item{ENNreg_cv}{Hyperparameter tuning using cross-validation}
#' \item{ENNreg_holdout}{Hyperparameter tuning using cross-validation using hold-out}
#' \item{predict.ENNreg}{Prediction with a trained ENNreg model}
#' \item{intervals}{Computation of probabilistic and belief prediction intervals}
#' \item{Bel}{Degree of belief of a real interval}
#' \item{Pl}{Degree of plausibility of a real interval}
#' \item{Belint}{Computation of an interval with given degree of belief}
#' \item{pl_contour}{Plausibility contour function of a GRFN}
#' \item{combination_GRFN}{Combination of two GRFNs}
#'}
#'
#' @docType package
#' @name evreg
#'
#' @seealso \code{\link{ENNreg}}, \code{\link{ENNreg_init}},\code{\link{ENNreg_cv}},
#' \code{\link{ENNreg_holdout}}, \code{\link{predict.ENNreg}}, \code{\link{intervals}},
#' \code{\link{intervals}}, \code{\link{Bel}}, \code{\link{Pl}},\code{\link{Belint}},
#' \code{\link{pl_contour}}, \code{\link{combination_GRFN}}.
#'
#' @references
#'
#' Thierry Denoeux. An evidential neural network model for regression based on random fuzzy
#' numbers. In "Belief functions: Theory and applications (proc. of BELIEF 2022)", pages 57-66,
#' Springer, 2022.
#'
#' Thierry Denoeux. Quantifying prediction uncertainty in regression using random fuzzy sets: the ENNreg
#' model. TechRxiv preprint, 2023b.
#'
#' Thierry Denoeux. Reasoning with fuzzy and uncertain evidence using epistemic random
#' fuzzy sets: general framework and practical models. Fuzzy Sets and Systems, Vol. 453,
#' Pages 1-36, 2023a.
NULL
