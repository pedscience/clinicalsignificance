#' Placebo Amplification Data
#'
#' A dataset containing the data from Claus et al. (2020). In a routine
#' inpatient setting for unipolar depressive disorders they implemented an
#' intervention that sought to amplify the placebo response of antidepressants.
#' In the study, two groups were compared: treatment as usual (TAU) and placebo
#' amplification (PA). Participants were examined four times during their
#' treatment.
#'
#' @format An object of class `tbl_df` with 172 rows and 9 columns.
#'
#'   \describe{ \item{id}{Participant ID} \item{age}{Age} \item{sex}{Sex}
#'   \item{treatment}{Treatment (TAU for treatment as usual and PA for placebo
#'   amplification)} \item{time}{Measurement} \item{bdi}{Beck Depression
#'   Inventory (2nd Edition) score} \item{shaps}{Snaith-Hamilton Pleasure Scale
#'   score} \item{who}{WHO-Five Well-Being Index score} \item{hamd}{Hamilton
#'   Rating Scale for Depression score} }
#'
#' @references
#' - Claus, B. B., Scherbaum, N., & Bonnet, U. (2020). Effectiveness
#'   of an Adjunctive Psychotherapeutic Intervention Developed for Enhancing the
#'   Placebo Effect of Antidepressants Used within an Inpatient-Treatment
#'   Program of Major Depression: A Pragmatic Parallel-Group, Randomized
#'   Controlled Trial. Psychotherapy and Psychosomatics, 89(4), 258-260.
#'   https://doi.org/10.1159/000505855
#'
#' @source \url{https://osf.io/rc754/}
"claus_2020"


#' Marital Therapy Data
#'
#' A dataset containing the data from Jacobson et al. (1989). The purpose of the
#' study was to examine two forms of behavioral marital therapy,
#'
#' @format An object of class `tbl_df` with 60 rows and 4 columns.
#'
#'   \describe{ \item{subject}{Subject ID} \item{time}{Measurement}
#'   \item{das}{Dyadic Adjustment Scale score} \item{gds}{Global Distress Scale
#'   score} }
#'
#' @references
#' - Jacobson, N. S., Schmaling, K. B., Holtzworth-Munroe, A., Katt,
#' J. L., Wood, L. F., & Follette, V. M. (1989). Research-structured vs
#' clinically flexible versions of social learning-based marital therapy.
#' Behaviour Research and Therapy, 27(2), 173-180.
#' https://doi.org/10.1016/0005-7967(89)90076-4
#'
#' - Jacobson, N. S., & Truax, P.
#' (1991). Clinical significance: A statistical approach to defining meaningful
#' change in psychotherapy research. Journal of Consulting and Clinical
#' Psychology, 59(1), 12-19. https://doi.org/10.1037//0022-006X.59.1.12
"jacobson_1989"


#'Anxiety Data (Complete)
#'
#'A fictional complete dataset to exemplify the use of HLM method for clinical
#'significance.
#'
#'In a fictional clinical trial, participants were split up to belong to either
#'a medical placebo ("Placebo") or psychotherapeutic intervention
#'("Intervention") group.
#'
#'They underwent outpatient treatment during which they were followed for 5
#'measurements at which a fictional anxiety score was measured. This anxiety
#'score may range from 0 - 60.
#'
#'The functional population (i.e., non-anxious
#'individuals) can be expected to have a mean score of M = 8 points with a
#'standard deviation of SD = 4.
#'
#'@format A data frame with 580 rows and 4 variables: \describe{
#'  \item{\code{subject}}{Participant} \item{\code{treatment}}{Treatment. Either
#'  Placebo or Intervention.} \item{\code{measurement}}{Number of measurement}
#'  \item{\code{anxiety}}{Anxiety score, lower is better.} }
"anxiety_complete"


#'Anxiety Data
#'
#'A fictional dataset with misisngs to exemplify the use of HLM method for
#'clinical significance.
#'
#'In a fictional clinical trial, participants were split up to belong to either
#'a medical placebo ("Placebo") or psychotherapeutic intervention
#'("Intervention") group.
#'
#'They underwent outpatient treatment during which they were followed for 5
#'measurements at which a fictional anxiety score was measured. This anxiety
#'score may range from 0 - 60.
#'
#'The functional population (i.e., non-anxious individuals) can be expected to
#'have a mean score of M = 8 points with a standard deviation of SD = 4.
#'
#'@format A data frame with 580 rows and 4 variables: \describe{
#'  \item{\code{subject}}{Participant} \item{\code{treatment}}{Treatment. Either
#'  Placebo or Intervention.} \item{\code{measurement}}{Number of measurement}
#'  \item{\code{anxiety}}{Anxiety score, lower is better.} }
"anxiety"
