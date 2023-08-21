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
#'   \describe{
#'   \item{id}{Participant ID}
#'   \item{age}{Age}
#'   \item{sex}{Sex}
#'   \item{treatment}{Treatment (TAU for treatment as usual and PA for placebo
#'   amplification)}
#'   \item{time}{Measurement}
#'   \item{bdi}{Beck Depression Inventory (2nd Edition) score (lower is better)}
#'   \item{shaps}{Snaith-Hamilton Pleasure Scale score (higher is better)}
#'   \item{who}{WHO-Five Well-Being Index score (higher is better)}
#'   \item{hamd}{Hamilton Rating Scale for Depression score (lower is better)}
#'   }
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
#'   \describe{
#'   \item{subject}{Subject ID}
#'   \item{time}{Measurement}
#'   \item{das}{Dyadic Adjustment Scale score (higher is better)}
#'   \item{gds}{Global Distress Scale score (lower is better)}
#'   }
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
#'@format A data frame with 580 rows and 4 variables:
#'
#' \describe{
#'  \item{\code{subject}}{Participant}
#'  \item{\code{treatment}}{Treatment. Either Placebo or Intervention}
#'  \item{\code{measurement}}{Number of measurement}
#'  \item{\code{anxiety}}{Anxiety score (lower is better)}
#'  }
"anxiety_complete"


#'Anxiety Data
#'
#'A fictional dataset with missings to exemplify the use of HLM method for
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
#'@format A data frame with 580 rows and 4 variables:
#'
#'  \describe{
#'  \item{\code{subject}}{Participant}
#'  \item{\code{treatment}}{Treatment. Either Placebo or Intervention}
#'  \item{\code{measurement}}{Number of measurement}
#'  \item{\code{anxiety}}{Anxiety score (lower is better)}
#'  }
"anxiety"


#'Antidepressant Data
#'
#'A fictional dataset used to showcase group-based clinical significance
#'analyses and analyses with many participants.
#'
#'In a fictional clinical trial, the effectiveness of a new antidepressant
#'should be examined and depressed patients were randomized to one of four
#'groups:
#'- A wait list control group that did not receive a medication
#'- An inactive placebo group, i.e., a group that received a placebo
#'(inert substance without proposed clinical effect) pill
#'- An active placebo group, i.e., a group that received a placebo that evokes
#'side effects like mild nausea or a dry mouth
#'- The antidepressant group, so the target medication of this trial that should
#'have a clinical impact on the patients' depressive symptoms
#'
#'Suppose they underwent outpatient treatment, depressive symptoms were measured
#'before and after treatment with the Mind over Mood Depression Inventory
#'(MoM-DI) by Greenberger & Padesky (2015), and if a patient received a pill,
#'the clinician and the patient did not know, what type of medicaction they
#'consumed.
#'
#'Further, the minimal important difference for an improvement as measured by
#'this instrument was agreed to be an 8 point decrease. A deterioration can be
#'assumed if instrument scores increased by 5 points.
#'
#'The functional population (i.e., non-depressed individuals) can be expected to
#'have a mean score of M = 8 points with a standard deviation of SD = 7.
#'
#'@format A tibble with 1140 rows and 4 variables:
#'
#'  \describe{
#'  \item{\code{patient}}{Patient identifier}
#'  \item{\code{condition}}{Experimental condition}
#'  \item{\code{measurement}}{Indicator of measurement}
#'  \item{\code{mom_di}}{Mind over Mood Depression inventory scores
#'  (lower is better)
#'  }
#'}
#'
#'@references Greenberger, D., & Padesky, C. A. (2015). Mind over mood, second
#'  edition (2nd ed.). New York, NY: Guilford Publications.
"antidepressants"
