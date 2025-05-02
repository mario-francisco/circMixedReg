#' Spatial Updating Experiment Data (Legge et al., 2016)
#'
#' This dataset originates from the spatial updating experiments conducted by Legge et al. (2016).
#' Participants --including sighted, low vision, and blind individuals-- were guided along predefined indoor paths 
#' and asked to estimate the direction and distance to a previously seen target under various sensory conditions.
#' The experiment was designed to simulate real-world navigation challenges with varying levels of sensory availability.
#'
#' @format A data frame with 679 observations and 7 variables:
#' \describe{
#'   \item{target_direction}{True angular direction to the target (degrees).}
#'   \item{target_distance}{True linear distance to the target (feet).}
#'   \item{condition}{Sensory condition during the trial (factor with five levels: Control, Forward Facing, Preview, Auditory, Deprivation).}
#'   \item{response_direction}{Participant's estimated angular direction to the target (degrees).}
#'   \item{response_distance}{Participant's estimated distance to the target (feet).}
#'   \item{subject_id}{Identifier of the participant.}
#'   \item{room_id}{Identifier of the room or path.}
#' }
#'
#' @details
#' In each trial, participants initially viewed a target (a small object placed on the floor), were guided along a three-segment path,
#' and then asked to estimate the direction and distance back to the target. The \code{condition} variable captures the level of
#' sensory information available during locomotion and estimation:
#' \itemize{
#'   \item \strong{Control}: Full vision and audition available.
#'   \item \strong{Forward Facing}: Vision allowed only during walking, not during estimation.
#'   \item \strong{Preview}: Brief visual preview before walking; blindfolded during locomotion and estimation.
#'   \item \strong{Auditory}: Blindfolded, but auditory cues (e.g., footsteps, echoes) available.
#'   \item \strong{Deprivation}: Blindfolded and noise-canceling headphones used (minimal sensory input).
#' }
#'
#' @source Legge, G. E., Granquist, C., Baek, Y., & Gage, R. (2016).
#' \emph{Indoor spatial updating with impaired vision}.
#' Investigative Ophthalmology & Visual Science, 57(15), 6757–6765.
#' \doi{10.1167/iovs.16-20226}
#'
#' @examples
#' data(legge_data)
#' summary(legge_data)
#' head(legge_data)
#'
"legge_data"
