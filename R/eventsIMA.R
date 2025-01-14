#' set of IMA events.
#'
#' Returns the set of IMA events.
#'
#' The set of available events is controlled by available sport plugins. The modules allocated to a user
#' account control the plugins that are available. Modules are allocated by Catapult support staff.
#' See \emph{OpenField Cloud API} vignette for more details.
#' @export
#' @return a vector of strings representing supported IMA events
#' @examples
#' all_events <- ima_events()
ima_events <- function() {
    return(c("ima_acceleration", "ima_jump", "ima_jump_ml", "ima_impact", "goalkeeping_v1", "goalkeeping_v2", "cricket_delivery_au", 
        "cricket_delivery", "running_symmetry", "ice_hockey_stride", "ice_hockey_bout", 
        "baseball_pitch_v1", "baseball_swing_v1", "baseball_pitch", "baseball_swing", "baseball_throw",
        "free_running", "football_movement_analysis",   # former "movement_cluster"
        "rugby_union_scrum", "rugby_union_contact_involvement", "rugby_union_kick", "rugby_union_lineout", "rugby_league_tackle", 
        "us_football_lineman_contact", "us_football_throw", "us_football_impact", "ice_hockey_goaltender_movement",
		"basketball", "tennis"))
}
