DecayQVals <- function(Q_matrix, Q_prior, action, state, phi, debug_info=NULL, condition=NULL) {
  ### Decay Q values ###
  # As in Niv et al. 15, the decay applies to everything but the chosen option 
  
  # Decay the non-chosen action for the current state
  Q_matrix[state, -action]  <-
    ((1-phi) * Q_matrix[state, -action]) + (phi * Q_prior[state, -action])
  
  # Decay the values of all states that aren't currently active  
  Q_matrix[setdiff(1:4, state), ]  <-
    ((1-phi) * Q_matrix[setdiff(1:4, state), ]) + (phi * Q_prior[setdiff(1:4, state), ])
  
Q_matrix  
}