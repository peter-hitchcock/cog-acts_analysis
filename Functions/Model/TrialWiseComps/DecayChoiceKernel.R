DecayChoiceKernel <- function(CK_matrix, CK_inits, action, state, choice_kernel_phi, debug_info=NULL, condition=NULL) {
  ### Decay choice choice kernel for all A that's not the current a in s
  
  # Decay the non-chosen action for the current state
  CK_matrix[state, -action]  <-
    ((1-choice_kernel_phi) * CK_matrix[state, -action]) + (choice_kernel_phi * CK_inits[state, -action])
  
  # Decay the values of all states that aren't currently active  
  CK_matrix[setdiff(1:4, state), ]  <-
    ((1-choice_kernel_phi) * CK_matrix[setdiff(1:4, state), ]) + (choice_kernel_phi * CK_inits[setdiff(1:4, state), ])
  
CK_matrix  
}