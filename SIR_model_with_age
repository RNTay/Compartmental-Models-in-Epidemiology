# PACKAGES
library(deSolve)
library(reshape2)
library(ggplot2)


# INPUT
N_pop <- 10^6  # population number
initial_state_values <- c(S1 = 0.20*N_pop, S2 = 0.65*N_pop, S3 = 0.15*N_pop,
                          I1 = 1,          I2 = 0,          I3 = 0,
                          R1 = 0,          R2 = 0,          R3 = 0)
# (S1, I1, R1), (S2, I2, R2) and (S3, I3, R3) are different age groups.
# Here, they are children, adults and elderly respectively.

# Set up an empty contact matrix with rows for each age group and columns for each age group
contact_matrix <- matrix(0,nrow=3,ncol=3)
# Fill in the contact matrix
contact_matrix[1,1] = 7     # daily number of contacts that children make with each other
contact_matrix[1,2] = 5     # daily number of contacts that children make with adults
contact_matrix[1,3] = 1     # daily number of contacts that children make with the elderly
contact_matrix[2,1] = 2     # daily number of contacts that adults make with children
contact_matrix[2,2] = 9     # daily number of contacts that adults make with each other
contact_matrix[2,3] = 1     # daily number of contacts that adults make with the elderly
contact_matrix[3,1] = 1     # daily number of contacts that elderly people make with children
contact_matrix[3,2] = 3     # daily number of contacts that elderly people make with adults
contact_matrix[3,3] = 2     # daily number of contacts that elderly people make with each other

# Parameters
parameters <- c(b = 0.05,     # probability of infection per contact
                contact_matrix = contact_matrix,   # age-specific average number of daily contacts (defined above)
                gamma = 1/5)  # rate of recovery

# Times
times <- seq(from = 0, to = 90, by = 0.1)


# MODEL FUNCTION
sir_age_model <- function(time, state, parameters) {  
  
  with(as.list(parameters), {
    
    n_agegroups <- 3                                 # number of age groups
    S <- state[1:n_agegroups]                        # assign to S the first 3 numbers in the initial_state_values vector
    I <- state[(n_agegroups+1):(2*n_agegroups)]      # assign to I numbers 4 to 6 in the initial_state_values vector
    R <- state[(2*n_agegroups+1):(3*n_agegroups)]    # assign to R numbers 7 to 9 in the initial_state_values vector
    
    N <- S+I+R
    
    # Force of infection acting on susceptible children
    lambda <- b * contact_matrix %*% as.matrix(I/N)
    # the lambda vector contains the forces of infection for children, adults and the elderly (length 3)
    
    # The differential equations
    # Rate of change in children:
    dS <- -lambda * S             
    dI <- lambda * S - gamma * I
    dR <- gamma * I
    
    # Output
    return(list(c(dS, dI, dR))) 
  })
}


# MODEL OUTPUT

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_age_model,
                            parms = parameters))

output_long <- melt(as.data.frame(output), id = "time")

ggplot(data = output_long,                                               
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                                                   
  ylab("Number of people") +                                
  labs(colour = "Compartment")
