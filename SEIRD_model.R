# SEIRD = Susceptible, Exposed, Infectious, Recovered, Deceased

# PACKAGES
library(deSolve)
library(reshape2)
library(ggplot2)


# MODEL INPUTS
N <- 10^6  # total population

initial_state_values <- c(S = N - 1,
                          E = 0,
                          I = 1,
                          R = 0,
                          D = 0)

parameters <- c(beta = 0.3885,  # infection rate
                sigma = 0.25,  # rate of becoming symptomatic
                gamma = 0.194,  # rate of recovery
                mu = 0.006)  # rate of mortality


# TIMESTEPS
times <- seq(from = 0,
             to = 365,
             by = 1)


# SEIRD MODEL FUNCTION
SEIRD_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S+E+I+R
    lambda <- beta*I/N
    dS <- -lambda * S
    dE <- lambda*S - sigma*E
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dD <- mu*I
    
    return(list(c(dS, dE, dI, dR, dD)))
  })
}


# MODEL OUTPUT
output <- as.data.frame(ode(y = initial_state_values,
                            times = times,
                            func = SEIRD_model,
                            parms = parameters))


# PLOT
output_long <- melt(output, id="time")
ggplot(data = output_long,
       aes(x=time,
           y=value/N,
           colour=variable,
           group=variable)) + geom_line() + xlab("Time (days)") + ylab("Proportion of population")
