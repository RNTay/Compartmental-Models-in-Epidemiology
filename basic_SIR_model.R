# SIR = Susceptible, Infectious, Recovered
# This model assumes that the population is:
# 1. homogeneous
# 2. well-mixed
# i.e. all susceptible individuals face the same risk of infection per unit time.


# LOAD THE PACKAGES:
library(deSolve)
library(reshape2)
library(ggplot2)


# MODEL INPUTS:
N <- 10^6  # N = population number

initial_state_values <- c(S = N - 1,
                          I = 1,
                          R = 0)

parameters <- c(beta = 0.7,  # beta = daily infection rate
                gamma = 0.1)  # gamma = recovery rate


# TIMESTEPS:
times <- seq(from = 0, to = 90, by = 0.1)


# SIR MODEL FUNCTION:
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S+I+R
    lambda <- beta * I/N  # lambda = force of infection
    dS <- -lambda * S
    dI <- lambda * S - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}


# MODEL OUTPUT (solving the differential equations):
output <- as.data.frame(ode(y = initial_state_values,
                            times = times,
                            func = sir_model,
                            parms = parameters))


# PLOT:
output_long <- melt(output, id="time")
ggplot(data = output_long,
       aes(x = time,
           y = value,
           colour = variable,
           group = variable)) + geom_line() + xlab("Time (days)") + ylab("Number of people")
