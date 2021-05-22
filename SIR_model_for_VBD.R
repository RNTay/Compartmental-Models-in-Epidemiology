# VBD = Vector-Borne Disease

# MODEL ASSUMPTIONS
# Vectors are mosquitoes and hosts are humans in this code
# There are no births, background deaths or disease-induced mortality in the host population.
# Vectors enter the population and die at the same rate.
# Vector survival is independent of infection status.
# Vector and host populations are homogeneous and well-mixed.
# transmission occurs only from vector to host and from host to vector.
#   (no host-host or vector-vector transmission)
# Hosts are infectious as soon as they are infected.
# Recovery induces permanent immunity in hosts.
# Vectors are infectious as soon as they get infected and remain infectious until they die.


# PACKAGES
library(deSolve)
library(reshape2)
library(ggplot2)


# MODEL INPUTS
Nh <- 10000  # human population number
Nv <- 20000  # vector population number

ph <- 0.28/100  # initial prevalence of infection in host population
pv <- 0.057/100  # initial prevalence of infection in vector population

initial_state_values <- c(Sh = Nh - ph*Nh, Ih = ph*Nh, Rh = 0,
                          Sv = Nv - pv*Nv, Iv = pv*Nv)

parameters <- c(a = 1,  # mosquito biting rate (per day)
                bv = 0.4,  # probability of vector getting infected by a host
                bh = 0.4,  # probability of host getting infected by a vector
                mu_v = 0.25,  # death rate of vectors (per day)
                r = 0.167)  # recovery rate of hosts (per day)

# Times
t <- seq(from = 0, to = 90, by = 0.1)


# MODEL FUNCTION
SIR_model_for_VBD <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    Nh <- Sh+Ih+Rh
    Nv < Sv+Iv
    
    dSv_dt = mu_v*Nv - (a*bv/Nh)*Sv*Ih - mu_v*Sv
    dIv_dt <- (a*bv/Nh)*Sv*Ih - mu_v*Iv
    
    dSh_dt <- -(a*bh/Nh)*Sh*Iv
    dIh_dt <- (a*bh/Nh)*Sh*Iv - r*Ih
    dRh_dt <- r*Ih
    
    return(list(c(dSh_dt, dIh_dt, dRh_dt,
                  dSv_dt, dIv_dt)))
  })
}


# MODEL OUTPUT
output <- as.data.frame(ode(y = initial_state_values,
                            times = t,
                            func = SIR_model_for_VBD,
                            parms = parameters))

output_long <- melt(output, id="time")
ggplot(data = output_long,
       aes(x = time,
           y = value,
           colour = variable,
           group = variable)) + geom_line() + xlab("Time (days)") + ylab("Number")
