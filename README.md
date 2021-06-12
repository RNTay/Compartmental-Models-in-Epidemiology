# Compartmental-Models-in-Epidemiology

Code to simulate (simple) epidemics with compartmental models.

## SIR-model.R
SIR = Susceptible, Infectious, Recovered  
This model assumes that the population is:  
1. homogeneous  
2. well-mixed  

i.e. all susceptible individuals face the same risk of infection per unit time.


## SEIRD-model.R
SEIRD = Susceptible, Exposed, Infectious, Recovered, Deceased

## SIR-model-with-age.R
(S1, I1, R1), (S2, I2, R2) and (S3, I3, R3) are different age groups.  
Here, they are children, adults and elderly respectively.

## SIR-model-for-VBD.R
VBD = Vector-Borne Disease  
Vectors are mosquitoes and hosts are humans in this code.  
There are no births, background deaths or disease-induced mortality in the host population.  
Vectors enter the population and die at the same rate.  
Vector survival is independent of infection status.  
Vector and host populations are homogeneous and well-mixed.  
Transmission occurs only from vector to host and from host to vector (no host-host or vector-vector transmission).  
Hosts are infectious as soon as they are infected.  
Recovery induces permanent immunity in hosts.  
Vectors are infectious as soon as they get infected and remain infectious until they die.  
