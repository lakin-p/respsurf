library(respsurf)

data(dwn.data)
demofit.dn <- rsFit(data = dwn.data,
                    upper = NULL,
                    lower = NULL)
## fit a response surface to the dwn.data data
summary(demofit.dn)
plot(demofit.dn)

?debug
data(ups.data)
demofit.up <- rsFit(data  = ups.data,
                    start = list(B = 0.12, Econ = 1.19, EC50.1 = 1.4,
                                 EC50.2 = 3.4, m1 = 2.0, m2 = 2.1,
                                 alfa = 2.0),
                    lower = c(B = 0, Econ = 0, EC50.1 = .0000001, EC50.2 = .0000001,
                                m1 = .1, m2 = .1, alfa = .1),
                    upper = c(B = 1, Econ = 5, EC50.1 = 20, EC50.2 = 20,
                               m1 = 6, m2 = 6, alfa = 50),
                    mpos = TRUE
)
summary(demofit.up)
plot(demofit.up)