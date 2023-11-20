model <- function(t, y, param) {

  S <- y[1]
  E <- y[2]
  I <- y[3]
  R <- y[4]
  N <- param["N"]

  beta <- param["beta"]
  mu <- param["mu"]
  gamma <- param["gamma"]
  lamda <- param["lamda"]

  dSt <- mu * (N - S) - beta * S * I/N
  dEt <- beta * S * I/N - mu * E-lamda*E
  dIt <-   - (mu + gamma) * I+lamda*E
  dRt <- gamma * I - mu * R

  outcome <- c(dSt, dEt,dIt, dRt)

  list(outcome)
}


#设置仿真参数
times <- seq(0, 156, by = 1/7)
param <- c(mu = 0.000, lamda = 0.03, beta = 4, gamma = 0.1,N = 1)
init <- c(S = 0.9999, E = 0.00008,I = 0.00002, R = 0)


result <-  deSolve::ode(y=init, times=times, func=model, parms = param)
result <- as.data.frame(result)

tail(round(result, 3.6),10)

#结果画图
#' @export
seirplot <- ggplot2::ggplot(data=result)+
  ggplot2::geom_line(ggplot2::aes(x=time, y=S,col="S"), lwd=2) +
  ggplot2::geom_line(ggplot2::aes(x=time, y=I,col="I"), lwd=2) +
  ggplot2::geom_line(ggplot2::aes(x=time, y=R,col="R"), lwd=2) +
  ggplot2::geom_line(ggplot2::aes(x=time, y=E,col="E"), lwd=2) +
  ggplot2::labs(x = "Time",y = "Ratio")+
  ggplot2::scale_color_manual(name = "SEIR", values = c("S" = "orange", "E" = "purple",
                                                        "I" = "red", "R" = "green"))

seirplot
ggplot2::ggsave(seirplot, file="seir.pdf", width=7, height=6)
ggplot2::ggsave(seirplot, file="seir.svg", width=7, height=6)
