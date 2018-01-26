#make circle
circle <- function(o = c(0,0), r, n = 720, append=F,...)
{
  x <- 0:n
  theta <- x*2*pi/n
  if(append)
  {
    polygon(o[1] + r*cos(theta), o[2] + r*sin(theta),...)
  }
  else
  {
    plot(o[1] + r*cos(theta), o[2] + r*sin(theta), xlab="", ylab="",
    main="繁花规", axes=F, type="n", asp=1,...)
    polygon(o[1] + r*cos(theta), o[2] + r*sin(theta),...)
  }
  points(o[1], o[2], pch=16,...)
}

#rate belongs to (-1,1)
fanhuagui <- function(rate1, rate2, N, color)
{
  R=1;
  r=R*rate1;
  d=r*rate2;
  c0<-c(R-r+d, 0)
  for(theta in seq(0, N*2*pi, length = N*36))
  {
    o1 = c((R-r)*cos(theta), (R-r)*sin(theta))
    circle(r = R, col = 0, append = F)
    circle(o = o1, r = r, col=0, append = T)
    c0 <- rbind(c0, c(d*cos(-theta*R/r), d*sin(-theta*R/r)) + o1)
    lines(rbind(o1, c(d*cos(-theta*R/r), d*sin(-theta*R/r)) + o1), col=0)
    lines(c0, col = color)
  }
}

#have a try
fanhuagui(1.1,0.4,20,4)
fanhuagui(0.7,1.1,20,4)
fanhuagui(0.3,0.8,20,4)
fanhuagui(0.1,1.2,20,4)
