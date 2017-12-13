HTcurveFit<- function(t,T1){
  
  a_start =20 
  b_start = 0.1
  c_start = 20
  d_start = 0.01
  
  m <- nls(T1 ~ c+a*(1-exp(-b*t)) + d*t, 
           start = list(a=a_start,b=b_start,c=c_start,d=d_start))
  
  return(m)
}