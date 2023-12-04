> library(readxl)
> 中国_产成品存货_PPI <- read_excel("C:/Users/tianzengxu/Desktop/中国_产成品存货&PPI.xlsx",  sheet = "nstock")
> View(中国_产成品存货_PPI)
> syndate <- seq.Date(from = as.Date("2013/09/01",format = "%Y/%m/%d"), by = "month", to = as.Date("2023/10/01",format = "%Y/%m/%d"))-1
> dat <- as.data.frame(中国_产成品存货_PPI)
> res <- data.frame(list(date=syndate,value = NA))
> for(i in seq(1,nrow(dat))){
  y = year(dat[i,'指标名称'])
  m = month(dat[i,'指标名称'])
  d = day(dat[i,'指标名称'])
  ind = as.Date(paste(y,m,d,sep='-'))
  res[res$date==ind,2:ncol(dat)] = dat[i,2:ncol(dat)]
}
> resf = res%>%mutate(across(where(is.numeric), ~if_else(is.na(.), (dplyr::lead(.) + dplyr::lag(.)) / 2, .)))
> 中国_PPI <- read_excel("C:/Users/tianzengxu/Desktop/中国_产成品存货&PPI.xlsx", sheet = "PPI")
> dat_PPI=as.data.frame(中国_PPI)
> resf=cbind(syndate,resf)
> resff=resf[,2:ncol(res)]/dat_PPI[,2:ncol(dat_PPI)]
> resf=resf[,1:(ncol(resf)-3)]
> resff=resff[,1:(ncol(resff)-3)]
> foo_tc=function(x){return(trendcycle(seas(x=ts(x,start=c(2013,8),frequency=12),x11="")))}
> foo_season=function(x){return(seasonal(seas(x=ts(x,start=c(2013,8),frequency=12),x11="")))}
> nstock=resf[,2:ncol(resf)]%>%mutate_all(~foo_tc(.))
> rstock=resff[,1:ncol(resff)]%>%mutate_all(~foo_tc(.))
> nstock.cycle=nstock-hp1(ts(nstock),lambda=129600)
> rstock.cycle=rstock-hp1(ts(rstock),lambda=129600)
> write.csv(as.data.frame(nstock_final),'C:/Users/tianzengxu/Desktop/ncycle.csv')
> write.csv(as.data.frame(rstock_final),'C:/Users/tianzengxu/Desktop/rcycle.csv')