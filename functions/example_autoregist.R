M <- calculateM(rowcount, colcount, frames1)

mydata1regist = list()
for (i in 1:nframe){
  registerimage = autoregist(M, frames1[[i]], colcount, rowcount)
  mydata1regist[[i]] = registerimage
}
