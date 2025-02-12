dev <- FALSE
if(dev) {
  # library(TomatoR6)
  
  # create object
  di_obj <- DataImport$new(name = "Testing")
  
  di_obj$fun1()
  
  di_obj$fun2(x = 100,
              y = 300)
  
  di_obj$fun3()
  
  unLip <- UntargetedLipidomics$new(name = "testing again")
  unLip
  
  unLip$fun1()
  
  unLip$fun2(x = 123,
             y = 1234)
  
  unLip$fun3()
  

}