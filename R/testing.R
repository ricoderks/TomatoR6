  library(TomatoR6)
  
  # create data import object
  di_obj <- DataImport$new(name = "Testing")
  
  di_obj$fun1()
  
  di_obj$fun2(x = 100,
              y = 300)
  
  di_obj$fun3()
  
  di_obj$monkey
  di_obj$fun4()
  di_obj$monkey
  
  # create untargeted lipidomics object
  unLip <- UntargetedLipidomics$new(name = "testing again")
  unLip
  
  unLip$fun1()
  
  unLip$fun2(x = 123,
             y = 1234)
  
  unLip$fun3()
  
  unLip$monkey
  unLip$fun4()
  unLip$monkey
  
