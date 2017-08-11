# Bayesian model:
- P(c|x,n) = p(x,n|c)P(c) / p(x,n)
- P(x|n,c) = P(x,n|c) / P(n|c) implies P(x,n|c) = P(x|n,c) * P(n|c)
- so P(c|x,n) = p(x,n|c)P(c) / p(x,n) = P(x|n,c) * P(n|c) * P(c) / p(x,n) ~ P(x|n,c) * P(n|c) * P(c) 

# distribution:
- P(c) ~ MN(n=1, p1,p2,...,p5)
- P(n|c) ~ Poi(l) (the count of engagement data - 1 for each label point, -1 is to make fit the support of poisson distribution) 
- P(x|n,c) = P(positions|n,c) * P(clicks|n,c) * P(views|n,c) where 
  - positions|n,c ~ MN(n, p1, p2,...,pa), 
  - clicks|n,c ~ MN(n, p1, p2,...,pb),
  - views|n,c ~ MN(n, p1, p2,...,pc), and
  - a, b, c are contants that is pre-defined by modeler

# dotation:
- Poi: Poisson
- MN: Multinomial

# to run:
- sbt clean; sbt assembly; scala -J-Xmx1024m -cp target/scala-2.11/apple-assembly-0.0.1.jar model.Driver
- output is new_label.txt
