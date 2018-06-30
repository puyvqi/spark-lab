val n = 7
(1 to n).flatMap(i=>
  (1 to i).map(j=>(i,j))
)
val flag = for(i<-1 to 10; j<-1 to 3)yield (i,j)
flag.distinct
val a = (5 to 1 by -1).toArray
((1 to 5).toArray).zip((5 to 1 by -1))