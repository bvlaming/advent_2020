
val x = List(1,2,3,4,5,6,7,8,9,0)
//x.permutations.filter{
//  case List(a,b,c,d,e,f,g,h,i,j) => {
//    val b1 = Set(2,3,5,7).contains(a)
//    val b2 = math.pow(e, h) == f*10 + g
//    val b3 = b * f == 10*h + d
//    val b4 = i*10 + g - e*10 - j == 11*a
//    b1 & b2 & b3 & b4
//  }
//}.toList
//x.permutations.filter{
//  case y => {
//    val b1 =
//  }
//}
2*11*2013
5*22*5925

//198632820000 * 997 + maxInt

val a = List(0, 'X', 'X')
val b = List(0, 1)
val d = Map((0, List(0)), ('X', List(0,1)))
val z = a.map(d)

z.foldLeft(List.empty: List[List[Int]]){
  case (acc: List[List[Int]], cs: List[Int]) => {
    println(cs)
    println(acc)
    val s = for (c <- cs) yield {
      acc.map(f => c :: f)
    }
    println(s.flatten)
//    List(List(0))
    s.flatten

  }
}
