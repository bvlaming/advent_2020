
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

val abc = "abcdefghijklmnopqrstuvwxyz".toCharArray
val u = abc.zipWithIndex.map{case (n, idx) => (n, idx + 1)}

u
val d = (7, 6, -11, 26, 5, 12)





val m = Map('b' -> '.', 'r' -> ' ', 'g' -> '-')

val v = "rrbgbrbgbrrgbbrbrbgbrrbrbgbrgb".toCharArray

v.map(m).mkString