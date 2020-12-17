

val years = Set(11,12,13,14,15,16,17,18,19)


val z = Map((11, 20),
  (12, 29),
  (13, 24),
(14, 19),
(15, 29),
(16, 27),
(17, 41),
(18, 27),
(19, 30))

val ys = for {
  j1 <- Range(11, 20)
  j2 <- Range(j1, 20)
  j3 <- Range(j2, 20)
  j4 <- Range(j3, 20)
  j5 <- Range(j4, 20)
  j6 <- Range(j5, 20)
  j7 <- Range(j6, 20)
  j8 <- Range(j7, 20)
//  n1 <- Range(1, z(11) + 1)
//  n2 <- Range(1, z(17) + 1)
//  n3 <- Range(1, z(14) + 1)
//  n4 <- Range(1, z(15) + 1)
//  n5 <- Range(1, z(j5) + 1)
//  n6 <- Range(1, z(j6) + 1)
//  n7 <- Range(1, z(j7) + 1)
//  n8 <- Range(1, z(j8) + 1)
  if {
    BigInt(j1)*BigInt(j2)*BigInt(j3)*BigInt(j4)*BigInt(j5)*BigInt(j6)*BigInt(j7)*BigInt(j8) % 314160 == 0
//    n1/11 + n2/17 + n3/14 + n4/15 + n5/j5 + n6/j6 +n7/j7 + n8/j8 == 1466473/314160
  }
} yield {
  (List(j1,j2,j3,j4,j5,j6,j7,j8), BigInt(j1)*BigInt(j2)*BigInt(j3)*BigInt(j4)*BigInt(j5)*BigInt(j6)*BigInt(j7)*BigInt(j8)/314160)

  //  (n1, 11 ,n2, 17, n3, 14, n4, 15,n5,j5,n6, j6,n7,j7,n8, j8)
}

println(ys.size)

ys.map{case (y, v) => y.groupBy(identity).