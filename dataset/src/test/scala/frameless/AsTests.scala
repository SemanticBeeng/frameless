package frameless

import org.scalacheck.Prop
import org.scalacheck.Prop._


class AsTests extends TypedDatasetSuite {
  test("as[X2[A, B]]") {
    def prop[A, B](data: Vector[(A, B)])(
      implicit
      eab: TypedEncoder[(A, B)],
      ex2: TypedEncoder[X2[A, B]]
    ): Prop = {
      val dataset = TypedDataset.create(data)

      val dataset2 = dataset.as[X2[A, B]]().collect().run().toVector
      val data2 = data.map { case (a, b) => X2(a, b) }

      dataset2 ?= data2
    }

    check(forAll(prop[Int, Int] _))
    check(forAll(prop[String, String] _))
    check(forAll(prop[String, Int] _))
    check(forAll(prop[Long, Int] _))
    check(forAll(prop[Seq[Seq[Option[Seq[Long]]]], Seq[Int]] _))
    check(forAll(prop[Seq[Option[Seq[String]]], Seq[Int]] _))
  }

  test("as[X2[X2[A, B], C]") {
    def prop[A, B, C](data: Vector[(A, B, C)])(
      implicit
      eab: TypedEncoder[((A, B), C)],
      ex2: TypedEncoder[X2[X2[A, B], C]]
    ): Prop = {
      val data2 = data.map {
        case (a, b, c) => ((a, b), c)
      }
      val dataset = TypedDataset.create(data2)

      val dataset2 = dataset.as[X2[X2[A,B], C]]().collect().run().toVector
      val data3 = data2.map { case ((a, b), c) => X2(X2(a, b), c) }

      dataset2 ?= data3
    }

    check(forAll(prop[String, Int, Int] _))
    check(forAll(prop[String, Int, String] _))
    check(forAll(prop[String, String, Int] _))
    check(forAll(prop[Long, Int, String] _))
    check(forAll(prop[Seq[Seq[Option[Seq[Long]]]], Seq[Int], Option[Seq[Option[Int]]]] _))
    check(forAll(prop[Seq[Option[Seq[String]]], Seq[Int], Seq[Option[String]]] _))
  }

//  /**
//    * #todo @wip on https://github.com/typelevel/frameless/issues/279
//    */
//  test("as nested") {
//    sealed trait T
//    case class K(a: String, b: Int) //extends T
//    case class I(i: K, j: Int) //extends T
//
//    def prop[B](data: Vector[B])(
//      implicit
//      //eaa: TypedEncoder[A],
//      eab: TypedEncoder[B]
//    ): Prop = {
//      val dataset = TypedDataset.create(data)
//
//      import shapeless._
//      Generic[I]
//      val ti = TypedEncoder[I]
//      val d = I(K("string", 1), 2)
//      val e = ti.catalystRepr
//      println(e)
//
//      implicit val asI = As
//      dataset.as[I] ?= null//dataset.as[I]
//    }
//
//    check(forAll(prop[((String, Int), Int)] _))
//  }
//
//  /**
//    * https://stackoverflow.com/questions/36746028/is-it-possible-to-automatically-derive-a-sealed-trait-family-adt
//    * https://github.com/alexarchambault/scalacheck-shapeless/issues/1
//    * https://github.com/milessabin/shapeless/issues/417
//    * https://github.com/alexarchambault/argonaut-shapeless/issues/79
//    * https://github.com/circe/circe/issues/251
//    */
//  trait Defs {
//    case class CC(i: Int, s: String)
//
//    sealed trait Sum
//    case class SumI(i: Int) extends Sum
//    case class SumS(s: String) extends Sum
//  }
//
//  object Defs extends Defs
//
//  object Derivations {
//    import shapeless._
//
//    // These are fine
//    Generic[Defs.CC]
//    Generic[Defs.SumI]
//    Generic[Defs.SumS]
//
//    // These fail
//    Generic[Defs.Sum]
//    Generic.materialize[Defs.Sum, Defs.SumI :+: Defs.SumS :+: CNil]
//  }
}
