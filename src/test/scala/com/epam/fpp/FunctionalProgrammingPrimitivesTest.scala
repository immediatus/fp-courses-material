import org.specs2._
import com.epam.fpp._


class FunctionalProgrammingPremitive_Spec extends mutable.Specification {
  import FPP._

  "Type-Class: Compose" should {
    "For functions: \n f(x) => x + 3 \n g(x) => x * 2" >> {
      val f = (_:Int) + 3
      val g = (_:Int) * 2

      "Function composition (f >>> g) (3) == 12" in {
        (f >>> g) (3) must equalTo(12)
      }
      "Function composition (f <<< g) (3) == 9" in {
        (f <<< g) (3) must equalTo(9)
      }
    }
  }

  "Type-Class: Category" should {
    "For function: \n f(x) => x + 3" >> {
      val f = (_:Int) + 3

      "(f >>> Category[Function1].id)(2) == 5" in {
        (f >>> Category[Function1].id)(2) must equalTo(5)
      }
      "(f >>> identity)(2) == 5" in {
        (f >>> identity)(2) must equalTo(5)
      }
      "(f <<< identity[Int])(2) == 5" in {
        (f <<< identity[Int])(2) must equalTo(5)
      }
    }
  }

  "Type-Class: Functor" should {
    "For functions: \n f(x) => x + 3 \n g(x) => x.toString" >> {
      val f = (_: Int) + 3
      val g = (_: Int).toString

      "Associative Law: List(1, 2, 3) ∘ ( f >>> g) == (List(1, 2, 3) ∘ f) ∘ g" in {
        List(1, 2, 3) ∘ ( f >>> g) must_==  (List(1, 2, 3) ∘ f) ∘ g
      }
      "Associative Law: Option(1) ∘ ( f >>> g) == (Option(1) ∘ f) ∘ g" in {
        List(1, 2, 3) ∘ ( f >>> g) must_==  (List(1, 2, 3) ∘ f) ∘ g
      }
    }
  }

  "Type-Class: Applicativa" should {
    "For function: \n f(x, y) => x + y" >> {
      val f = (_: Int) + (_: Int)

      "Option(3) <*> { Option(5) map f.curried } == Option(8)" in {
        Option(3) <*> { Option(5) map f.curried } must_== Option(8)
      }
      "(Option(3) |@| Option(5))(f) == Option(8)" in {
        (Option(3) |@| Option(5))(f) must_== Option(8)
      }
      "Applicative[List].lift2(f)(List(1, 2), List(3, 4)) == List(4, 5, 5, 6)" in {
        Applicative[List].lift2(f)(List(1, 2), List(3, 4)) must_== List(4, 5, 5, 6)
      }
      "Applicative[Option].lift2(f)(Option(1), Option(3)) == Option(4)" in {
        Applicative[Option].lift2(f)(Option(1), Option(3)) must_== Option(4)
      }
    }

    "For function: \n g(x, y, z) => x + y - z" >> {
      val g = (x: Int, y: Int, z: Int) => x + y - z

      "Applicative[List].lift3(g)(List(1, 2), List(3, 4), List(1, 2)) == List(3, 4, 6, 8)" in {
        Applicative[List].lift3(g)(List(1, 2), List(3, 4), List(1, 2)) must_== List(3, 2, 4, 3, 4, 3, 5, 4)
      }
      "Apply[Option].lift3(g)(Option(1), Option(3), Option(2)) == Option(2)" in {
        Applicative[Option].lift3(g)(Option(1), Option(3), Option(2)) must_== Option(2)
      }
    }

    "For functions: \n f(x, y) => x + y\n g(x, y, z) => x + y - z" >> {
      val f = (_: Int) + (_: Int)
      val g = (x: Int, y: Int, z: Int) => x + y - z

      "Applicative[({type l[X] = Function1[Int, X]})#l].lift2(f)((_:Int) + 5, (_:Int) * 2) apply (2) ==  11" in {
        Applicative[({type l[X] = Function1[Int, X]})#l].lift2(f)((_:Int) + 5, (_:Int) * 2) apply (2) must equalTo(11)
      }
      "Applicative[({type l[X] = Function1[Int, X]})#l].lift3(g)((_:Int) + 5, (_:Int) * 2, (_:Int) - 5) apply (2) ==  14" in {
        Applicative[({type l[X] = Function1[Int, X]})#l].lift3(g)((_:Int) + 5, (_:Int) * 2, (_:Int) - 5) apply (2) must equalTo(14)
      }
    }
  }

  "Type-Class: Monad" should {
    "For special function: \n f(x) => (1 to x).toList" >> {
      val f = (x: Int) => (1 to x).toList

      "List(2, 3, 4) >>= f == List(1, 2, 1, 2, 3, 1, 2, 3, 4)" in {
        (List(2, 3, 4) >>= f) must_== List(1, 2, 1, 2, 3, 1, 2, 3, 4)
      }
    }
    "Option(Option(2)).join == Option(2)" in {
      Option(Option(2)).join must_== Option(2)
    }

    "For special functions: \n f(x) => x + 10\n g(x) => s\"x = x + $x\"" >> {
      val f = (x: Int) => x + 10
      val g = (x: Int) => s"x = x + $x"

      """{for {
x <- g
y <- h
} yield (x, y)} apply 10 == (20, \"x = x + 10\")""" in {
        {for {
          x <- f
          y <- g
        } yield (x, y)} apply 10 must_== (20, "x = x + 10")
      }
    }
    "(((x: Int) => (y: Int) => x + y).join) apply 2 == 4" in {
      (((x: Int) => (y: Int) => x + y).join) apply 2 must equalTo(4)
    }
  }

  "Type-Class: Kleisli" should {
    "For functions: \n f(x) => Some(x + 1)\n g(x) => if(x < 100) None else Some(x * 2)" >> {
      val f = (x: Int) => if(x > 100) None else Option(x + 1)
      val g = (x: Int) => Option(x * 2)

      "(Option(2) >>= Kleisli(f) >=> Kleisli(g)) == Option(6)" in {
        (Option(2) >>= Kleisli(f) >=> Kleisli(g)) must_== Option(6)
      }
      "(Option(200) >>= Kleisli(f) >=> Kleisli(g)) == None" in {
        (Option(200) >>= Kleisli(f) >=> Kleisli(g)) must_== None
      }
      "(Kleisli(f) >=> Kleisli(g) =<< Option(2)) == Option(6)" in {
        (Kleisli(f) >=> Kleisli(g) =<< Option(2)) must_== Option(6)
      }
    }
  }

  "Type-Class: Arrow" should {
    "For functions: \n f(x) => Some(x + 1)\n g(x) => if(x < 100) None else Some(x * 2)" >> {
      val f = (x: Int) => if(x > 100) None else Option(x + 1)
      val g = (x: Int) => Option(x * 2)

      "(f &&& g)(10) == (Option(11), Option(20))" in {
        (f &&& g)(10) must_== (Option(11), Option(20))
      }
      "(f *** g)(200, 10) must_== (None, Option(20))" in {
        (f *** g)(200, 10) must_== (None, Option(20))
      }
      "f.first(7, \"abc\") must_== (Option(8), \"abc\")" in {
        f.first(7, "abc") must_== (Option(8), "abc")
      }
      "g.second(\"abc\", 7) must_== (\"abc\", Option(14))" in {
        g.second("abc", 7) must_== ("abc", Option(14))
      }
    }
  }
}
