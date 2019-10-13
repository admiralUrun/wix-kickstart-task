import org.specs2.mutable.Specification
import Messages._

class MessagesSpec extends Specification {
  type K = Int
  type NumberOfMessages = Int
  type TestTuple = (String, K, NumberOfMessages)

  val t1: TestTuple = ("You're going down You're going down" , 12 , 3)
  val t2: TestTuple = ("Hey. hey", 4, 2)
  val t3: TestTuple = ("Someone said you're feeling brave tonight", 14, 3)
  val t4: TestTuple = ("Step up", 5, 2)
  val t5: TestTuple = ("Do you really wanna cross the line?", 13, 3)
  val t6: TestTuple = ("Right now", 7, 2)
  val t7: TestTuple = ("Go ahead and try to take what's mine", 10, 5)
  val t8: TestTuple = ("Let it roll, let it roll, I'm unstoppable Let it roll, let it roll, I'm unstoppable Let it roll, let it roll", 27 , 5)
  val t9: TestTuple = ("So you think you can shoot your mouth off Well you better watch your aim Cause I'm loaded and you're in my house You wanna stay, you're gonna have to pay", 1, -1)
  val t10: TestTuple = ("Try to hide but there's no way out", 4, -1)
  val t11: TestTuple = ("Hello", 5, 1)
  val t12: TestTuple = ("really", 6, 1)

  val list : List[TestTuple] = List(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)

  "Words are too long"  >> {
    functionSolution("Rousing from dreams crying someone", 6) must_== -1
    functionSolution("Where is the candle in this lonesome night", 7) must_== -1
    functionSolution("As long as I can always keep you safe", 5) must_== -1
    functionSolution(t9._1, t9._2) must_== t9._3
    functionSolution(t10._1, t10._2) must_== t10._3
    forall(list)(t => functionSolution(t._1, 2) must_== -1)
  }

  "Split first word" >> {
    functionSolution("Rousing from dreams crying someone", 7) must_== 5
    functionSolution("Do it", 2) must_== 2
    functionSolution("What is this", 4) must_== 3
  }


  "Single word in a Message" >> {
    functionSolution(t11._1, t11._2) must_== t11._3
    functionSolution(t12._1, t12._2) must_== t12._3
  }

  "Doesn't split in a middle of word" >> {
    functionSolution(t3._1, t3._2) must_== t3._3
    functionSolution(t5._1, t5._2) must_== t5._3
    functionSolution(t6._1, t6._2) must_== t6._3
    functionSolution(t7._1, t7._2) must_== t7._3
    functionSolution(t8._1, t8._2) must_== t8._3
  }

  "Splitting in the end" >> {
    functionSolution(t2._1, t2._2) must_== t2._3
    functionSolution(t4._1, t4._2) must_== t4._3
    functionSolution(t5._1, t5._2) must_== t5._3
    functionSolution(t6._1, t6._2) must_== t6._3
   }


  "Right split messages" >> {
    forall(list)(t => functionSolution(t._1, t._2) must_== t._3)
  }

}
