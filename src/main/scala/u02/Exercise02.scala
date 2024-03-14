package u02

object Exercise02 extends App:

  def positive(x: Int): String = x match
    case x if x >= 0 => "positive"
    case _           => "negative"
