package CoreSyntax

/**
 * Created by Matthias on 31/08/14.
 */
sealed trait Side{
  def loc:Int
}
case object Left extends Side{
  def loc = 0;
}
case object Right extends Side{
  def loc = 1;
}

