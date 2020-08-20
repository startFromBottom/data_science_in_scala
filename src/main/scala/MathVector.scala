import scala.collection.LazyZip2

case class MathVector(value: List[Double]) {

  def length: Int = this.value.length

  private def zip(that: MathVector): LazyZip2[Double, Double, value.type] = {
    require(this.length == that.length)
    this.value.lazyZip(that.value)
  }

  private def sumOfSquare: Double = this.value.map(math.pow(_, 2)).sum

  private def magnitude: Double = math.sqrt(sumOfSquare)

  private def zipMap(that: MathVector)(f: (Double, Double) => Double): List[Double] =
    ((this zip that) map (f))

  def add(that: MathVector): MathVector = MathVector(zipMap(that)(_ + _))

  def +(that: MathVector): MathVector = add(that)

  def subtract(that: MathVector): MathVector = MathVector(zipMap(that)(_ - _))

  def -(that: MathVector): MathVector = subtract(that)

  def squareDistance(that: MathVector): Double =
    math.sqrt(zipMap(that)((w, v) => math.pow(w - v, 2)).sum)

  def dot(that: MathVector): Double =
    zipMap(that)(_ * _).sum

  def *(that: MathVector): Double = dot(that)

  def scalaMultiply(num: Double): MathVector =
    MathVector(this.value map (_ * num))

  def project(that: MathVector): MathVector = {
    val projLength = this * that
    that scalaMultiply projLength
  }

  def removeProjectionFromVector(that: MathVector): MathVector =
    this - (this project that)

  def numDifferences(that: MathVector): Int =
    (this zip that).foldLeft(0)((a, b) => if (b._1 == b._2) a + 1 else a)

}

object MathVector {

  def vectorSum(vectors: List[MathVector]): MathVector = vectors.reduceLeft(_ + _)

  def vectorMean(vectors: List[MathVector]): MathVector = {
    val sum = vectorSum(vectors)
    sum scalaMultiply (1f / vectors.length)
  }

  def deMean(vectors: List[MathVector]): List[MathVector] = {
    val mean = vectorMean(vectors)
    vectors.map(v => v subtract mean)
  }

  def removeProjection(vectors: List[MathVector], w: MathVector): List[MathVector] =
    vectors.map(v => v removeProjectionFromVector w)


}