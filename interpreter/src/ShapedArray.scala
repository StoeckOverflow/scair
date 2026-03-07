package scair.interpreter

case class ShapedArray(
    shape: Seq[Int],
    private val data: Array[Any],
    private val baseOffset: Int,
):

  require(shape.forall(_ >= 0), "Shape dimensions must be non-negative")

  require(baseOffset >= 0, s"Base offset must be non-negative, got $baseOffset")
  require(
    baseOffset + shape.product <= data.length,
    s"View out of bounds: base=$baseOffset shape=$shape dataLength=${data.length}",
  )

  lazy val strides: Seq[Int] =
    shape.scanRight(1)(_ * _).tail

  def length: Int =
    shape.product

  private def offset(indices: Seq[Int]): Int =
    require(
      indices.length == shape.length,
      s"Expected ${shape.length} indices, got ${indices.length}",
    )
    require(
      indices.zip(shape).forall((i, dim) => i >= 0 && i < dim),
      s"Index out of bounds: indices=$indices shape=$shape",
    )
    baseOffset + indices.zip(strides).map(_ * _).sum

  def apply(indices: Seq[Int]): Any = data(offset(indices))

  def update(indices: Seq[Int], value: Any): Unit =
    data(offset(indices)) = value

  def subview(offsets: Seq[Int], sizes: Seq[Int]): ShapedArray =
    require(
      offsets.length == shape.length && sizes.length == shape.length,
      s"Subview rank mismatch: offsets=${offsets.length} sizes=${sizes.length} rank=${shape.length}",
    )
    require(
      offsets.zip(sizes).zip(shape).forall { case ((off, sz), dim) =>
        off >= 0 && sz >= 0 && off + sz <= dim
      },
      s"Subview out of bounds: offsets=$offsets sizes=$sizes shape=$shape",
    )
    val newBase = baseOffset + offsets.zip(strides).map(_ * _).sum
    ShapedArray(shape = sizes, data = data, baseOffset = newBase)

object ShapedArray:

  def apply(shape: Seq[Int]): ShapedArray =
    new ShapedArray(
      shape = shape,
      data = Array.fill(shape.product)(0),
      baseOffset = 0,
    )
