package scair.interpreter

case class ShapedArray(
    shape: Seq[Int],
    private val data: Array[Any],
    private val baseOffset: Int,
    strides: Seq[Int],
):

  require(shape.forall(_ >= 0), "Shape dimensions must be non-negative")

  require(baseOffset >= 0, s"Base offset must be non-negative, got $baseOffset")
  require(
    strides.length == shape.length,
    s"Stride rank mismatch: shape=${shape.length} strides=${strides.length}",
  )
  require(strides.forall(_ >= 0), s"Strides must be non-negative, got $strides")
  private val maxReach =
    if shape.isEmpty then 0
    else shape.zip(strides).map {
      case (0, _)            => 0
      case (dim, stride)     => (dim - 1) * stride
    }.sum
  require(
    baseOffset + maxReach < data.length || (shape.product == 0 && baseOffset <= data.length),
    s"View out of bounds: base=$baseOffset shape=$shape strides=$strides dataLength=${data.length}",
  )

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

  def subview(offsets: Seq[Int], sizes: Seq[Int], subviewStrides: Seq[Int]): ShapedArray =
    require(
      offsets.length == shape.length &&
        sizes.length == shape.length &&
        subviewStrides.length == shape.length,
      s"Subview rank mismatch: offsets=${offsets.length} sizes=${sizes.length} strides=${subviewStrides.length} rank=${shape.length}",
    )
    require(
      offsets.zip(sizes).zip(subviewStrides).zip(shape).forall {
        case (((off, sz), stride), dim) =>
          off >= 0 && sz >= 0 && stride >= 0 &&
          (if sz == 0 then off <= dim else off + (sz - 1) * stride < dim)
      },
      s"Subview out of bounds: offsets=$offsets sizes=$sizes strides=$subviewStrides shape=$shape",
    )
    val newBase = baseOffset + offsets.zip(strides).map(_ * _).sum
    val newStrides = strides.zip(subviewStrides).map(_ * _)
    ShapedArray(shape = sizes, data = data, baseOffset = newBase, strides = newStrides)

  def reinterpret(offset: Int, sizes: Seq[Int], newStrides: Seq[Int]): ShapedArray =
    require(
      sizes.length == newStrides.length,
      s"Reinterpret rank mismatch: sizes=${sizes.length} strides=${newStrides.length}",
    )
    require(offset >= 0, s"reinterpret offset must be non-negative, got $offset")
    require(newStrides.forall(_ >= 0), s"reinterpret strides must be non-negative, got $newStrides")
    ShapedArray(shape = sizes, data = data, baseOffset = baseOffset + offset, strides = newStrides)

object ShapedArray:

  def apply(shape: Seq[Int]): ShapedArray =
    new ShapedArray(
      shape = shape,
      data = Array.fill(shape.product)(0),
      baseOffset = 0,
      strides = shape.scanRight(1)(_ * _).tail,
    )
