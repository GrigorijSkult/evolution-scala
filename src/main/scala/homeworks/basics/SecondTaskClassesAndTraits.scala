package homeworks.basics

object SecondTaskClassesAndTraits {

  /*
  * Homework:
  *
  * 2D shapes: circle, rectangle, square and (triangle - unimplemented)
  * with `area` methods
  *
  * 3D shapes: point, sphere, cube and (cuboid, triangle - unimplemented)
  * with `surfaceArea` and `volume` methods
  * */

  //Coordinates
  sealed trait Coordinates
  case class CoordinatesTwoDimensional(cordX: Double, cordY: Double) extends Coordinates {
    def x: Double = cordX
    def y: Double = cordY
}

  case class CoordinatesThreeDimensional(cordX: Double, cordY: Double, cordZ: Double) extends Coordinates {
    def x: Double = cordX
    def y: Double = cordY
    def z: Double = cordZ
  }

  sealed trait LocationCoordinates[C <: Coordinates] {
    def coordinates: C
  }

  //Bounded
  sealed trait Bounded
  case class BoundedTwoDimensional(minValX: Double, maxValX: Double, minValY: Double, maxValY: Double) extends Bounded {
    def minX: Double = minValX
    def maxX: Double = maxValX
    def minY: Double = minValY
    def maxY: Double = maxValY
  }

  case class BoundedThreeDimensional(minValX: Double, maxValX: Double, minValY: Double, maxValY: Double, minValZ: Double, maxValZ: Double) extends Bounded {
    def minX: Double = minValX
    def maxX: Double = maxValX
    def minY: Double = minValY
    def maxY: Double = maxValY
    def minZ: Double = minValZ
    def maxZ: Double = maxValZ
  }

  sealed trait BoundedCoordinates[B <: Bounded]{
    def bound : B
  }

  //Parameters
  sealed trait Parameters
  case class ParametersTwoDimensional(areaInput:Double) extends Parameters {
    def area: Double = areaInput
  }

  case class ParametersThreeDimensional(surfaceAreaInput:Double, volumeInput:Double) extends Parameters {
    def surfaceArea: Double = surfaceAreaInput
    def volume: Double = volumeInput
  }

  sealed trait ParametersValues[P <: Parameters]{
    def parameters: P
  }


  sealed trait Shape[C <: Coordinates, B <: Bounded, P <: Parameters] extends LocationCoordinates[C] with BoundedCoordinates[B] with ParametersValues[P]

  sealed trait MovableTwoDimensional[C] {
    def move( dx: Double = 0, dy: Double = 0): C
  }

  sealed trait MovableThreeDimensional[C] {
    def move( dx: Double = 0, dy: Double = 0, dz: Double = 0): C
  }


  //Shapes 2D
  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape[CoordinatesTwoDimensional, BoundedTwoDimensional, ParametersTwoDimensional] with MovableTwoDimensional[Circle] {
    override def coordinates: CoordinatesTwoDimensional = CoordinatesTwoDimensional(centerX, centerY)
    override def bound: BoundedTwoDimensional = BoundedTwoDimensional(
    centerX - radius,
    centerX + radius,
    centerY - radius,
    centerY + radius
    )
    override def move(dx: Double, dy:Double): Circle = Circle(centerX + dx, centerY + dy, radius)
    override def parameters: ParametersTwoDimensional = ParametersTwoDimensional(math.Pi * radius * radius)
  }

  final case class Rectangle(x: Double, y: Double, width: Double, length: Double) extends Shape[CoordinatesTwoDimensional, BoundedTwoDimensional, ParametersTwoDimensional] with MovableTwoDimensional[Rectangle] {
    override def coordinates: CoordinatesTwoDimensional = CoordinatesTwoDimensional(x, y)
    override def bound: BoundedTwoDimensional = BoundedTwoDimensional(
    x - (width / 2),
    x + (width / 2),
    y - (length / 2),
    y + (length / 2)
    )
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, width, length)
    override def parameters: ParametersTwoDimensional = ParametersTwoDimensional(width * length)
  }

  final case class Square(x: Double, y: Double, facet: Double) extends Shape[CoordinatesTwoDimensional, BoundedTwoDimensional, ParametersTwoDimensional] with MovableTwoDimensional[Square] {
    override def coordinates: CoordinatesTwoDimensional = CoordinatesTwoDimensional(x, y)
    override def bound: BoundedTwoDimensional = BoundedTwoDimensional(
    x - (facet / 2),
    x + (facet / 2),
    y - (facet / 2),
    y + (facet / 2)
    )
    override def move(dx: Double, dy: Double): Square = Square(x + dx, y + dy, facet)
    override def parameters: ParametersTwoDimensional = ParametersTwoDimensional(facet * facet)
  }

  final case class Triangle(x: Double, y: Double) extends Shape[CoordinatesTwoDimensional, BoundedTwoDimensional, ParametersTwoDimensional] with MovableTwoDimensional[Triangle] {
    override def coordinates: CoordinatesTwoDimensional = ???
    override def bound: BoundedTwoDimensional = ???
    override def move(dx: Double, dy: Double): Triangle = Triangle(x + dx, y + dy)
    override def parameters: ParametersTwoDimensional = ???
  }


  //3D shapes
  final case class Point(x: Double, y: Double, z: Double) extends Shape[CoordinatesThreeDimensional, BoundedThreeDimensional, ParametersThreeDimensional] with MovableThreeDimensional[Point] {
    override def coordinates: CoordinatesThreeDimensional = CoordinatesThreeDimensional(x, y, z)
    override def bound: BoundedThreeDimensional = BoundedThreeDimensional(x, x, y, y, z, z)
    override def move(dx: Double, dy:Double, dz:Double): Point = Point(x + dx, y + dy, z + dz)
    override def parameters: ParametersThreeDimensional = ParametersThreeDimensional(0.00, 0.00)
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius:Double) extends Shape[CoordinatesThreeDimensional, BoundedThreeDimensional, ParametersThreeDimensional] with MovableThreeDimensional[Sphere] {
    override def coordinates: CoordinatesThreeDimensional = CoordinatesThreeDimensional(x, y, z)
    override def bound: BoundedThreeDimensional = BoundedThreeDimensional(
      x - radius,
      x + radius,
      y - radius,
      y + radius,
      z - radius,
      z + radius
    )
    override def move(dx: Double, dy:Double, dz:Double): Sphere = Sphere(x + dx, y + dy, z + dz, radius)
    override def parameters: ParametersThreeDimensional = ParametersThreeDimensional(4 * math.Pi * radius * radius, (4 * math.Pi * radius * radius * radius)/3)
  }

  final case class Cube(x: Double, y: Double, z:Double, facet: Double) extends Shape[CoordinatesThreeDimensional, BoundedThreeDimensional, ParametersThreeDimensional] with MovableThreeDimensional[Cube] {
    override def coordinates: CoordinatesThreeDimensional = CoordinatesThreeDimensional(x, y, z)
    override def bound: BoundedThreeDimensional = BoundedThreeDimensional(
      x - (facet / 2),
      x + (facet / 2),
      y - (facet / 2),
      y + (facet / 2),
      z - (facet / 2),
      z + (facet / 2)
    )
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(x + dx, y + dy, z + dz, facet)
    override def parameters: ParametersThreeDimensional = ParametersThreeDimensional(6 * facet * facet, facet * facet * facet)
  }

  final case class Cuboid(x: Double, y: Double, z:Double, w: Double, h: Double, l: Double) extends Shape[CoordinatesThreeDimensional, BoundedThreeDimensional, ParametersThreeDimensional] with MovableThreeDimensional[Cuboid] {
    override def coordinates: CoordinatesThreeDimensional = ???
    override def bound: BoundedThreeDimensional = ???
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = ???
    override def parameters: ParametersThreeDimensional = ???
  }

  final case class ThreeDimensionalTriangle(x: Double, y: Double, z:Double) extends Shape[CoordinatesThreeDimensional, BoundedThreeDimensional, ParametersThreeDimensional] with MovableThreeDimensional[ThreeDimensionalTriangle] {
    override def coordinates: CoordinatesThreeDimensional = ???
    override def bound: BoundedThreeDimensional = ???
    override def move(dx: Double, dy: Double, dz: Double): ThreeDimensionalTriangle = ???
    override def parameters: ParametersThreeDimensional = ???
  }

}
