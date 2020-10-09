package homeworks.basics

import homeworks.basics.Task2._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Task2Test extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "MoveTwoDimensional" should "be correct" in {
    var circle = Circle.apply(1.2, 2.4, 5)
    circle = circle.move(2, 3)
    circle.coordinates.x shouldEqual 3.2
    circle.coordinates.y shouldEqual 5.4
    circle.radius shouldEqual 5.0

    circle.bound.minX shouldEqual -1.7999999999999998
    circle.bound.maxX shouldEqual 8.2
    circle.bound.minY shouldEqual 0.40000000000000036
    circle.bound.maxY shouldEqual 10.4
    circle.parameters.area shouldEqual 78.53981633974483
  }

  "MoveThreeDimensional" should "be correct" in {
    var point = Point.apply(1.2, 2, 0)
    point = point.move(2, 3, 1)
    point.x shouldEqual 3.2
    point.y shouldEqual 5.0
    point.z shouldEqual 1.0
    point.bound.minX shouldEqual 3.2
    point.bound.maxX shouldEqual 3.2
    point.bound.minY shouldEqual 5.0
    point.bound.maxY shouldEqual 5.0
    point.bound.minZ shouldEqual 1.0
    point.bound.maxZ shouldEqual 1.0

    point.parameters.surfaceArea shouldEqual 0.00
    point.parameters.volume shouldEqual 0.00
  }


  "Circle" should "be correct" in {
    val circle = Circle.apply(1.2, 2.4, 5)
    circle.bound.minX shouldEqual -3.8
    circle.bound.maxX shouldEqual 6.2
    circle.bound.minY shouldEqual -2.6
    circle.bound.maxY shouldEqual 7.4
    circle.radius shouldEqual 5.0

    circle.parameters.area shouldEqual 78.53981633974483
  }

  "Rectangle" should "be correct" in {
    val rectangle = Rectangle.apply(1.2, 2.4, 5.1, 3.3)
    rectangle.bound.minX shouldEqual -1.3499999999999999
    rectangle.bound.maxX shouldEqual 3.75
    rectangle.bound.minY shouldEqual 0.75
    rectangle.bound.maxY shouldEqual 4.05
    rectangle.width shouldEqual 5.1
    rectangle.length shouldEqual 3.3

    rectangle.parameters.area shouldEqual 16.83
  }

  "Square" should "be correct" in {
    val rectangle = Square.apply(2.2, 3.3, 4.4)
    rectangle.bound.minX shouldEqual 0.0
    rectangle.bound.maxX shouldEqual 4.4
    rectangle.bound.minY shouldEqual 1.0999999999999996
    rectangle.bound.maxY shouldEqual 5.5
    rectangle.facet shouldEqual 4.4

    rectangle.parameters.area shouldEqual 19.360000000000003
  }

  //Shapes 3D
  "Point" should "be correct" in {
    val point = Point.apply(1.2, 2.4, 5)
    point.bound.maxX shouldEqual 1.2
    point.bound.minX shouldEqual 1.2
    point.bound.maxY shouldEqual 2.4
    point.bound.minY shouldEqual 2.4
    point.bound.maxZ shouldEqual 5.0
    point.bound.minZ shouldEqual 5.0

    point.parameters.surfaceArea shouldEqual 0.00
    point.parameters.volume shouldEqual 0.00
  }

  "Sphere" should "be correct" in {
    val sphere = Sphere.apply(2.2, 3.3, 4.4, 5.2)
    sphere.bound.minX shouldEqual -3.0
    sphere.bound.maxX shouldEqual 7.4
    sphere.bound.minY shouldEqual -1.9000000000000004
    sphere.bound.maxY shouldEqual 8.5
    sphere.bound.maxZ shouldEqual 9.600000000000001
    sphere.bound.minZ shouldEqual -0.7999999999999998
    sphere.radius shouldEqual 5.2

    sphere.parameters.surfaceArea shouldEqual 339.79466141227203
    sphere.parameters.volume shouldEqual 588.9774131146049
  }

  "Cube" should "be correct" in {
    val cube = Cube.apply(1.1, 3.3, 4.4, 2.2)
    cube.bound.minX shouldEqual 0.0
    cube.bound.maxX shouldEqual 2.2
    cube.bound.minY shouldEqual 2.1999999999999997
    cube.bound.maxY shouldEqual 4.4
    cube.bound.minZ shouldEqual 3.3000000000000003
    cube.bound.maxZ shouldEqual 5.5
    cube.facet shouldEqual 2.2

    cube.parameters.surfaceArea shouldEqual 29.040000000000006
    cube.parameters.volume shouldEqual 10.648000000000003
  }

}
