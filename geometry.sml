signature VECTOR = sig
  type vector
  val zero : vector
  val scale : real * vector -> vector
  val add : vector * vector -> vector
  val dot : vector * vector -> real
end

signature POINT = sig
  structure Vector : VECTOR
  type point
  (* move a point along a vector *)
  val translate : point * Vector.vector -> point
  (* the vector from a to b *)
  val ray : point * point -> Vector.vector
end

signature SPHERE = sig
  structure Vector : VECTOR
  structure Point : POINT
  sharing Point.Vector = Vector
  type sphere
  val sphere : Point.point * Vector.vector -> sphere
end

signature GEOMETRY = sig
  structure Point : POINT
  structure Sphere : SPHERE
  sharing Point = Sphere.Point
      and Point.Vector = Sphere.Vector
end
