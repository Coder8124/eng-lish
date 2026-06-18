# geometry package

Use the `geometry` package when you want to measure shapes, distances, and angles.

```eng
use "geometry".

output the result of distance with 0.0 and 0.0 and 3.0 and 4.0.
output the result of circleArea with 5.0.
```

## Functions

- `distance` finds the distance between two points: `(x1, y1)` and `(x2, y2)`.
- `hypotenuse` finds the longest side of a right triangle.
- `triangleArea` finds the area of a triangle from its base and height.
- `circleArea` finds the area inside a circle.
- `circumference` finds the distance around a circle.
- `degreesToRadians` changes degrees into radians.
- `radiansToDegrees` changes radians into degrees.

## Point

`Point` stores an `x` and `y` position.

```eng
use "geometry".

let start be a Point created with 0.0 and 0.0.
output the result of asking start to distanceTo with 3.0 and 4.0.
```
