
triangles n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], y > x, z > y, x ^ 2 + y ^ 2 == z ^ 2]
