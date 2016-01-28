-> x, y {
  x + y
}.call(3, 4)

-> x {
  -> y {
    x + y
  }
}.call(3).call(4)

# p, q are extensional equality.
p = -> n { n * 2}
q = -> x { p.call(x) }
# -> n { n + 5 }[6] #=> 11
