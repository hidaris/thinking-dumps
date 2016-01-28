# use ruby
(1..100).each do |n|
  if (n % 15).zero?
    puts 'FizzBuzz'
  elsif (n % 3).zero?
    puts 'Fizz'
  elsif (n % 5).zero?
    puts 'Buzz'
  else
    puts n.to_s
  end
end

# E2
(1..100).map do |n|
  if (n % 15).zero?
    'FizzBuzz'
  elsif (n % 3).zero?
    'Fizz'
  elsif (n % 5).zero?
    'Buzz'
  else
    n.to_s
  end
end

# church encoding, church numeral.
ZERO    = -> p { -> x {       x    } }
ONE     = -> p { -> x {     p[x]   } }
TWO     = -> p { -> x {   p[p[x]]  } }
THREE   = -> p { -> x { p[p[p[x]]] } }
FIVE    = -> p { -> x { p[p[p[p[p[x]]]]] } }
FIFTEEN = -> p { -> x { p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[x]]]]]]]]]]]]]]] } }
HUNDRED = -> p { -> x { p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[x]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] } }

def to_integer(proc)
  proc[-> n { n + 1 }][0]
end

# (ONE..HUNDRED).map do |n|
#   if (n % FIFTEEN).zero?
#     'FizzBuzz'
#   elsif (n % THREE).zero?
#     'Fizz'
#   elsif (n % FIVE).zero?
#     'Buzz'
#   else
#     n.to_s
#   end
# end

TRUE  = -> x { -> y { x } }
FALSE = -> x { -> y { y } }

# def to_boolean(proc)
#   proc[true][false]
# end

# if(proc, x, y) = proc[x][y]
# IF =
#   -> p {
#     -> x {
#       -> y {
#         p[x][y]
#       }
#     }
#   }
IF =
  -> p { p }

# change to_boolean by IF
def to_boolean(proc)
  IF[proc][true][false]
end

# rewrite by IF
# (ONE..HUNDRED).map do |n|
#   IF[(n % FIFTEEN).zero?][
#     'FizzBuzz'
#   ][IF[(n % THREE).zero?][
#       'Fizz'
#     ][IF[(n % FIVE).zero?][
#         'Buzz'
#       ][
#         n.to_s
#       ]]]
# end

# def zero?(proc)
#   proc[-> x { FALSE }][TRUE]
# end
IS_ZERO = -> p { p[-> x { FALSE }][TRUE]}

# rewrite by IS_ZERO
# (ONE..HUNDRED).map do |n|
#   IF[IS_ZERO[n % FIFTEEN]][
#     'FizzBuzz'
#   ][IF[IS_ZERO[n % THREE]][
#       'Fizz'
#     ][IF[IS_ZERO[n % FIVE]][
#         'Buzz'
#       ][
#         n.to_s
#       ]]]
# end

PAIR  = -> x { -> y { -> f { f[x][y] } } }
LEFT  = -> p { p[-> x { -> y { x } } ] }
RIGHT = -> p { p[-> x { -> y { y } } ] }

INCREMENT = -> proc { -> proc { -> x { p[proc[p][x]] } } }

def slide(pair)
  [pair.last, pair.last + 1]
end
