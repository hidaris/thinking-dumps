thrower = -> (_) { raise 'uh oh' }
inc = -> (n) { n + 1 }

val = 3
begin
  val = thrower[val]
  begin
    p val = inc[val]
  rescue
    p val
  end
rescue
  begin
    p val = inc[val]
  rescue
    p val
  end
end
