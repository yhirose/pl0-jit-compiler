
def fib(x)
  return 1 if x == 0
  return 1 if x == 1
  return fib(x - 2) + fib(x - 1)
end

35.times do |i|
  puts(fib(i))
end
