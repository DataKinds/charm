pa = 0
a = 1
20.times do |n|
    puts a
    b = pa + a
    pa = a
    a = b
end
