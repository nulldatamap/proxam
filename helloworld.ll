; ModuleID = 'helloworld'

declare {} @print_int(i32)

define i32 @id(i32 %x) {
  ret i32 %x
}

define i32 @test_overapplication() {
  %1 = call i32 (i32)* (i32)* @wowser(i32 0)
  %2 = call i32 %1(i32 3)
  ret i32 %2
}

define i32 (i32)* @wowser(i32 %x) {
  ret i32 (i32)* @id
}

define {} @main() {
  %1 = call i32 (i32)* (i32)* @wowser(i32 0)
  %2 = call i32 %1(i32 10)
  %3 = call {} @print_int(i32 %2)
  ret {} %3
}