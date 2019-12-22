## tests for pc2
i1 = c()
i2 = ""
i3 = "a b c d e f g"
i4 = "a b c d e f g x"
i5 = c("","")
i6 = c("",i3)
i7 = c(i3,i4)
i8 = " "
i9 = c(1953, 2019)
test_that("pc2 identical to tabr::pc when no redundant blanks", {
  expect_identical(pc2(i1),tabr::pc(i1) )
  expect_identical(pc2(i2),tabr::pc(i2) )
  expect_identical(pc2(i3),tabr::pc(i3) )
  expect_identical(pc2(i4),tabr::pc(i4) )
  expect_identical(pc2(i5),tabr::pc(i5) )
  expect_identical(pc2(i6),tabr::pc(i6) )
  expect_identical(pc2(i7),tabr::pc(i7) )
  expect_identical(pc2(i8),tabr::pc(i8) )
  expect_identical(pc2(i9),tabr::pc(i9) )
})
i3a = "  a b c d e f g"
i3b = "a b   c d  e f  g"
i3c = "a b   c d  e f  g  "
i8a = "   "
test_that("pc2 identical to tabr::pc when redundant blanks are removed", {
  expect_identical(pc2(i3a),tabr::pc(i3) )
  expect_identical(pc2(i3b),tabr::pc(i3) )
  expect_identical(pc2(i3c),tabr::pc(i3) )
  expect_identical(pc2(i8a),tabr::pc(i8) )
})
## tests for expand_notes
i9 = 'a3  b r c4 g3 f'
o9 = "a3 b3 r c#4 g3 f#3"
i10 = c('a3 b c g f',      'a3 b r4 c g f',      'a3 b c g f')
o10 = c( "a3 b3 c3 g3 f3", "a3 b3 r c#4 g#4 f#4","a3 b_3 c3 g3 f3" )
i11 = c('a3 b c g f',       'a3 b r4 c g f',     'a3 b c g f')
o11 = c("a3 b3 c#3 g3 f#3", "a3 b_3 r c4 g4 f4", "a3 b3 c#3 g3 f#3" )
i12 = 'a3  b r c4 g3 fx'
o12 = 'a3 b3 r c4 g3 f3'
i13 = 'a3  b r c4 g3 fx'
o13 = 'a3 b3 r c4 g3 f3'
i14 = 'a3  b r cx4 g3 fx'
o14 = 'a3 b3 r c4 g#3 f3'
i15 = 'a3 b c g f | a3 b c g f | a3 b c g f'
o15 = 'a3 b c g f | a3 b c g f | a3 b c g f'
o15a = "a3 b3 c#3 g3 f#3 | a3 b3 c#3 g3 f#3 | a3 b3 c#3 g3 f#3"
o15b = "a3 b3 c#3 g3 f#3 a3 b3 c#3 g3 f#3 a3 b3 c#3 g3 f#3"
test_that("expand_notes", {
  expect_identical(expand_notes(i9,sh_fl = 2),o9 )
  expect_identical(expand_notes(i10,sh_fl = c(0,3,-1)),o10 )
  expect_identical(expand_notes(i11,sh_fl = c(2,-1)),o11 )
  expect_identical(expand_notes(i12,sh_fl = 1),o12 )
  expect_identical(expand_notes(i13,sh_fl = 0),o13 )
  expect_identical(expand_notes(i14,sh_fl = 3),o14 )
  expect_identical(expand_notes(i15,sh_fl = 2,rmv_mi = F),o15a )
  expect_identical(expand_notes(i15,sh_fl = 2,rmv_mi = T),o15b )
})
## tests for check_times
i16 = " 2. | 8*3 16*4 8|8*6| 8*3 16*4 8"
o16 = check_times(i16,8)
test_that("check_times", {
  expect_identical(names(o16),c("times","counts") )
  expect_identical(o16$times,"2. 8*3 16*4 8 8*6 8*3 16*4 8" )
  expect_identical(o16$counts,rep(6,4) )
})

