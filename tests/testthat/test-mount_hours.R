test_that("there are no lesson overlaps", {
  MP <- long2matrix(as.data.frame(mount_hours(LSS, Tuplaje)))
  vrf <- apply(MP, 2, table)
  tf <- sapply(1:length(vrf), function(i) any(vrf[[i]][-1] > 1))
  expect_equal(all(tf==FALSE), TRUE)
})
