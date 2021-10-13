let () = try f () with [%effect? E, k when true] when false -> ()
