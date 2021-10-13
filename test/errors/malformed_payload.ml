let () = try f () with [%effect? E (* missing [, k] *)] -> ()
