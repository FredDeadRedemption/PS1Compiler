let import =
  let buffer = Buffer.create 128 in
  let add_str s = Buffer.add_string buffer s in
  add_str "#include <stdio.h>\n"; Buffer.contents buffer
