open Base

let filter table program =
  program
  +> Binding.bind table
  +> Rename.rename
