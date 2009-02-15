let version =
  "0.4.1[2009-xx-xx]+auto-import"

module OS = struct
  let dir_sep =
    if Sys.os_type = "Win32" then
      ";"
    else
      ":"

  let base =
    Filename.dirname Sys.executable_name

  let default_includes =
    if Sys.os_type = "Win32" then
      [Filename.concat base "./lib";"."]
    else
      [Filename.concat base "../lib/habc/";"."]

  let default_template =
    if Sys.os_type = "Win32" then
      Filename.concat base "./template.xml"
    else
      Filename.concat base "../share/habc/template.xml"
end

