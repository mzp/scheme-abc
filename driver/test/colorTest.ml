open Base
open Color
open OUnit

let ok ?msg x y =
  OUnit.assert_equal ~printer:Std.dump ?msg
    x y

let _  = ("color.ml" >::: [
	    "#-format" >::
	      (fun () ->
		 ok {Color.red=255;
		     green=255;
		     blue=255;
		     alpha=1.0} @@
		   Color.parse "#ffffFF";
		 ok {Color.red=0x00;
		     green=0x22;
		     blue=0x33;
		     alpha=1.0} @@
		   Color.parse "#002233";
	      );
	    "rgb-format" >::
	      (fun () ->
		 ok {Color.red=255;
		     green=255;
		     blue=255;
		     alpha=1.0} @@
		   Color.parse "rgb(255,255,255)";
		 ok {Color.red=00;
		     green=22;
		     blue=33;
		     alpha=1.0} @@
		   Color.parse "rgb(00, 22 , 33)";
	      );
	    "name" >::
	      (fun () ->
		 List.iter (fun (c,name) ->
			      ok ~msg:name c @@ Color.parse name;
			      ok ~msg:name c @@ Color.parse @@ String.uppercase name;
			      ok ~msg:name c @@ Color.parse @@ String.capitalize name)
		   [red,"red";
		    lime,"lime";
		    blue,"blue";
	            white,"white";
		    maroon,"maroon";
		    green,"green";
		    navy,"navy";
		    silver,"silver";
		    yellow,"yellow";
		    aqua,"aqua";
		    fuchsia,"fuchsia";
		    gray,"gray";
		    olive,"olive";
		    teal,"teal";
		    purple,"purple";
		    black,"black"]);
	    "of_int" >::
	      (fun () ->
		 ok {red=0x12; green=0x34; blue=0x56; alpha=1.0} @@
		   Color.of_int 0x123456);
	    "to_int" >::
	      (fun () ->
		 ok 0x123456 @@
		   Color.to_int {red=0x12; green=0x34; blue=0x56; alpha=1.0})
	  ]) +> run_test_tt
